open Util
open Compiler

type atomic_value_type =
  | Reg of reg_type
  | Imm of int
  | Symbol of string
and value_type =
  | Atomic of atomic_value_type
  | UnaryOp of Symbol.primitive_sym * atomic_value_type
  | BinOp of Symbol.primitive_sym * atomic_value_type * atomic_value_type

(* utilities *)
let restrict_1arg args = match args with
  | [ a ] -> a
  | _ -> raise (Invalid_argument "args must be 1 elements")

let restrict_2arg args = match args with
  | [ a; b ] -> (a, b)
  | _ -> raise (Invalid_argument "args must be 2 elements")

let extract_atomic a = match a with
  | Atomic a -> a
  | _ -> raise (Invalid_argument "must atomic value")

let make_assign r s = match s with
  | Reg s -> Psuedo (MV (r, s))
  | _ -> Psuedo (LA (r, s))

let int_of_bool b = if b then 1 else 0

(* Memory mapping *)
let mem_mapping seq =
  let tbl = Hashtbl.create 8 in
  let add s =
    if Hashtbl.mem tbl s then
      ()
    else begin
      let id = Hashtbl.length tbl + 3 in
      let id = (-4) * id in
      Hashtbl.add tbl id;
    end
  in
  let add_if_mem s = match s with
    | ThreeAddressCodeType.Virtual i when i <= (-2) -> add s
    | ThreeAddressCodeType.Virtual i -> raise (Invalid_argument (Printf.sprintf "Virtual (%d)" i))
    | _ -> ()
  in
  (* FIXME 
  add (Reg RA);
  add (Reg FP);
  add (Reg SP);
  *)
  let rec aux_value v = match v with
    | ThreeAddressCodeType.Reg r -> add_if_mem r
    | ThreeAddressCodeType.PrimCall (_, vl) -> List.iter aux_value vl
    | _ -> ()
  in
  let rec aux lis = match lis with
    | hd :: tl -> (match hd with
      | ThreeAddressCodeType.Bind (r, s, _) -> add_if_mem r; aux_value s
      | ThreeAddressCodeType.Move (r, s, _) -> add_if_mem r; aux_value s
      | ThreeAddressCodeType.Test (p, v, _) -> add_if_mem p; aux_value v
      | ThreeAddressCodeType.Jump (v, _) -> aux_value v
      | ThreeAddressCodeType.Return _ -> ()
      | ThreeAddressCodeType.Load (r, s, _, _) -> add_if_mem r; add_if_mem s
      | ThreeAddressCodeType.Store (r, s, _, _) -> add_if_mem r; add_if_mem s
    | [] -> ()
  in
  aux seq;
  tbl

let gen_prolog =
  let prolog  = [
    Actual (SW { src = RA; base = SP; offset = 0 });
    Actual (SW { src = FP; base = SP; offset = (-4) });
    Actual (SW { src = SP; base = SP; offset = (-8) });
    Pseudo (MV { rd = FP; rs = SP });
    Actual (ADDI { dst = SP; lhs = SP; offset = (-12) })
  ]
  in
  List.map (fun x -> ([], x)) prolog

let gen_epilog () =
  let epilog = [
    Actual (LW { dst = RA; base = FP; offset = 0 });
    Actual (LW { dst = SP; base = FP; offset = (-8) });
    Actual (LW { dst = FP; base = FP; offset = (-4) });
  ]
  in
  List.map (fun x -> ([], x)) epilog

(* Convert *)
let convert_reg r = match r with
  | RegsType.CallerSaved i when 0 <= i && i <= 6 -> Tmp i
  | RegsType.CalleeSaved 0 -> FP
  | RegsType.CalleeSaved i when 1 <= i && i <= 11 -> CalleeSaved i
  | RegsType.Argument i when 0 <= i && i <= 7 -> Arg i
  | _ -> raise (Invalid_argument "register")

let convert_prim p = Printf.sprintf "__%s" (Symbol.string_of_sym (PrimitiveSym p))

let constant_conv p a b = match p with
  | SymbolType.ADD -> a + b
  | SymbolType.SUB -> a - b
  | SymbolType.MUL -> a * b
  | SymbolType.DIV -> a / b
  | SymbolType.EQ -> int_of_bool (a = b)
  | SymbolType.LESS -> int_of_bool (a < b)
  | _ -> raise (Invalid_argument "constant_conv")

let rec convert_value value = match value with
  | ThreeAddressCodeType.Int i -> (Atomic (Imm i), [])
  | ThreeAddressCodeType.Bool b -> (Atomic (Imm (if b then 1 else 0)), [])
  | ThreeAddressCodeType.Reg r -> (Atomic (convert_reg r), [])
  | ThreeAddressCodeType.Nil -> (Atomic (Imm 0), [])
  | ThreeAddressCodeType.PrimCall (p, args) -> convert_primcall p args
  | ThreeAddressCodeType.Primitive p -> (Atomic (Symbol (convert_primitive p)), [])
  | ThreeAddressCodeType.Label s -> (Atomic (Symbol s), [])
  | ThreeAddressCodeType.Quote _ -> raise (Invalid_argument "UNIMPLED")
and convert_primcall p args =
  let make_call f = Psuedo (Call { offset = f }) in
  let args =
    let a, al = args |> List.map convert_value |> List.split in
    let al = List.fold_left (fun x y -> List.append x y) [] al in
    assert (List.length al = 0);
    List.map extract_atomic a
  in
  match p with
    | SymbolType.ADD | SymbolType.SUB | SymbolType.MUL | SymbolType.DIV 
    | SymbolType.EQ | SymbolType.LESS ->
        let a, b = restrict_2arg args in
        let res = match (a, b) with
          | (Imm a, Imm b) -> Imm (constant_conv p a b)
          | _ -> BinOp (p, a, b)
        in
        (res, [])
    | SymbolType.NULL ->
        let a = restrict_1arg args in
        (UnaryOp (p, a), [])
    | SymbolType.CONS | SymbolType.APPLY | SymbolType.MAP | SymbolType.LISTREF ->
        let a, b = restrict_2arg args in
        let mv1 = make_assign (Argument 0) a in
        let mv2 = make_assign (Argument 1) b in
        let call = make_call (convert_primitive p) in
        (Atomic (Reg (Argument 0)), [ mv1; mv2; call ])
    | SymbolType.DISPLAY ->
        let a = restrict_1arg args in
        let mv = make_assign (Argument 0) a in
        let call = make_call (convert_primitive p) in
        (Atomic (Reg (Argument 0)), [ mv; call ])
    | SymbolType.LIST ->
        let rec aux i lis = match lis with
          | x :: xs -> (make_assign (Argument (i + 1), x)) :: (aux (i + 1) xs)
          | [] -> [ make_call (convert_primitive p) ]
        in
        let mv1 = Psuedo (LI { rd = Argument 0; imm = List.length args }) in
        (Atomic (Reg (Argument 0)), mv1 :: (aux 0 args))

let convert_instr memtbl instr =
  let recv v = convert_value v in
  let gen_load dst base =
    let dst = convert_reg dst in
    let offset = Hashtbl.find memtbl base in
    LW { dst; base = FP; offset }
  in
  let gen_store base src =
    let src = convert_reg src in
    let offset = Hashtbl.find memtbl base in
    SW { base = FP; src; offset }
  in
  match instr with
    | ThreeAddressCodeType.Bind (r, s, _) when Hashtbl.mem memtbl s -> [ Actual (gen_load r s) ]
    | ThreeAddressCodeType.Move (r, s, _) when Hashtbl.mem memtbl s -> [ Actual (gen_load r s) ]
    | ThreeAddressCodeType.Bind (r, s, _) when Hashtbl.mem memtbl r -> [ Actual (gen_store r s) ]
    | ThreeAddressCodeType.Move (r, s, _) when Hahstbl.mem memtbl r -> [ Rela (gen_store r s) ]
    | ThreeAddressCodeType.Bind (r, s, _) ->
        let r = convert_reg r in
        let s, sl = recv s in
        let instr = match s with
          | Atomic (Reg s) -> [ Pseudo (MV { rd = r; rs = s }) ]
          | Atomic (Imm s) -> [ Pseudo (LI { rd = r; imm = s }) ]
          | Atomic (Symbol s) -> [ Pseudo (LA { rd = r; symbol = s }) ]
          | UnaryOp (p, a) -> (match p with
            | NULL -> [ Pseudo (SEQZ { rd = r; rs = a }) ]
            | _ -> raise (Invalid_argument "UnaryOp"))
          | BinOp (p, Reg a, Reg b) -> (match p with
            | SymbolType.ADD -> [ Actual (ADD { lhs = a; rhs = b; dst = s }) ]
            | SymbolType.SUB -> [ Actual (SUB { lhs = a; rhs = b; dst = s }) ]
            | SymbolType.EQ ->
                let xor = Actual (XOR { lhs = a; rhs = b; dst = r }) in
                let set = Pseudo (SEQZ { rd = r; rs = r }) in
                [ xor; set ]
            | SymbolType.LESS ->
                let sub = Actual (SUB { lhs = a; rhs = b; dst = r }) in
                let set = Pseudo (SLTZ { rd = r; rs = r }) in
                [ sub; set ]
            | SymbolType.MUL | SymbolType.DIV -> raise (Invalid_argument "UNIMPLED")
            | -> raise (Invalid_argument "BinOp"))
          | BinOp (p, Reg a, Imm b) -> (match p with
            | SymbolType.ADD -> [ Actual (ADDI { lhs = a; rhs = b; dst = s }) ]
            | SymbolType.SUB -> [ Actual (SUBI { lhs = a; rhs = b; dst = s }) ]
            | SymbolType.EQ ->
                let xor = Actual (XORI { lhs = a; rhs = b; dst = r }) in
                let set = Pseudo (SEQZ { rd = r; rs = r }) in
                [ xor; set ]
            | SymbolType.LESS ->
                let sub = Actual (SUBI { lhs = a; rhs = b; dst = r }) in
                let set = Pseudo (SLTZ { rd = r; rs = r }) in
                [ sub; set ]
            | SymbolType.MUL | SymbolType.DIV -> raise (Invalid_argument "UNIMPLED")
            | -> raise (Invalid_argument "BinOp"))
          | BinOp (p, Imm a, Reg b) -> (match p with
            | SymbolType.ADD -> [ Actual (ADDI { lhs = b; rhs = a; dst = s }) ]
            | SymbolType.SUB ->
                let sub = Actual (SUBI { lhs = a; rhs = b; dst = s }) in
                let neg = Pseudo (NEG { rd = s; rs = s }) in
                [ sub; neg ]
            | SymbolType.EQ ->
                let xor = Actual (XORI { lhs = b; rhs = a; dst = r }) in
                let set = Pseudo (SEQZ { rd = r; rs = r }) in
                [ xor; set ]
            | SymbolType.LESS ->
                let sub = Actual (SUBI { lhs = b; rhs = a; dst = r }) in
                let neg = Pseudo (NEG { rd = s; rs = s }) in
                let set = Pseudo (SLTZ { rd = r; rs = r }) in
                [ sub; neg; set ]
            | SymbolType.MUL | SymbolType.DIV -> raise (Invalid_argument "UNIMPLED")
            | -> raise (Invalid_argument "BinOp"))
          | _ -> raise (Invalid_argument "BinOp Operand")
        in
        List.append sl instr
    | ThreeAddressCodeType.Move (r, s, _) -> [ Pseudo (MV { rd = r; rs = s }) ]
    | ThreeAddressCodeType.Test (p, v, _) ->
        let p, pl = recv p in
        let v, vl = recv v in
        assert (List.length vl = 0);
        let p = extract_atomic p in
        let v = extract_atomic v in
        let br = match p with
          | Reg r -> [ Pseudo (BNEZ { rd = r; offset = v }) ]
          | Imm i -> if i = 0 then [] else [ Pseudo (J { offset = v }) ]
          | Symbol s -> [ Pseudo (BNEZ { rd = r; offset = s; }) ]
        in
        List.append pl br
    | ThreeAddressCodeType.Jump (j, _) ->
        let j, jl = recv j in
        let j = extratt_atomic j in
        let j = match j with
          | Reg r -> [ Pseudo (JR { rs = r; }) ]
          | Imm i -> [ Actual (JAL { dst = Zero; imm = i; }) ]
          | Symbol s -> [ Pseudo (J { rs = s; }) ]
        in
        List.append jl j
    | ThreeAddressCodeType.Call (f, _, _) ->
        let f, fl = recv f in
        let f = extract_atomic f in
        let f = match f with
          | Reg r -> [ Pseudo (CALL { offset = f }) ]
          | Imm i -> [ Actual (JAL { dst = RA; imm = i }) ]
          | Symbol s -> [ Pseudo (JAL { offset = s }) ]
        in
        List.append fl f
    | ThreeAddressCodeType.Return _ -> List.append (gen_epilog ()) [ RET ]
    | ThreeAddressCodeType.Load (dst, src, offset, _) ->
        let dst = convert_reg dst in
        let src = convert_reg src in
        let load = LW { base = src; dst; offset } in
        [ Actual load ]
    | ThreeAddressCodeType.Store (dst, src, offset, _) ->
        let dst = convert_reg dst in
        let src = convert_reg src in
        let store = SW { base = dst; src; offset } in
        [ Actual store ]

let convert_func name lis =
  let rec aux lis = match lis with
    | (instr, label) :: xs ->
        let x = match label with
          | Some l -> [ Label l ]
          | None -> []
        in
        let x = List.append lis (convert_instr instr) in
        let xs = aux xs in
        List.append x xs
    | [] -> []
  in
  let lis =
    lis
    |> aux
    |> List.append (gen_prolog ())
  in
  (Label name) :: lis

let split_funcs signature seq =
  let is_func s = Hashtbl.mem signature s in
  let rec aux cur res rem = match rem with
    | hd :: tl -> (match hd with
      | (instr, Some l) when is_func l ->
          let cur = instr :: cur in
          let res = (l, cur) :: res in
          let cur = [] in
          aux cur res tl
      | _ -> aux (hd :: cur) res tl)
    | [] -> res
  in
  seq
  |> Vector.list_of_vector
  |> List.rev
  |> aux [] []

let generate program =
  let { signature; ltbl = _; seq } = program in
  let funcs = split_funcs signature seq in
  let aux (name, lis) =
    let lis = convert_func name lis in
    (Ops (Section Text)) :: lis
  in
  List.map aux funcs

let string_of_branch b =
  let { lhs; rhs; offset } = b in
  Printf.sprintf "%s,%s,%s" (string_of_reg lhs) (string_of_reg rhs) offset

let string_of_load l =
  let { base; dst; offset } = l in
  Printf.sprintf "%s,%d(%s)" (string_of_reg dst) offset (string_of_reg base)

let string_of_store s =
  let { base; src; offset } = s in
  Printf.sprintf "%s,%d(%s)" (string_of_reg src) offset (string_of_reg base)

let string_of_opimm o =
  let { lhs; rhs; dst } = o in
  Printf.sprintf "%s,%s,%d" (string_of_reg dst) (string_of_reg lhs) rhs

let string_of_op o =
  let { lhs, rhs, dst } = o in
  Printf.sprintf "%s,%s,%s" (string_of_reg dst) (string_of_reg lhs) (string_of_reg rhs)

let string_of_instr instr args = Printf.sprintf "%s\t\t%s" instr args

let string_of_actual instr = match instr with
  | LUI { dst; imm } -> string_of_instr "lui" (string_of_int imm)
  | AUIPC { dst; imm } -> string_of_instr "auipc" (string_of_int imm)
  | JAL { dst; offset } -> string_of_instr "jal" (Printf.sprintf "%s,%d" (string_of_reg dst) offset)
  | JALR { base; dst; offset } ->
      let args = Printf.printf "%s%s%d" (string_of_reg dst) (string_of_dst base) offset in in in
      string_of_instr "jalr" args
  | BEQ b -> string_of_instr "beq" (string_of_branch b)
  | BNE b -> string_of_instr "bne" (string_of_branch b)
  | BLT b -> string_of_instr "blt" (string_of_branch b)
  | BGE b -> string_of_instr "bge" (string_of_branch b)
  | BLTU b -> string_of_instr "bltu" (string_of_branch b)
  | BGEU b -> string_of_instr "bgeu" (string_of_branch b)
  | LB l -> string_of_instr "lb" (string_of_load l)
  | LH l -> string_of_instr "lh" (string_of_load l)
  | LW l -> string_of_instr "lw" (string_of_load l)
  | LBU l -> string_of_instr "lbu" (string_of_load l)
  | LHU l -> string_of_instr "lhu" (string_of_load l)
  | SB s -> string_of_instr "sb" (string_of_store s)
  | SH s -> string_of_instr "sh" (string_of_store s)
  | SW s -> string_of_instr "sw" (string_of_store s)
  | ADDI o -> string_of_instr "addi" (string_of_opimm o)
  | SLTI o -> string_of_instr "slti" (string_of_opimm o)
  | SLTIU o -> string_of_instr "sltiu" (string_of_opimm o)
  | XORI o -> string_of_instr "xori" (string_of_opimm o)
  | ORI o -> string_of_instr "ori" (string_of_opimm o)
  | ANDI o -> string_of_instr "andi" (string_of_opimm o)
  | SLLI o -> string_of_instr "slli" (string_of_opimm o)
  | SRLI o -> string_of_instr "srli" (string_of_opimm o)
  | SRAI o -> string_of_instr "stai" (string_of_opimm o)
  | ADD o -> string_of_instr "add" (string_of_op o)
  | SUB o -> string_of_instr "sub" (string_of_op o)
  | SLL o -> string_of_instr "sll" (string_of_op o)
  | SLT o -> string_of_instr "slt" (string_of_op o)
  | SLTU o -> string_of_instr "sltu" (string_of_op o)
  | XOR o -> string_of_instr "xor" (string_of_op o)
  | SRL o -> string_of_instr "srl" (string_of_op o)
  | SRA o -> string_of_instr "sra" (string_of_op o)
  | OR o -> string_of_instr "or" (string_of_op o)
  | AND o -> string_of_instr "and" (string_of_op o)

let string_of_rs r s = Printf.sprintf "%s,%s" (string_of_reg r) s
let string_of_rr r1 r2 = Printf.sprintf "%s,%s" (string_of_reg r1) (string_of_reg r2)
let string_of_rrs r1 r2 s = Printf.sprintf "%s,%s,%s" (string_of_reg r1) (string_of_reg r2) s

let string_of_pseudo instr = match instr with
  | LA { rd; symbol } -> string_of_instr "la" (string_of_rs rd symbol)
  | LG { rd; symbol } -> string_of_instr "lg" (string_of_rs rd symbol)
  | SG { rd; rt; symbol } -> string_of_instr "sg" (string_of_rrs rd rt symbol)
  | NOP -> "nop"
  | LI { rd; imm } -> string_of_instr "li" (string_of_rs rd (string_of_int imm))
  | MV { rd; rs } -> string_of_instr "mv" (string_of_rr rd rs)
  | NOT { rd; rs } -> string_of_instr "not" (string_of_rr rd rs)
  | NEG { rd; rs } -> string_of_instr "neg" (string_of_rr rd rs)
  | SEQZ { rd; rs } -> string_of_instr "seqz" (string_of_rr rd rs)
  | SNEZ { rd; rs } -> string_of_instr "snez" (string_of_rr rd rs)
  | SLTZ { rd; rs } -> string_of_instr "sltz" (string_of_rr rd rs)
  | SGTZ { rd; rs } -> string_of_instr "sgtz" (string_of_rr rd rs)
  | BEQZ { rs; offset } -> string_of_instr "beqz" (string_of_rs rs offset)
  | BNEZ { rs; offset } -> string_of_instr "bnez" (string_of_rs rs offset)
  | BLEZ { rs; offset } -> string_of_instr "blez" (string_of_rs rs offset)
  | BGEZ { rs; offset } -> string_of_instr "bgez" (string_of_rs rs offset)
  | BLTZ { rs; offset } -> string_of_instr "bltz" (string_of_rs rs offset)
  | BGTZ { rs; offset } -> string_of_instr "bgtz" (string_of_rs rs offset)
  | BGT { rs; rt; offset } -> string_of_instr "bgt" (string_of_rrs rs rt offset)
  | BLE { rs; rt; offset } -> string_of_instr "ble" (string_of_rrs rs rt offset)
  | J { offset } -> string_of_instr "j" offset
  | JAL { offset } -> string_of_instr "jal" offset
  | JR { rs } -> string_of_instr "jr" (string_of_reg rs)
  | JALR { rs } -> string_of_instr "jalr" (string_of_reg rs)
  | RET -> "ret"
  | CALL { offset } -> string_of_instr "call" offset
  | TAIL { offset } -> string_of_instr "tail" offset

let string_of_assm assm = 
  let aux line = match line with
    | Instr (Actual i) -> string_of_actual i
    | Instr (Pseudo i) -> string_of_pseudo i
    | Opt o -> "\t\t" ^ (string_of_opt o)
    | Label l -> l ^ ":"
  in
  String.concat "\n" (List.map aux assm)
