open Util
open Compiler
open Rv32i
open RiscvAssm
open PseudoInstr
open ActualInstr
open Builtin

type psym_type = SymbolType.primitive_sym

exception Unimpled

let wsize = 8  (* !!!! FIXME !!!! *)

type atomic_value_type =
  | Reg of reg_type
  | Int of int
  | Sym of string
and value_type =
  | Atomic of atomic_value_type
  | UnaryOp of psym_type * atomic_value_type
  | BinOp of psym_type * atomic_value_type * atomic_value_type

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
  | Reg s -> Pseudo (MV { rd = r; rs = s })
  | Int s -> Pseudo (LI { rd = r; imm = s; })
  | Sym s -> Pseudo (LA { rd = r; symbol = s })

let int_of_bool b = if b then 1 else 0

(* Memory mapping *)
let mem_mapping seq =
  let tbl = Hashtbl.create 8 in
  let add s =
    if Hashtbl.mem tbl s then
      ()
    else begin
      let id = Hashtbl.length tbl + 3 in
      let id = (-wsize) * id in
      Hashtbl.add tbl s id;
    end
  in
  let add_if_mem s = match s with
    | RegsType.Virtual i when i <= (-2) -> add s
    | RegsType.Virtual i -> raise (Invalid_argument (Printf.sprintf "Virtual (%d)" i))
    | _ -> ()
  in
  let rec aux_value v = match v with
    | ThreeAddressCodeType.Reg r -> add_if_mem r
    | ThreeAddressCodeType.PrimCall (_, vl) -> List.iter aux_value vl
    | _ -> ()
  in
  let rec aux lis = match lis with
    | (hd, _) :: tl -> 
        let () = match hd with
          | ThreeAddressCodeType.Bind (r, s, _) -> add_if_mem r; aux_value s
          | ThreeAddressCodeType.Move (r, s, _) -> add_if_mem r; add_if_mem s
          | ThreeAddressCodeType.Test (p, v, _) -> aux_value p; aux_value v
          | ThreeAddressCodeType.Jump (v, _) -> aux_value v
          | ThreeAddressCodeType.Call (f, _, _) -> aux_value f
          | ThreeAddressCodeType.Return _ -> ()
          | ThreeAddressCodeType.Load (r, s, _, _) -> add_if_mem r; add_if_mem s
          | ThreeAddressCodeType.Store (r, s, _, _) -> add_if_mem r; add_if_mem s
        in
        aux tl
    | [] -> ()
  in
  aux seq;
  tbl

let gen_prologue var_num = [
  Actual (SW { src = RA; base = SP; offset =  Int 0 });
  Actual (SW { src = FP; base = SP; offset =  Int (-1 * wsize) });
  Actual (SW { src = SP; base = SP; offset =  Int (-1 * wsize * 2) });
  Pseudo (MV { rd = FP; rs = SP });
  Actual (ADDI { dst = SP; lhs = SP; rhs = Int (-1 * wsize * (var_num + 3)) })
]

let epilogue = [
  Actual (LW { dst = RA; base = FP; offset = Int 0 });
  Actual (LW { dst = SP; base = FP; offset = Int (-1 * wsize * 2) });
  Actual (LW { dst = FP; base = FP; offset = Int (-1 * wsize * 1) });
]

(* Convert *)
let convert_reg r = match r with
  | RegsType.CallerSaved i when 0 <= i && i <= 6 -> Tmp i
  | RegsType.CalleeSaved i when 0 <= i && i <= 10 -> CalleeSaved (i + 1)
  | RegsType.Argument i when 0 <= i && i <= 7 -> Arg i
  | _ -> print_endline (Regs.string_of_reg r); raise (Invalid_argument "register")

let constant_conv p a b = match p with
  | SymbolType.ADD -> a + b
  | SymbolType.SUB -> a - b
  | SymbolType.MUL -> a * b
  | SymbolType.DIV -> a / b
  | SymbolType.EQ -> int_of_bool (a = b)
  | SymbolType.LESS -> int_of_bool (a < b)
  | _ -> raise (Invalid_argument "constant_conv")

let rec convert_value value = match value with
  | ThreeAddressCodeType.Int i -> (Atomic (Int i), [])
  | ThreeAddressCodeType.Bool b -> (Atomic (Int (if b then 1 else 0)), [])
  | ThreeAddressCodeType.Reg r -> (Atomic (Reg (convert_reg r)), [])
  | ThreeAddressCodeType.Nil -> (Atomic (Int 0), [])
  | ThreeAddressCodeType.PrimCall (p, args) -> convert_primcall p args
  | ThreeAddressCodeType.Primitive p -> (Atomic (Sym (closure_of_prim p)), [])
  | ThreeAddressCodeType.Label s -> (Atomic (Sym s), [])
  | ThreeAddressCodeType.Quote _ -> raise (Invalid_argument "UNIMPLED")
and convert_primcall p args =
  let make_call f = Pseudo (CALL { offset = f }) in
  let args =
    let a, al = args |> List.map convert_value |> List.split in
    let al = List.fold_left (fun x y -> List.append x y) [] al in
    assert (List.length al = 0);
    List.map extract_atomic a
  in
  (* FIXME : convert to clo style *) 
  match p with
    | SymbolType.ADD | SymbolType.SUB | SymbolType.MUL | SymbolType.DIV 
    | SymbolType.EQ | SymbolType.LESS ->
        let a, b = restrict_2arg args in
        let res = match (a, b) with
          | (Int a, Int b) -> Atomic (Int (constant_conv p a b))
          | _ -> BinOp (p, a, b)
        in
        (res, [])
    | SymbolType.NULL ->
        let a = restrict_1arg args in
        (UnaryOp (p, a), [])
    | SymbolType.CONS | SymbolType.APPLY | SymbolType.MAP | SymbolType.LISTREF ->
        let a, b = restrict_2arg args in
        let mv1 = make_assign (Arg 0) a in
        let mv2 = make_assign (Arg 1) b in
        let call = make_call (closure_of_prim p) in
        (Atomic (Reg (Arg 0)), [ mv1; mv2; call ])
    | SymbolType.CAR | SymbolType.CDR | SymbolType.DISPLAY ->
        let a = restrict_1arg args in
        let mv = make_assign (Arg 0) a in
        let call = make_call (closure_of_prim p) in
        (Atomic (Reg (Arg 0)), [ mv; call ])
    | SymbolType.LIST ->
        let rec aux i lis = match lis with
          | x :: xs ->
              let x = make_assign (Arg (i + 1)) x in
              let xs = aux (i + 1) xs in
              x :: xs
          | [] -> [ make_call (closure_of_prim p) ]
        in
        let mv1 = Pseudo (LI { rd = Arg 0; imm = List.length args }) in
        (Atomic (Reg (Arg 0)), mv1 :: (aux 0 args))

let convert_instr memtbl instr =
  let recv v = convert_value v in
  let gen_load dst base =
    let dst = convert_reg dst in
    let offset = Rv32i.Int (Hashtbl.find memtbl base) in
    LW { dst; base = FP; offset }
  in
  let gen_store base src =
    let src = convert_reg src in
    let offset = Rv32i.Int (Hashtbl.find memtbl base) in
    SW { base = FP; src; offset }
  in
  match instr with
    | ThreeAddressCodeType.Move (r, s, _) when Hashtbl.mem memtbl s -> [ Actual (gen_load r s) ]
    | ThreeAddressCodeType.Move (r, s, _) when Hashtbl.mem memtbl r -> [ Actual (gen_store r s) ]
    | ThreeAddressCodeType.Bind (r, s, _) ->
        let r = convert_reg r in
        let s, sl = recv s in
        let instr = match s with
          | Atomic (Reg s) -> [ Pseudo (MV { rd = r; rs = s }) ]
          | Atomic (Int i) -> [ Pseudo (LI { rd = r; imm = i }) ]
          | Atomic (Sym s) -> [ Pseudo (LA { rd = r; symbol = s }) ]
          | UnaryOp (NULL, Reg s) -> [ Pseudo (SEQZ { rd = r; rs = s }) ]
          | UnaryOp (NULL, Int s) -> 
              [ Pseudo (LI { rd = r; imm = (if s = 0 then 1 else 0) }) ]
          | UnaryOp (NULL, ((Sym _) as s)) -> 
              [ make_assign r s; 
                Pseudo (SEQZ { rd = r; rs = r; }) ]
          | UnaryOp _ -> raise (Invalid_argument "unexpected unary op")
          | BinOp (p, Reg a, Reg b) -> (match p with
            | SymbolType.ADD -> [ Actual (ADD { lhs = a; rhs = b; dst = r }) ]
            | SymbolType.SUB -> [ Actual (SUB { lhs = a; rhs = b; dst = r }) ]
            | SymbolType.EQ ->
                let xor = Actual (XOR { lhs = a; rhs = b; dst = r }) in
                let set = Pseudo (SEQZ { rd = r; rs = r }) in
                [ xor; set ]
            | SymbolType.LESS ->
                let sub = Actual (SUB { lhs = a; rhs = b; dst = r }) in
                let set = Pseudo (SLTZ { rd = r; rs = r }) in
                [ sub; set ]
            | SymbolType.MUL | SymbolType.DIV -> raise Unimpled
            | _ -> raise (Invalid_argument "BinOp"))
          | BinOp (p, Reg a, Int b) -> (match p with
            | SymbolType.ADD -> [ Actual (ADDI { lhs = a; rhs = Int b; dst = r }) ]
            | SymbolType.SUB -> [ Actual (ADDI { lhs = a; rhs = Int (-b); dst = r }) ]
            | SymbolType.EQ ->
                let xor = Actual (XORI { lhs = a; rhs = Int b; dst = r }) in
                let set = Pseudo (SEQZ { rd = r; rs = r }) in
                [ xor; set ]
            | SymbolType.LESS ->
                let sub = Actual (ADDI { lhs = a; rhs = Int (-b); dst = r }) in
                let set = Pseudo (SLTZ { rd = r; rs = r }) in
                [ sub; set ]
            | SymbolType.MUL | SymbolType.DIV -> raise (Invalid_argument "UNIMPLED")
            | _ -> raise (Invalid_argument "BinOp"))
          | BinOp (p, Int a, Reg b) -> (match p with
            | SymbolType.ADD -> [ Actual (ADDI { lhs = b; rhs = Int a; dst = r }) ]
            | SymbolType.SUB ->
                let sub = Actual (ADDI { lhs = b; rhs = Int (-a); dst = r }) in
                let neg = Pseudo (NEG { rd = r; rs = r }) in
                [ sub; neg ]
            | SymbolType.EQ ->
                let xor = Actual (XORI { lhs = b; rhs = Int a; dst = r }) in
                let set = Pseudo (SEQZ { rd = r; rs = r }) in
                [ xor; set ]
            | SymbolType.LESS ->
                let sub = Actual (ADDI { lhs = b; rhs = Int a; dst = r }) in
                let neg = Pseudo (NEG { rd = r; rs = r }) in
                let set = Pseudo (SLTZ { rd = r; rs = r }) in
                [ sub; neg; set ]
            | SymbolType.MUL | SymbolType.DIV -> raise (Invalid_argument "UNIMPLED")
            | _ -> raise (Invalid_argument "BinOp"))
          | _ -> raise (Invalid_argument "BinOp Operand")
        in
        List.append sl instr
    | ThreeAddressCodeType.Move (r, s, _) -> [ Pseudo (MV { rd = convert_reg r; rs = convert_reg s }) ]
    | ThreeAddressCodeType.Test (p, v, _) ->
        let p, pl = recv p in
        let v, vl = recv v in
        assert (List.length vl = 0);
        let p = extract_atomic p in
        let v = match extract_atomic v with
          | Sym v -> v
          | _ -> raise Unimpled
        in
        let br = match p with
          | Reg p -> [ Pseudo (BNEZ { rs = p; offset = v }) ]
          | Int p -> if p = 0 then [] else [ Pseudo (J { offset = v }) ]
          | Sym _ -> raise Unimpled
        in
        List.append pl br
    | ThreeAddressCodeType.Jump (j, _) ->
        let j, jl = recv j in
        let j = extract_atomic j in
        let j = match j with
          | Reg j -> [ Pseudo (JR { rs = j; }) ]
          | Sym j -> [ Pseudo (J { offset = j }) ]
          | Int _ -> raise Unimpled
        in
        List.append jl j
    | ThreeAddressCodeType.Call (f, _, _) ->
        let f, fl = recv f in
        let f = extract_atomic f in
        let f = match f with
          | Reg f ->
              (* FIXME *)
              let tmp = if f = Tmp 0 then Tmp 1 else Tmp 0 in
              [ Pseudo (MV { rd = Arg 7; rs = Arg 6 });
                Pseudo (MV { rd = Arg 6; rs = Arg 5 });
                Pseudo (MV { rd = Arg 5; rs = Arg 4 });
                Pseudo (MV { rd = Arg 4; rs = Arg 3 });
                Pseudo (MV { rd = Arg 3; rs = Arg 2 });
                Pseudo (MV { rd = Arg 2; rs = Arg 1 });
                Pseudo (MV { rd = Arg 1; rs = Arg 0 });
                Actual (LW { dst = tmp; base = f; offset = Int 0 });  (* car *)
                Actual (LW { dst = Arg 0; base = f; offset = Int wsize });  (* closure *)
                Pseudo (JALR { rs = tmp }); ]
          | Sym f when f = "allocate" -> [ Pseudo (CALL { offset = f  }) ]
          | Sym f ->
              [ Pseudo (MV { rd = Arg 7; rs = Arg 6 });
                Pseudo (MV { rd = Arg 6; rs = Arg 5 });
                Pseudo (MV { rd = Arg 5; rs = Arg 4 });
                Pseudo (MV { rd = Arg 4; rs = Arg 3 });
                Pseudo (MV { rd = Arg 3; rs = Arg 2 });
                Pseudo (MV { rd = Arg 2; rs = Arg 1 });
                Pseudo (MV { rd = Arg 1; rs = Arg 0 });
                Pseudo (LA { rd = Tmp 0; symbol = f });
                Actual (LW { dst = Arg 0; base = Tmp 0; offset = Int wsize });
                Actual (LW { dst = Tmp 0; base = Tmp 0; offset = Int 0 });
                Pseudo (JALR { rs = Tmp 0 }); ]
          | Int _ -> raise Unimpled
        in
        List.append fl f
    | ThreeAddressCodeType.Return _ -> List.append epilogue [ Pseudo RET ]
    | ThreeAddressCodeType.Load (dst, src, offset, _) ->
        let dst = convert_reg dst in
        let src = convert_reg src in
        let load = LW { base = src; dst; offset = Int (wsize * offset) } in
        [ Actual load ]
    | ThreeAddressCodeType.Store (dst, src, offset, _) ->
        let dst = convert_reg dst in
        let src = convert_reg src in
        let store = SW { base = dst; src; offset = Int (wsize * offset) } in
        [ Actual store ]

let convert_func lis =
  let memtbl = mem_mapping lis in
  let bind i = Instr i in
  let rec aux lis = match lis with
    | (instr, label) :: xs ->
        let x = match label with
          | Some l -> [ Label l ]
          | None -> []
        in
        let x =
          instr
          |> convert_instr memtbl
          |> List.map bind
          |> List.append x
        in
        let xs = aux xs in
        List.append x xs
    | [] -> []
  in
  let lis = aux lis in
  let pr = List.map bind (gen_prologue (Hashtbl.length memtbl)) in
  match lis with
    | ((Label _) as hd) :: tl -> hd :: (List.append pr tl)
    | _ -> raise (Invalid_argument "code")

let split_funcs signature seq =
  let is_func s = Hashtbl.mem signature s in
  let rec aux cur res rem = match rem with
    | hd :: tl -> (match hd with
      | (_, Some l) when is_func l ->
          let cur = hd :: cur in
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
  let { signature; ltbl = _; seq }: ThreeAddressCodeType.t = program in
  let funcs = split_funcs signature seq in
  let aux (name, lis) =
    let lis = convert_func lis in
    if name = "entry" then
      let hd = [
        (Ops (Section Text));
        (Ops (Globl (Symbol name)));
        (Label "entry");
        (Instr (Pseudo (LI { rd = Arg 0; imm = 65536 })));
        (Instr (Pseudo (CALL { offset = "malloc" })));
        (Instr (Pseudo (SG { rd = Arg 0; rt = Tmp 0; symbol = "__free_list" })))
      ]
      in
      List.append hd (List.tl lis)
    else
      (Ops (Section Text)) :: (Ops (Globl (Symbol name))) :: lis
  in
  funcs |> List.map aux |> List.flatten

let string_of_reg r = match r with
  | ZERO -> "x0"
  | RA -> "ra"
  | SP -> "sp"
  | FP -> "s0"
  | Tmp i -> Printf.sprintf "t%d" i
  | Arg i -> Printf.sprintf "a%d" i
  | CalleeSaved i -> Printf.sprintf "s%d" i

let string_of_imm (c: imm_type) =
  let aux s i =
    if i = 0 then
      s
    else if 0 < i then
      Printf.sprintf "%s+%d" s i
    else
      Printf.sprintf "%s+%db" s (-i)
  in
  let string_of_rel l s = Printf.sprintf "%%%s(%s)" l s in
  match c with
    | Int i -> string_of_int i
    | Hi (s, i) -> string_of_rel "hi" (aux s i)
    | Raw s -> s
    | Lo (s, i) -> string_of_rel "lo" (aux s i)
    | PcrelHi (s, i) -> string_of_rel "pcrel_hi" (aux s i)
    | PcrelLo s -> string_of_rel "pcrel_lo" s

let string_of_branch b =
  let { lhs; rhs; offset } = b in
  Printf.sprintf "%s,%s,%s" (string_of_reg lhs) (string_of_reg rhs) (string_of_imm offset)

let string_of_load l =
  let { base; dst; offset } = l in
  Printf.sprintf "%s,%s(%s)" (string_of_reg dst) (string_of_imm offset) (string_of_reg base)

let string_of_store s =
  let { base; src; offset } = s in
  Printf.sprintf "%s,%s(%s)" (string_of_reg src) (string_of_imm offset) (string_of_reg base)

let string_of_opimm o =
  let { lhs; rhs; dst }: ActualInstr.op_imm_type = o in
  Printf.sprintf "%s,%s,%s" (string_of_reg dst) (string_of_reg lhs) (string_of_imm rhs)

let string_of_op o =
  let { lhs; rhs; dst }: ActualInstr.op_type = o in
  Printf.sprintf "%s,%s,%s" (string_of_reg dst) (string_of_reg lhs) (string_of_reg rhs)

let string_of_line instr args = Printf.sprintf "%s\t\t%s" instr args

let string_of_rs r s = Printf.sprintf "%s,%s" (string_of_reg r) s
let string_of_rim r c = Printf.sprintf "%s,%s" (string_of_reg r) (string_of_imm c)
let string_of_rr r1 r2 = Printf.sprintf "%s,%s" (string_of_reg r1) (string_of_reg r2)
let string_of_rrs r1 r2 s = Printf.sprintf "%s,%s,%s" (string_of_reg r1) (string_of_reg r2) s
let string_of_rrim r1 r2 c = 
  Printf.sprintf "%s,%s,%s" (string_of_reg r1) (string_of_reg r2) (string_of_imm c)

(* FIXME *)
let sw_instr = if wsize = 8 then "sd" else "sw"
let lw_instr = if wsize = 8 then "ld" else "lw"

let string_of_actual instr = match instr with
  | LUI { dst; imm } -> string_of_line "lui" (string_of_rim dst imm)
  | AUIPC { dst; imm } -> string_of_line "auipc" (string_of_rim dst imm)
  | JAL { dst; offset } -> string_of_line "jal" (string_of_rim dst offset)
  | JALR { base; dst; offset } -> string_of_line "jalr" (string_of_rrim dst base offset)
  | BEQ b -> string_of_line "beq" (string_of_branch b)
  | BNE b -> string_of_line "bne" (string_of_branch b)
  | BLT b -> string_of_line "blt" (string_of_branch b)
  | BGE b -> string_of_line "bge" (string_of_branch b)
  | BLTU b -> string_of_line "bltu" (string_of_branch b)
  | BGEU b -> string_of_line "bgeu" (string_of_branch b)
  | LB l -> string_of_line "lb" (string_of_load l)
  | LH l -> string_of_line "lh" (string_of_load l)
  | LW l -> string_of_line lw_instr (string_of_load l)
  | LBU l -> string_of_line "lbu" (string_of_load l)
  | LHU l -> string_of_line "lhu" (string_of_load l)
  | SB s -> string_of_line "sb" (string_of_store s)
  | SH s -> string_of_line "sh" (string_of_store s)
  | SW s -> string_of_line sw_instr (string_of_store s)
  | ADDI o -> string_of_line "addi" (string_of_opimm o)
  | SLTI o -> string_of_line "slti" (string_of_opimm o)
  | SLTIU o -> string_of_line "sltiu" (string_of_opimm o)
  | XORI o -> string_of_line "xori" (string_of_opimm o)
  | ORI o -> string_of_line "ori" (string_of_opimm o)
  | ANDI o -> string_of_line "andi" (string_of_opimm o)
  | SLLI o -> string_of_line "slli" (string_of_opimm o)
  | SRLI o -> string_of_line "srli" (string_of_opimm o)
  | SRAI o -> string_of_line "stai" (string_of_opimm o)
  | ADD o -> string_of_line "add" (string_of_op o)
  | SUB o -> string_of_line "sub" (string_of_op o)
  | SLL o -> string_of_line "sll" (string_of_op o)
  | SLT o -> string_of_line "slt" (string_of_op o)
  | SLTU o -> string_of_line "sltu" (string_of_op o)
  | XOR o -> string_of_line "xor" (string_of_op o)
  | SRL o -> string_of_line "srl" (string_of_op o)
  | SRA o -> string_of_line "sra" (string_of_op o)
  | OR o -> string_of_line "or" (string_of_op o)
  | AND o -> string_of_line "and" (string_of_op o)

let string_of_pseudo instr = match instr with
  | LA { rd; symbol } -> string_of_line "la" (string_of_rs rd symbol)
  | LG { rd; symbol } -> string_of_line lw_instr (string_of_rs rd symbol)
  | SG { rd; rt; symbol } -> string_of_line sw_instr (Printf.sprintf "%s,%s,%s" (string_of_reg rd) symbol (string_of_reg rt))
  | NOP -> "nop"
  | LI { rd; imm } -> string_of_line "li" (string_of_rs rd (string_of_int imm))
  | MV { rd; rs } -> string_of_line "mv" (string_of_rr rd rs)
  | NOT { rd; rs } -> string_of_line "not" (string_of_rr rd rs)
  | NEG { rd; rs } -> string_of_line "neg" (string_of_rr rd rs)
  | SEQZ { rd; rs } -> string_of_line "seqz" (string_of_rr rd rs)
  | SNEZ { rd; rs } -> string_of_line "snez" (string_of_rr rd rs)
  | SLTZ { rd; rs } -> string_of_line "sltz" (string_of_rr rd rs)
  | SGTZ { rd; rs } -> string_of_line "sgtz" (string_of_rr rd rs)
  | BEQZ { rs; offset } -> string_of_line "beqz" (string_of_rs rs offset)
  | BNEZ { rs; offset } -> string_of_line "bnez" (string_of_rs rs offset)
  | BLEZ { rs; offset } -> string_of_line "blez" (string_of_rs rs offset)
  | BGEZ { rs; offset } -> string_of_line "bgez" (string_of_rs rs offset)
  | BLTZ { rs; offset } -> string_of_line "bltz" (string_of_rs rs offset)
  | BGTZ { rs; offset } -> string_of_line "bgtz" (string_of_rs rs offset)
  | BGT { rs; rt; offset } -> string_of_line "bgt" (string_of_rrs rs rt offset)
  | BLE { rs; rt; offset } -> string_of_line "ble" (string_of_rrs rs rt offset)
  | J { offset } -> string_of_line "j" offset
  | JAL { offset } -> string_of_line "jal" offset
  | JR { rs } -> string_of_line "jr" (string_of_reg rs)
  | JALR { rs } -> string_of_line "jalr" (string_of_reg rs)
  | RET -> "ret"
  | CALL { offset } -> string_of_line "call" offset
  | TAIL { offset } -> string_of_line "tail" offset

let string_of_instr i = match i with
  | Actual i -> string_of_actual i
  | Pseudo i -> string_of_pseudo i

let string_of_opsvar v = match v with
  | Symbol "entry" -> "main"
  | Symbol v -> v
  | Num v -> Int32.to_string v

let string_of_ops o = match o with
  | File s -> Printf.sprintf ".file .%s" s
  | Globl s -> Printf.sprintf ".globl %s" (string_of_opsvar s)
  | Local s -> Printf.sprintf ".local %s" (string_of_opsvar s)
  | Section s ->
      let s = match s with
        | Text -> "text"
        | Data -> "data"
        | Rodata -> "rodata"
        | Bss -> "bss"
      in
      Printf.sprintf ".section .%s" s
  | Word s -> Printf.sprintf ".dword %s" (string_of_opsvar s)
  | String s -> Printf.sprintf ".string \"%s\"" s

let string_of_assm assm = 
  let aux line = match line with
    | Instr i -> "\t" ^ (string_of_instr i)
    | Ops o -> "\t" ^ (string_of_ops o)
    | Label l ->
        let l = if l = "entry" then "main" else l in
        l ^ ":"
  in
  String.concat "\n" (List.map aux assm)
