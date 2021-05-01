open SymbolType
open Symbol
open Util
open RegsType
open Regs
open ThreeAddressCodeType

let vreg_slot = SlotNumber.make (fun x -> Virtual x)
let br_label_slot = SlotNumber.make (fun x -> "__LABEL_br_" ^ (string_of_int x))

let allocate_label = "allocate"

(******************** Utils ********************)
let get_vreg var2vreg name =
  if Hashtbl.mem var2vreg name then
    Hashtbl.find var2vreg name
  else
    let vreg = SlotNumber.fresh vreg_slot in
    Hashtbl.add var2vreg name vreg; vreg

let restrict_1arg lis = match lis with
  | [ x ] -> x
  | _ -> raise (Invalid_argument "must be 1 element")

let restrict_2arg lis = match lis with
  | x1 :: x2 :: [] -> (x1, x2)
  | _ -> raise (Invalid_argument "must be 2 element")

let get_instr_id instr = match instr with
  | Bind (_, _, id) -> id
  | Move (_, _, id) -> id
  | Test (_, _, id) -> id
  | Jump (_, id) -> id
  | Call (_, _, id) -> id
  | Return id -> id
  | Load (_, _, _, id) -> id
  | Store (_, _, _, id) -> id

let replace_id instr id = match instr with
  | Bind (r, v, _) -> Bind (r, v, id)
  | Move (a, b, _) -> Move (a, b, id)
  | Test (a, b, _) -> Test (a, b, id)
  | Jump (t, _) -> Jump (t, id)
  | Call (a, b, _) -> Call (a, b, id)
  | Return _ -> Return id
  | Load (a, b, c, _) -> Load (a, b, c, id)
  | Store (a, b, c, _) -> Store (a, b, c, id)

let reset_id seq =
  let new_seq = Vector.empty () in
  let new_label_tbl = Hashtbl.create 0 in
  let reset_label_tbl label i = match label with
    | Some l -> Hashtbl.add new_label_tbl l i
    | None -> ()
  in
  let rec aux i =
    if i = Vector.length seq then
      (new_seq, new_label_tbl)
    else begin
      let instr, label = Vector.get seq i in
      let instr = replace_id instr (Vector.length new_seq) in
      Vector.push_back new_seq (instr, label);
      reset_label_tbl label i;
      aux (i + 1)
    end
  in
  aux 0


(******************** Dump ********************)

let rec string_of_value value = match value with
  | Int i -> string_of_int i
  | Bool true -> "true"
  | Bool false -> "false"
  | Reg r -> string_of_reg r
  | Nil -> "null"
  | Label s -> s ^ " (label)"
  | Primitive p -> string_of_sym (PrimitiveSym p)
  | Quote a -> "`" ^ (Ast.code_of_ast a)
  | PrimCall (p, l) ->
      let p = string_of_sym (PrimitiveSym p) in
      let l = 
        l |> List.map string_of_value
          |> String.concat " "
      in
      Printf.sprintf "(%s %s)" p l

let string_of_instr instr = match instr with
  | Bind (a, b, _) ->
      let a = string_of_reg a in
      let b = string_of_value b in
      Printf.sprintf "%s <- %s (bind)" a b
  | Move (a, b, _) ->
      let a = string_of_reg a in
      let b = string_of_reg b in
      Printf.sprintf "%s <- %s (move)" a b
  | Test (a, b, _) ->
      let a = string_of_value a in
      let b = string_of_value b in
      Printf.sprintf "if %s then %s" a b
  | Jump (a, _) -> Printf.sprintf "jump %s" (string_of_value a)
  | Call (a, _, _) -> Printf.sprintf "call %s" (string_of_value a)
  | Return _ -> "return"
  | Load (a, b, c, _) ->
      let a = string_of_reg a in
      let b = string_of_reg b in
      Printf.sprintf "%s <- %d(%s)" a c b
  | Store (a, b, c, _) ->
      let a = string_of_reg a in
      let b = string_of_reg b in
      Printf.sprintf "%d(%s) <- %s" c a b


(******************** Trans AbstractMachineCode to ThreeAddressCode ********************)
let rv_reg = Argument 0
let wsize = 8
let sentinel_num = Int32.min_int |> Int32.to_int

let make_assign dst src = match src with
  | Reg r -> Move (dst, r, -1)
  | _ -> Bind (dst, src, -1)

let argreg_list l r =
  let rec aux idx =
    if idx = r then
      []
    else
      (Argument idx) :: (aux (idx + 1))
  in
  aux l

let call_func vec f args =
  let rec aux i l = match l with
    | x :: xs -> Vector.push_back vec (make_assign (Argument i) x, None); aux (i + 1) xs
    | [] -> ()
  in
  let set_sentinel l =
    let reg = Argument l in
    let mv = make_assign reg (Int sentinel_num) in
    Vector.push_back vec (mv, None)
  in
  match f with
    | Reg r ->
        let reg1 = SlotNumber.fresh vreg_slot in
        let reg2 = SlotNumber.fresh vreg_slot in
        Vector.push_back vec (Move (reg2, r, -1), None);
        Vector.push_back vec (Move (Argument 0, reg2, -1), None);
        Vector.push_back vec (Bind (reg1, PrimCall (CAR, [ Reg (Argument 0) ]), -1), None);
        Vector.push_back vec (Move (Argument 0, reg2, -1), None);
        Vector.push_back vec (Bind (reg2, PrimCall (CDR, [ Reg (Argument 0) ]), -1), None);
        aux 1 args;
        set_sentinel (List.length args + 1);
        Vector.push_back vec (Move (Argument 0, reg2, -1), None);
        Vector.push_back vec (Call (Reg reg1, argreg_list 0 (List.length args + 1), -1), None)
    | Primitive _ ->
        aux 0 args;
        Vector.push_back vec (Call (f, argreg_list 0 (List.length args), -1), None)
    | Label _ ->
        aux 0 args;
        set_sentinel (List.length args);
        Vector.push_back vec (Call (f, argreg_list 0 (List.length args), -1), None)
    | _ -> raise (Invalid_argument "Jump")

let call_prim p vec args = match p with
    | ADD | SUB | MUL | DIV -> (match args with
      | [] | _ :: [] -> raise (Invalid_argument "binop")
      | x :: y :: [] -> PrimCall (p, [ x; y ])
      | _ ->
        let reg = SlotNumber.fresh vreg_slot in
        let rec reduce lis = match lis with
          | x1 :: x2 :: xs ->
              let rhs = PrimCall (p, [ x1; x2 ]) in
              Vector.push_back vec (Bind (reg, rhs, -1), None);
              let xs = match xs with
                | [] -> []
                | _ -> (Reg reg) :: xs
              in
              reduce xs
          | [ _ ] -> raise (Invalid_argument "binop received 1 operand")
          | [] -> reg
        in
        Reg (reduce args))
    | EQ | LESS ->
        let x1, x2 = restrict_2arg args in
        PrimCall (p, [ x1; x2 ])
    | NULL ->
        let x = restrict_1arg args in
        PrimCall (p, [ x ])
    | CAR | CDR | DISPLAY ->
        let x = restrict_1arg args in
        call_func vec (Primitive p) [ x ];
        Reg rv_reg
    | CONS | LISTREF ->
        let x, y = restrict_2arg args in
        call_func vec (Primitive p) [ x; y ];
        Reg rv_reg
    | LIST ->
        call_func vec (Primitive p) args;
        Reg rv_reg
    | _ -> raise (Invalid_argument (string_of_sym (PrimitiveSym p)))

let rec from_abs_value var2vreg vec value =
  let recv v = from_abs_value var2vreg vec v in
  let trans_args l =
    let rec aux l i = match l with
      | x :: xs ->
          let a = recv x in
          let r = SlotNumber.fresh vreg_slot in
          Vector.push_back vec (make_assign r a, None);
          (Reg r) :: (aux xs (i + 1))
      | [] -> []
    in
    aux l 0
  in
  match value with
    | AbstractMachineType.Int i -> Int i
    | AbstractMachineType.Bool b -> Bool b
    | AbstractMachineType.Ref s -> Reg (get_vreg var2vreg (AbstractMachine.string_of_ref s))
    | AbstractMachineType.Nil -> Nil
    | AbstractMachineType.Primitive p -> Primitive p
    | AbstractMachineType.PrimCall (p, vl) ->
        let vl = trans_args vl in
        call_prim p vec vl
    | AbstractMachineType.Quote a -> Quote a
    | AbstractMachineType.Label s -> Label s
    | AbstractMachineType.Allocate (n, vl) ->
        let vl = trans_args vl in
        call_func vec (Label allocate_label) ((Int n) :: vl);
        Reg rv_reg
    | AbstractMachineType.Cons (car, cdr) ->
        let vl = trans_args [ car; cdr ] in
        call_func vec (Primitive CONS) vl;
        Reg rv_reg
    | AbstractMachineType.AccessClosure (s, il) ->
        let s = AbstractMachine.string_of_ref s in
        let r = SlotNumber.fresh vreg_slot in
        Vector.push_back vec (Move (r, get_vreg var2vreg s, -1), None);
        let rec aux il = match il with
          | i :: xs -> Vector.push_back vec (Load (r, r, i, -1), None); aux xs
          | [] -> r
        in
        Reg (aux il)

let rec from_abs_proc var2vreg vec proc =
  let recf p = from_abs_proc var2vreg vec p in
  let recv v = from_abs_value var2vreg vec v in
  match proc with
    | hd :: tl -> (match hd with
      | AbstractMachineType.Bind (s, v) ->
          let v = recv v in
          (* FIXME FUNCTION LAGBEL *)
          let s = get_vreg var2vreg (AbstractMachine.string_of_ref s) in
          Vector.push_back vec (make_assign s v, None);
          recf tl
      | AbstractMachineType.Test (p, e1, e2) ->
          let p = recv p in
          let jt = SlotNumber.fresh br_label_slot in
          Vector.push_back vec (Test (p, Label jt, -1), None);
          recf e2;
          let idx = Vector.length vec in
          recf e1;
          let () =
            let fst, snd = Vector.get vec idx in
            assert (not (Option.is_some snd));
            Vector.set vec idx (fst, Some jt)
          in
          recf tl
      | AbstractMachineType.Return v ->
          let v = recv v in
          let () = match v with
            | Reg r when r = rv_reg -> Vector.push_back vec (Return (-1), None)
            | _ ->
                begin
                  Vector.push_back vec (make_assign rv_reg v, None);
                  Vector.push_back vec (Return (-1), None)
                end
          in
          recf tl
      | AbstractMachineType.Jump v ->
        let v = recv v in
        Vector.push_back vec (Jump (v, -1), None);
        recf tl
      | AbstractMachineType.Call (f, args) ->
          let fn a =
            let r = SlotNumber.fresh vreg_slot in
            let a = recv a in
            Vector.push_back vec (make_assign r a, None);
            Reg r
          in
          let f, args =
            let lis = List.map fn (f :: args) in
            (List.hd lis, List.tl lis)
          in
          call_func vec f args; recf tl)
    | [] -> ()

let from_abs_func (c, args, oarg, body) =
  let vec = Vector.empty () in
  let var2vreg =
    let tbl = Hashtbl.create 8 in
    let args = if Option.is_some c then (Option.get c) :: args else args in
    let args = if Option.is_some oarg then List.append args [ Option.get oarg ] else args in
    let args = List.map AbstractMachine.string_of_ref args in
    let rec assign_args lis i = match lis with
      | x :: xs ->
          let reg = get_vreg tbl x in
          Vector.push_back vec (Move (reg, Argument i, -1), None);
          assign_args xs (i + 1)
      | [] -> ()
    in
    assign_args args 0;
    Hashtbl.add tbl "__RV" (Argument 0);
    tbl
  in
  from_abs_proc var2vreg vec body;
  reset_id vec

let output_program = ref true

let set_logging f = output_program := f
 
let print_program program =
  if !output_program then
    print_endline (AbstractMachine.string_of_program program)
  else
    ()

let from_abs_program program =
  let ft, jt = program in
  assert (Hashtbl.length jt = 0);
  print_program program;
  let seq, ltbl =
    let labeling name body = match body with
      | (_ as hd, None) :: tl -> (hd, Some name) :: tl
      | _ -> raise (Invalid_argument "flabel")
    in
    let trans name proc =
      proc
      |> from_abs_func
      |> fst
      |> Vector.list_of_vector
      |> labeling name
    in
    ft
    |> fun tbl -> Hashtbl.fold (fun x y l -> (trans x y) :: l) tbl []
    |> List.flatten
    |> Vector.vector_of_list
    |> reset_id
  in
  let signature =
    let trans cl args ea =
      let cl = Option.map AbstractMachine.string_of_ref cl in
      let args = List.map AbstractMachine.string_of_ref args in
      let ea = Option.map AbstractMachine.string_of_ref ea in
      (cl, args, ea)
    in
    let tbl = Hashtbl.create (Hashtbl.length ft) in
    Hashtbl.iter (fun x (cl, args, ea, _) -> Hashtbl.add tbl x (trans cl args ea)) ft;
    tbl
  in
  { signature; ltbl; seq }


(******************** Trans ThreeAddressCode to AbstractMachineCode ********************)
let insert_jump lv =
  let rec aux lis = match lis with
    | (Test _ as i1, l1) :: (i2, l2) :: xs ->
        let l2 = match l2 with
          | Some l -> l
          | None -> SlotNumber.fresh br_label_slot
        in
        let xs = (i2, Some l2) :: xs in
        let xs = aux xs in
        (i1, l1) :: (Jump (Label l2, -1), None) :: xs
    | ((i1, _) as e1) :: ((i2, Some l2) as e2) :: xs -> (match i1 with    (* i1 isn't Test *)
      | Return _ | Jump _ -> e1 :: (aux (e2 :: xs))
      | _ -> e1 :: (Jump (Label l2, -1), None) :: (aux (e2 :: xs)))
    | x :: xs -> x :: (aux xs)
    | [] -> []
  in
  lv |> Vector.list_of_vector 
     |> aux
     |> Vector.vector_of_list 
     |> reset_id

let abs_rv = AbstractMachineType.RV

let trans_reg r = match r with
  | Virtual x when x <= (-2) -> AbstractMachineType.Local (Printf.sprintf "__mem_%d" x)
  | Virtual _ -> AbstractMachineType.Local (string_of_reg r)
  | Argument 0 -> AbstractMachineType.RV
  | _ -> AbstractMachineType.Global (string_of_reg r)

let rec abs_of_value value = match value with
  | Int i -> AbstractMachineType.Int i
  | Bool b -> AbstractMachineType.Bool b
  | Reg r -> AbstractMachineType.Ref (trans_reg r)
  | Nil -> AbstractMachineType.Nil
  | PrimCall (p, vl) -> AbstractMachineType.PrimCall (p, List.map abs_of_value vl)
  | Primitive p -> AbstractMachineType.Primitive p
  | Label l -> AbstractMachineType.Label l
  | Quote a -> AbstractMachineType.Quote a

let rec abs_of_block b = match b with
  | Test (p, v1, _) :: xs ->
      let p = abs_of_value p in
      let v1 = abs_of_value v1 in
      let jt = AbstractMachineType.Jump v1 in
      let res = match xs with
        | [] -> AbstractMachineType.Test (p, [ jt ], [])
        | (Return _) :: [] -> 
            AbstractMachineType.Test (p, [ jt ], [ AbstractMachineType.Return (Ref abs_rv) ])
        | (Jump (jf, _)) :: [] ->
            let jf = AbstractMachineType.Jump (abs_of_value jf) in
            AbstractMachineType.Test (p, [ jt ], [ jf ])
        | _ -> raise (Invalid_argument "Test")
      in
      [ res ]
  | x :: xs -> 
      let x = match x with
        | Bind (r, Primitive p, _) -> AbstractMachineType.Bind (trans_reg r, PrimitiveCLO p)
        | Bind (r, v, _) -> AbstractMachineType.Bind (trans_reg r, abs_of_value v)
        | Move (dst, src, _) ->
            let dst = trans_reg dst in
            let src = trans_reg src in
            AbstractMachineType.Bind (dst, AbstractMachineType.Ref src)
        | Jump (v, _) -> AbstractMachineType.Jump (abs_of_value v)
        | Call (v, _, _) when v = Label "allocate" -> 
            AbstractMachineType.Call (abs_of_value v, [ abs_of_value (Reg (Argument 0)) ])
        | Call (v, l, _) ->
            let l = List.map (fun x -> Reg x) l in
            AbstractMachineType.Call (abs_of_value v, List.map abs_of_value l)
        | Return _ -> AbstractMachineType.Return (Ref abs_rv)
        | Load (dst, src, offset, _) ->
            (* FIXME *)
            let dst = trans_reg dst in
            let src = trans_reg src in
            let src = AbstractMachineType.AccessClosure (src, [ offset ]) in
            AbstractMachineType.Bind (dst, src)
        | Store (dst, src, offset, _) ->
            (* FIXME *)
            assert (offset = 0);
            let dst = trans_reg dst in
            let src = trans_reg src in
            AbstractMachineType.Bind (dst, Ref src)
        | Test _ -> raise (Invalid_argument "UNREACHABLE")
      in
      let xs = abs_of_block xs in
      x :: xs
  | [] -> []

let split_by_labels lis =
  let rec aux lis cur res = match lis with
    | (i, Some l) :: xs ->
        let cur = i :: cur in
        aux xs [] ((l, cur) :: res)
    | (i, _) :: xs -> aux xs (i :: cur) res
    | [] -> res
  in
  aux (List.rev lis) [] []

let reformat_sig (clarg, args, earg) =
  let i = ref 0 in
  let assign _ =
    let c = !i in
    incr i;
    if c = 0 then
      AbstractMachineType.RV
    else begin
      let r = Argument c in
      AbstractMachineType.Global (string_of_reg r)
    end
  in
  let rec aux args = match args with
    | x :: xs ->
        let x = assign x in
        let xs = aux xs in
        x :: xs
    | [] -> []
  in
  let clarg = Option.map assign clarg in
  let args = aux args in
  let earg = Option.map assign earg in
  (clarg, args, earg)

let to_abs_program program =
  let { signature; ltbl = _; seq } = program in
  let signature =
    let tbl = Hashtbl.create (Hashtbl.length signature) in
    Hashtbl.iter (fun x y -> Hashtbl.add tbl x (reformat_sig y)) signature;
    tbl
  in
  let transed =
    seq 
    |> insert_jump
    |> fst
    |> Vector.list_of_vector
    |> split_by_labels
    |> List.map (fun (x, y) -> (x, abs_of_block y))
  in
  let func_table = Hashtbl.create (Hashtbl.length signature) in
  let jump_table = Hashtbl.create ((List.length transed) - (Hashtbl.length signature)) in
  let update (l, p) =
    if Hashtbl.mem signature l then begin
      let a, b, c = Hashtbl.find signature l in
      Hashtbl.add func_table l (a, b, c, p)
    end else begin
      Hashtbl.add jump_table l p
    end
  in
  List.iter update transed;
  (func_table, jump_table)


(******************** Sample ********************)
let sample1 =
  let a = Virtual 0 in
  let b = Virtual 1 in
  let c = Virtual 2 in
  let seq = [
    (Bind   (a, Int 1,                                      0), Some "entry");
    (Bind   (b, PrimCall (ADD, [ Reg a; Int 1 ]),           1), Some "1");
    (Bind   (c, PrimCall (ADD, [ Reg c; Reg b ]),           2), None);
    (Bind   (a, PrimCall (MUL, [ Reg b; Int 2 ]),           3), None);
    (Test   (PrimCall (LESS, [ Reg a; Int 10 ]), Label "1", 4), None);
    (Move   (rv_reg, c,                                     5), None);
    (Return (                                               6), None)
  ]
  in
  let seq = Vector.vector_of_list seq in
  let seq, ltbl = reset_id seq in
  { signature = Hashtbl.create 0; ltbl; seq; }

let sample2 =
  let a = Virtual 0 in
  let b = Virtual 1 in
  let c = Virtual 2 in
  let d = Virtual 3 in
  let e = Virtual 4 in
  let r1 = Argument 0 in
  let r2 = Argument 1 in
  let r3 = CalleeSaved 0 in
  let seq = [
    (Move   (a, r1,                                           0), Some "f");
    (Move   (b, r2,                                           1), None);
    (Bind   (d, Int 0,                                        2), None);
    (Move   (e, a,                                            3), None);
    (Bind   (d, PrimCall (ADD, [ Reg d; Reg b ]),             4), Some "loop");
    (Bind   (e, PrimCall (SUB, [ Reg e; Int 1 ]),             5), None);
    (Test   (PrimCall (LESS, [ Int 0; Reg e ]), Label "loop", 6), None);
    (Move   (rv_reg, d,                                       7), None);
    (Return (                                                 8), None)
  ]
  in
  let seq, ltbl = seq |> Vector.vector_of_list |> reset_id in
  let signature = Hashtbl.create 1 in
  Hashtbl.add signature "f" (None, List.map string_of_reg [ Argument 0; Argument 1 ], None);
  { signature; ltbl; seq }
