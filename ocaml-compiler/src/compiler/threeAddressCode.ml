open SymbolType
open Symbol
open Util
open ThreeAddressCodeType

let vreg_slot = SlotNumber.make (fun x -> Virtual x)
let br_label_slot = SlotNumber.make (fun x -> "__LABEL_br_" ^ (string_of_int x))
let instr_id_slot = SlotNumber.make (fun x -> x)

let allocate_label = FuncLabel "allocate"

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
  | Return id -> id
  | Load (_, _, _, id) -> id
  | Store (_, _, _, id) -> id

let replace_id instr id = match instr with
  | Bind (r, v, _) -> Bind (r, v, id)
  | Move (a, b, _) -> Move (a, b, id)
  | Test (a, b, _) -> Test (a, b, id)
  | Jump (t, _) -> Jump (t, id)
  | Return _ -> Return id
  | Load (a, b, c, _) -> Load (a, b, c, id)
  | Store (a, b, c, _) -> Store (a, b, c, id)

let new_instr vec instr = Vector.push_back vec instr 

let push_assign vec dst v =
  let id = SlotNumber.fresh instr_id_slot in
  let instr = match v with
    | Reg r -> Move (dst, r, id)
    | _ -> Bind (dst, v, id)
  in
  new_instr vec instr

let push_test vec v l =
  let id = SlotNumber.fresh instr_id_slot in
  let instr = Test (v, l, id) in
  new_instr vec instr

let push_jump vec v =
  let id = SlotNumber.fresh instr_id_slot in
  let instr = Jump (v, id) in
  new_instr vec instr

let push_return vec =
  let id = SlotNumber.fresh instr_id_slot in
  let instr = Return id in
  new_instr vec instr

let push_load vec dst base offset =
  let id = SlotNumber.fresh instr_id_slot in
  let instr = Load (dst, base, offset, id) in
  new_instr vec instr

let push_store vec src base offset =
  let id = SlotNumber.fresh instr_id_slot in
  let instr = Store (src, base, offset, id) in
  new_instr vec instr

let set_args vec lis =
  let rec aux lis i = match lis with
    | x :: xs -> push_assign vec (Argument i) x; aux xs (i + 1)
    | [] -> ()
  in
  aux lis 0

let rv_reg = Argument 0

let rec from_abs_proc ptbl var2vreg vec proc =
  let recf p = from_abs_proc ptbl var2vreg vec p in
  let recv v = from_abs_value ptbl var2vreg vec v in
  match proc with
    | p_hd :: p_tl -> (match p_hd with
      | AbstractMachineType.Bind (s, v) ->
          let v = recv v in
          let reg = get_vreg var2vreg s in
          push_assign vec reg v;
          recf p_tl
      | AbstractMachineType.Move (s, v) ->
          let v = recv v in
          let reg = get_vreg var2vreg s in
          push_assign vec reg v;
          recf p_tl
      | AbstractMachineType.Test (v, t1, t2) ->
          let v = recv v in
          let l1 = SlotNumber.fresh br_label_slot in
          let l2 = SlotNumber.fresh br_label_slot in
          push_test vec v (JumpLabel l1);
          recf t2;
          push_jump vec (JumpLabel l2);
          Hashtbl.add ptbl l1 (Vector.length vec);
          recf t1;
          Hashtbl.add ptbl l2 (Vector.length vec);
          recf p_tl
      | AbstractMachineType.Return ->
          let f s = 
            push_assign vec rv_reg (Reg s); push_return vec; recf p_tl
          in
          (match Vector.rget vec 0 with
            | Bind (s, _, _) -> f s
            | Move (s, _, _) -> f s
            | Jump _ -> push_return vec; recf p_tl;     (* FIXME : tail call, maybe *)
            | Load (s, _, _, _) -> f s
            | Store (s, _, _, _) -> f s
            | Test _ -> raise (Invalid_argument "Test")
            | Return _ -> raise (Invalid_argument "Return"))
      | AbstractMachineType.Value v ->
          let id = SlotNumber.fresh instr_id_slot in
          let v = recv v in
          let reg = SlotNumber.fresh vreg_slot in
          (match v with
            | Reg r -> Vector.push_back vec (Move (reg, r, id)); recf p_tl
            | _ -> Vector.push_back vec (Bind (reg, v, id)); recf p_tl)
      | AbstractMachineType.Call (f, args) ->
          let vs = List.map recv (f :: args) in
          from_abs_call ptbl var2vreg vec f vs;
          recf p_tl)
    | [] -> ()
and from_abs_value ptbl var2vreg vec value = 
  let recf p = from_abs_proc ptbl var2vreg vec p in
  let recv v = from_abs_value ptbl var2vreg vec v in
  match value with
    | AbstractMachineType.Int i -> Int i
    | AbstractMachineType.Bool b -> Bool b
    | AbstractMachineType.Primitive p -> Primitive p
    | AbstractMachineType.Ref s -> Reg (get_vreg var2vreg s)
    | AbstractMachineType.Label s -> FuncLabel s
    | AbstractMachineType.Allocate args ->
        let vs = List.map recv args in
        set_args vec vs;
        push_jump vec allocate_label;
        Reg rv_reg
    | AbstractMachineType.RA -> Reg RA
    | AbstractMachineType.Nil -> Nil
    | AbstractMachineType.Quote a -> Quote a
    | AbstractMachineType.Cons (car, cdr) ->
        let car = recv car in
        let reg1 = SlotNumber.fresh vreg_slot in
        push_assign vec reg1 car;
        let cdr = recv cdr in
        let reg2 = SlotNumber.fresh vreg_slot in
        push_assign vec reg2 cdr;
        set_args vec [ Reg reg1; Reg reg2 ];
        push_jump vec (FuncLabel (string_of_sym (PrimitiveSym CONS)));
        Reg rv_reg
    | AbstractMachineType.AccessClosure (s, il) ->
        let reg = SlotNumber.fresh vreg_slot in
        let rec aux lis base = match lis with
          | i :: is -> 
              push_load vec reg base i;
              aux is (Reg reg)
          | [] -> ()
        in
        let base = get_vreg var2vreg s in
        Reg reg
and from_abs_call ptbl var2vreg vec f args =
  let reg = SlotNumber.fresh vreg_slot in
  match f with
    | AbstractMachineType.Primitive p -> (match p with
      | ADD | SUB | MUL | DIV ->
        let rec reduce lis = match lis with
          | x1 :: x2 :: xs ->
              push_assign vec reg (PrimCall (p, [ x1; x2 ]));
              (match xs with
                | [] -> ()
                | _ -> reduce ((Reg reg) :: xs))
          | [ x ] -> push_assign vec reg (PrimCall (p, [ x ]))
          | [] -> push_assign vec reg (PrimCall (p, []))
        in
        reduce args
      | EQ | LESS ->
          let x1, x2 = restrict_2arg args in
          push_assign vec reg (PrimCall (p, [ x1; x2 ]))
      | NULL ->
          let x = restrict_1arg args in
          push_assign vec reg (PrimCall (p, [ x ]))
      | CAR | CDR | CONS | LIST | LISTREF | APPLY | MAP | DISPLAY ->
          set_args vec args;
          push_jump vec (FuncLabel (string_of_sym (PrimitiveSym p))))
    | _ -> 
        let f = from_abs_value ptbl var2vreg vec f in
        set_args vec args; push_jump vec f

let labeling vec ptbl =
  let rptbl =
    let tbl = Hashtbl.create (Hashtbl.length ptbl) in
    Hashtbl.iter (fun x y -> Hashtbl.add tbl y x) ptbl;
    tbl;
  in
  let find_opt x = 
    if Hashtbl.mem rptbl x then Some (Hashtbl.find rptbl x) else None 
  in
  let ret = Vector.empty () in
  for i = 0 to (Vector.length vec) do
    let instr = Vector.get vec i in
    let label = find_opt i in
    Vector.push_back ret (instr, label)
  done;
  ret

let from_abs_program program =
  let vec = Vector.empty () in
  let var2vreg = Hashtbl.create 8 in
  let ptbl = Hashtbl.create (Hashtbl.length program) in
  let sigtbl = Hashtbl.create (Hashtbl.length program) in
  let prolog (c, args, oarg, body) =
    let args = if Option.is_some c then (Option.get c) :: args else args in
    let args = if Option.is_some oarg then List.append args [ Option.get oarg ] else args in
    let rec assign_args lis i = match lis with
      | x :: xs -> 
          let reg = get_vreg var2vreg x in
          push_assign vec reg (Reg (Argument i)); assign_args xs (i + 1)
      | [] -> ()
    in
    assign_args args 0
  in
  let trans name (c, args, oarg, body) =
    Hashtbl.add ptbl name (Vector.length vec);
    prolog (c, args, oarg, body);
    from_abs_proc ptbl var2vreg vec body;
    Hashtbl.add sigtbl name (c, args, oarg)
  in
  Hashtbl.iter (fun x y -> trans x y) program;
  let seq = labeling vec ptbl in
  {
    signature = sigtbl;
    label_tbl = ptbl;
    seq;
  }

let string_of_reg r = match r with
  | RA -> "RA"
  | CallerSaved i -> Printf.sprintf "CER_%d" i
  | CalleeSaved i -> Printf.sprintf "CEE_%d" i
  | Argument i -> Printf.sprintf "ARG_%d" i
  | Virtual i -> Printf.sprintf "VTR_%d" i

let string_of_value value = match value with
  | Int i -> string_of_int i
  | Bool true -> "true"
  | Bool false -> "false"
  | Reg r -> string_of_reg r
  | Nil -> "null"
  | FuncLabel s -> s ^ " (function)"
  | JumpLabel s -> s ^ " (jump)"
  | Primitive p -> string_of_sym (PrimitiveSym p)
  | Quote a -> "`" ^ (Ast.code_of_ast a)

let string_of_instr instr = match instr with
  | Move (a, b, _) ->
      let a = string_of_reg a in
      let b = string_of_value b in
      Printf.sprintf "%s <- %s" a b
  | Test (a, b, _) ->
      let a = string_of_value a in
      let b = string_of_value b in
      Printf.sprintf "if %s then %s" a b
  | Jump (a, _) -> Printf.sprintf "jump %s" (string_of_value a)
  | Return _ -> "return"
  | Load (a, b, c, _) ->
      let a = string_of_reg a in
      let b = string_of_value b in
      Printf.sprintf "%s <- %d(%s)" a c b
  | Store (a, b, c, _) ->
      let a = string_of_reg a in
      let b = string_of_value b in
      Printf.sprintf "%d(%s) <- %s" c a b
  | PrimCall (a, p, l, _) ->
      let a = string_of_reg a in
      let p = string_of_sym (PrimitiveSym p) in
      let l = 
        l |> List.map string_of_value
          |> String.concat " "
      in
      Printf.sprintf "%s <- (%s %s)" a p l

let string_of_regtbl rs =
  (Hashtbl.fold (fun x _ l -> x :: l) rs [])
  |> List.map string_of_reg
  |> String.concat " "
  |> Printf.sprintf "[ %s ]"

let make_label_tbl vec =
  let ret = Hashtbl.create 8 in
  for i = 0 to (Vector.length vec) - 1 do
    let instr = Vector.get vec i in
    match instr with
      | (_, Some l) -> Hashtbl.add ret l i
      | _ -> ()
  done;
  ret

let make_reg_set reg_num =
  let caller_saved, callee_saved, args = reg_num in
  let rec aux i n cnstr =
    if i = n then [] else (cnstr i) :: (aux (i + 1) n cnstr)
  in
  let caller_saved_regs = aux 0 caller_saved (fun x -> CallerSaved x) in
  let callee_saved_regs = aux 0 callee_saved (fun x -> CalleeSaved x) in
  let argument_regs = aux 0 args (fun x -> Argument x) in
  let all_regs = List.flatten [ caller_saved_regs; callee_saved_regs; argument_regs ] in
  let reg_sum = List.length all_regs in
  { caller_saved_regs; callee_saved_regs; argument_regs; all_regs; reg_sum; }

let sample_program =
  let a = Virtual 0 in
  let b = Virtual 1 in
  let c = Virtual 2 in
  let d = Virtual 3 in
  let seq = [
    (Move     (a, Int 1,                   0), Some "entry");
    (PrimCall (b, ADD, [ Reg a; Int 1 ],   1), Some "1");
    (PrimCall (c, ADD, [ Reg c; Reg b ],   2), None);
    (PrimCall (a, MUL, [ Reg b; Int 2 ],   3), None);
    (PrimCall (d, LESS, [ Reg a; Int 10 ], 4), None);
    (Jump     (JumpLabel "1",              5), None);
    (Move     (RV, Reg c,                  6), None);
    (Return   (                            7), None)
  ]
  in
  let seq = Vector.vector_of_list seq in
  let label_tbl = make_label_tbl seq in
  { signature = Hashtbl.create 0; label_tbl; seq; }
