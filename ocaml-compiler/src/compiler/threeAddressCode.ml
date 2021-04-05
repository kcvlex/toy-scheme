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
  | PrimCall (_, _, _, id) -> id

let new_instr vec instr = Vector.push_back vec instr 

let push_bind vec reg v = 
  let id = SlotNumber.fresh instr_id_slot in
  let instr = Bind (reg, v, id) in
  new_instr vec instr

let push_move vec reg v =
  let id = SlotNumber.fresh instr_id_slot in
  let instr = Move (reg, v, id) in
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

let push_primcall vec dst p vl =
  let id = SlotNumber.fresh instr_id_slot in
  let instr = PrimCall (dst, p, vl, id) in
  new_instr vec instr

let set_args vec lis =
  let rec aux lis i = match lis with
    | x :: xs -> push_move vec (Argument i) x; aux xs (i + 1)
    | [] -> ()
  in
  aux lis 0

let rec from_abs_proc ptbl var2vreg vec proc =
  let recf p = from_abs_proc ptbl var2vreg vec p in
  let recv v = from_abs_value ptbl var2vreg vec v in
  match proc with
    | p_hd :: p_tl -> (match p_hd with
      | AbstractMachineType.Bind (s, v) ->
          let v = recv v in
          let reg = get_vreg var2vreg s in
          push_bind vec reg v;
          recf p_tl
      | AbstractMachineType.Move (s, v) ->
          let v = recv v in
          let reg = get_vreg var2vreg s in
          push_move vec reg v; recf p_tl
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
            push_move vec RV (Reg s); push_return vec; recf p_tl
          in
          (match Vector.rget vec 0 with
            | Bind (s, _, _) -> f s
            | Move (s, _, _) -> f s
            | Jump _ -> push_return vec; recf p_tl;     (* FIXME : tail call, maybe *)
            | Load (s, _, _, _) -> f s
            | Store (s, _, _, _) -> f s
            | PrimCall (s, _, _, _) -> f s
            | Test _ -> raise (Invalid_argument "Test")
            | Return _ -> raise (Invalid_argument "Return"))
      | AbstractMachineType.Value v ->
          let id = SlotNumber.fresh instr_id_slot in
          let v = recv v in
          let reg = SlotNumber.fresh vreg_slot in
          Vector.push_back vec (Move (reg, v, id));
          recf p_tl
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
        Reg RV
    | AbstractMachineType.RA -> Reg RV
    | AbstractMachineType.Nil -> Nil
    | AbstractMachineType.Quote a -> Quote a
    | AbstractMachineType.Cons (car, cdr) ->
        let car = recv car in
        let reg1 = SlotNumber.fresh vreg_slot in
        push_move vec reg1 car;
        let cdr = recv cdr in
        let reg2 = SlotNumber.fresh vreg_slot in
        push_move vec reg2 cdr;
        set_args vec [ Reg reg1; Reg reg2 ];
        push_jump vec (FuncLabel (string_of_sym (PrimitiveSym CONS)));
        Reg RV
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
              push_primcall vec reg p [ x1; x2 ];
              (match xs with
                | [] -> ()
                | _ -> reduce ((Reg reg) :: xs))
          | [ x ] -> push_primcall vec reg p [ x ]
          | [] -> push_primcall vec reg p []
        in
        reduce args
      | EQ | LESS ->
          let x1, x2 = restrict_2arg args in
          push_primcall vec reg p [ x1; x2 ]
      | NULL ->
          let x = restrict_1arg args in
          push_primcall vec reg p [ x ]
      | CAR | CDR | CONS | LIST | LISTREF | APPLY | MAP | DISPLAY ->
          set_args vec args;
          push_jump vec (FuncLabel (string_of_sym (PrimitiveSym p))))
    | _ -> 
        let f = from_abs_value ptbl var2vreg vec f in
        set_args vec args; push_jump vec f

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
          push_move vec reg (Reg (Argument i)); assign_args xs (i + 1)
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
  (sigtbl, ptbl, vec)


let vec_of_list lis =
  let ret = Vector.empty () in
  let rec aux lis = match lis with
    | x :: xs -> Vector.push_back ret x; aux xs
    | [] -> ()
  in
  aux lis; ret

let sample_program =
  let a = Virtual 0 in
  let b = Virtual 1 in
  let c = Virtual 2 in
  let d = Virtual 3 in
  let proc = [
    Move     (a, Int 1,                   0);
    PrimCall (b, ADD, [ Reg a; Int 1 ],   1);
    PrimCall (c, ADD, [ Reg c; Reg b ],   2);
    PrimCall (a, MUL, [ Reg b; Int 2 ],   3);
    PrimCall (d, LESS, [ Reg a; Int 10 ], 4);
    Jump     (JumpLabel "1",              5);
    Move     (RV, Reg c,                  6);
    Return   (                            7)
  ]
  in
  let ptbl = Hashtbl.create 1 in
  Hashtbl.add ptbl "1" 1;
  (Hashtbl.create 0, ptbl, vec_of_list proc)
