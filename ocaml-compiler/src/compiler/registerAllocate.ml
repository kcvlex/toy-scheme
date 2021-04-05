open Util
open ThreeAddressCode
open ThreeAddressCodeType

type t = {
  reg_num : int * int * int;
  intrf_g : (reg_type, bool) Graph.t;
  proc : instr_type Vector.t;
  liveness : Liveness.t;
  move_pair : (reg_type, reg_type) Hashtbl.t;
  move_related : reg_type Hashtbl.t;
  freezed : reg_set;
  spilled : reg_set;
  stack : reg_type Stack.t;
}

let offset = 10000
let vreg_slot = SlotNumber.make (fun x -> Virtual (x + offset))
let dummy_reg = Virtual -1
  
let replace_id instr id = match instr with
  | Bind (s, v, _) -> Bind (s, v, id)
  | Move (s, v, _) -> Move (s, v, id)
  | Test (a, b, _) -> Test (a, b, id)
  | Jump (t, _) -> Jump (t, id)
  | Return _ -> Return id
  | Load (a, b, c, _) -> Load (a, b, c, id)
  | Store (a, b, c, _) -> Store (a, b, c, id)
  | PrimCall (a, b, c, _) -> PrimCall (a, b, c, id)

let reset_id vec =
  for i in 0 to (Vector.length vec) - 1 do
    let instr = Vector.get vec i in
    let instr = replace_id instr i in
    Vector.set vec i instr
  done

let split_program (_, ptbl, vec) =
  let funcs =
    List.split sigtbl
    |> fst
    |> List.map (fun x -> (x, Hashtbl.find ptbl x))
    |> List.sort (fun x y -> (snd x) - (snd y))
  in
  let order, idxlis = List.split funcs in
  let vvec = Vector.empty () in
  let rec split idx idxlis new_vec =
    if idx = Vector.length vec then
      ()
    else begin
      let idxlis, new_vec = match idxlis with
        | x :: xs when x = idx -> 
            vector.push_back vvec (vector.copy new_vec);
            (xs, vector.empty ())
        | _ -> (idxlis, new_vec)
      in
      vector.push_back new_vec (vector.get vec idx);
      split (idx + 1) idxlis new_vec
    end
  in
  split 0 idxlis (Vector.empty ());
  for i in 0 to (Vector.length vvec) - 1 do
    reset_id (Vector.get vvec i)
  done;
  vvec

let insert_caller_save rnum vec =
  let ret = Vector.empty () in
  let sv = Vector.empty () in
  for i in 0 to rnum - 1 do
    let reg = SlotNumber.fresh vreg_slot in
    let instr = Move (reg, Reg (CallerSave i), i) in
    Vector.push_back sv reg;
    Vector.push_back ret instr
  done;
  for i in 0 to (Vector.length vec) - 1 do
    let instr = Vector.get vec i in
    Vector.push_back ret (replace_id (i + rnum))
  done;
  for i in 0 to rnum - 1 do
    let id = Vector.length ret in
    let reg = Vector.get sv i in
    let instr = Move (CallerSave i, Reg reg, id) in
    Vector.push_back ret instr
  done;
  ret


(* Build interference graph *)
let build reg_num vec ptbl =
  let g = Graph.make false in
  let livness = Liveness.analyze vec ptbl in
  let mp = Hashtbl.create 8 in
  let add_edges r i =
    let live_out = Liveness.live_out alloc.liveness i in
    Hashtbl.iter (fun x _ -> Graph.add_edge g r s) live_out
  in
  let f instr = match instr with
    | Bind (r, s, i) -> add_edges r i
    | Load (r, _, _, i) -> add_edges r i
    | PrimCall (r, _, _, i) -> add_edges r i
    | Move (lhs, s, i) ->
        let rhs = match s with
            | Reg reg -> reg
            | _ -> dummy_reg
        in
        let live_out = Liveness.live_out alloc.liveness i in
        Hashtbl.iter (fun x _-> if x != rhs then Graph.add_edge lhs x else ()) live_out;
    | _ -> ()
  in
  Graph.rm_node dummy_reg;
  let mp =
    let new_mp = Hashtbl.create 8 in
    let add (x, y) _ = if Graph.has_edge g x y then () else Hashtbl.add new_mp (x, y) () in
    Hashtbl.iter add mp; new_mp
  in
  let mr =
    let tbl = Hashtbl.create (Hashtbl.length mp) in
    let add x = Hashtbl.replace tbl x () in
    Hashtbl.iter (fun (x, y) _ -> begin add x; add y end) mp; tbl
  in
  {
    reg_num = alloc.reg_num;
    proc = vec;
    intrf_g = g;
    liveness = liveness;
    move_pair = mp;
    move_related = mr;
    freezed = Hashtbl.create 2;
    spilled = Hashtbl.create 2;
    stack = Stack.create ()
  }

let sum3 (a, b, c) = a + b + c

let simplify alloc =
  let rsum = sum3 alloc.reg_num in
  let nodes = Graph.get_nodes alloc intrf_g in
