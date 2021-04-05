open Util
open ThreeAddressCode
open ThreeAddressCodeType

type t = {
  reg_num : int * int * int;
  rsum : int;
  intrf_g : (reg_type, unit) Graph.t;
  proc : instr_type Vector.t;
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
    rsum = (fun (x, y, z) -> x + y + z) reg_num;
    proc = vec;
    intrf_g = g;
    move_pair = mp;
    move_related = mr;
    freezed = Hashtbl.create 2;
    spilled = Hashtbl.create 2;
    stack = Stack.create ()
  }

(* FIXME *)
let check_coalesce alloc n1 n2 =
  let set1 = Hashtbl.copy (Graph.succ alloc.instr_g n1) in
  let set2 = Graph.succ alloc.instr_g n2 in
  Hashtbl.iter (fun x _ -> Hashtbl.replace set1 x) set2;
  (Hashtbl.length set1) < alloc.rsum

let exec_coalesce alloc dst src =
  Graph.contraction alloc.intrf_g dst src;
  Hashtbl.remove alloc.move_pair (dst, src)

let sort_by_deg alloc set =
  set |> fun s -> Hashtbl.fold (fun x _ l -> (x, Graph.degree alloc.intrf_g x)) s []
      |> List.sort (fun x y -> (snd x) - (snd y))

let simplify alloc =
  let rsum = sum3 alloc.reg_num in
  let nodes = Graph.nodes alloc intrf_g in
  let deg = 
    nodes
    |> List.filter (fun x -> not (Hashtbl.mem alloc.move_related x))
    |> List.map (fun x -> (x, Graph.degree alloc.intrf_g x))
    |> List.sort (fun x y -> (snd x) - (snd y))
  in
  let rec rm_node lis update = match lis with
    | (n, d) :: xs when d < rsum ->
        Graph.rm_node alloc.intrf_g n;
        Stack.push n alloc.stack;
        rm_node xs true
    | _ -> update
  in
  rm_node deg false

(* FIXME *)
let pickup_freeze alloc =
  let lis = sort_by_deg alloc alloc.move_related in
  if List.length lis = 0 then
    None
  else
    Some (lis |> List.hd |> fst)

let coalesce alloc =
  let mplis = Hashtbl.fold (fun x _ l -> x :: l) alloc.move_pair [] in
  let rec col lis = match lis with
    | (dst, src) :: col ->
        if check_coalesce alloc src dst then begin
          exec_coalesce alloc dst src;
          true
        end else
          col xs
    | [] -> false
  in
  col mplis

(* FIXME *)
let pickup_spill alloc =
  let lis = sort_by_deg alloc (Graph.nodes alloc.intrf_g) in
  let lis = List.rev lis in
  List.hd lis

let filter_move_pair f alloc =
  let copy = Hashtbl.copy alloc.move_pair in
  let rm (dst, src) =
    if (f dst) and (f src) then
      ()
    else
      Hashtbl.remove alloc.move_pair (dst, src)
  in
  Hashtbl.iter rm copy

let freeze alloc =
  let fr = pickup_freeze alloc in
  match fr with
    | Some r ->
        filter_move_pair (fun x -> r != x) alloc;
        true
    | None -> false

let spill alloc =
  let r = pickup_spill alloc in
  Hashtbl.add alloc.spilled r ();
  filter_move_pair (fun x -> r != x) alloc

let remove_all_nodes alloc =
  let rec step () =
    let update = simplify alloc in
    let update = update && (coalesce alloc) in
    if update then
      ()
    else begin
      let update = freeze alloc in
      if update then
        ()
      else
        spill alloc
    end
  in
  while 0 < Graph.length (g.intrf_g) do
    step ()
  done

let coloring alloc =
