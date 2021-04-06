open Util
open ThreeAddressCode
open ThreeAddressCodeType

type actual_reg_set = {
  reg_num : int * int * int;
  rsum : int;
  caller_save_regs : reg_type list;
  callee_save_regs : reg_type list;
  argument_regs : reg_type list;
  all_regs : reg_type list;
}

type t = {
  regs : actual_reg_set;
  intrf_g : reg_type Graph.t;
  proc : instr_type Vector.t;
  move_pair : (reg_type * reg_type, unit) Hashtbl.t;
  move_related : (reg_type, int) Hashtbl.t;
  freezed : reg_set;
  spilled : reg_set;
  stack : reg_type Stack.t;
}

type color_mapping = {
  k : int;
  coloring : (reg_type, int) Hashtbl.t;
}

let offset = 10000
let vreg_slot = SlotNumber.make (fun x -> Virtual (x + offset))
let dummy_reg = Virtual (-1)
let mem_reg = Virtual (-2)
  
let list_of_ptbl ptbl =
  ptbl |> fun p -> Hashtbl.fold (fun x y l -> (x, y) :: l) p []
       |> List.sort (fun x y -> (snd x) - (snd y))
  
let make_act_regs reg_num =
  let caller_save, callee_save, args = reg_num in
  let rsum =
    let sum3 (a, b, c) = a + b + c in
    sum3 reg_num
  in
  let rec make i len cnstr =
    if i = len then [] else (cnstr i) :: (make (i + 1) len cnstr)
  in
  let caller_save_regs = make 0 caller_save (fun x -> CallerSave x) in
  let callee_save_regs = make 0 callee_save (fun x -> CalleeSave x) in
  let argument_regs = make 0 args (fun x -> Argument x) in
  let all_regs = List.flatten [ caller_save_regs; callee_save_regs; argument_regs ] in
  { 
    reg_num; 
    rsum; 
    caller_save_regs; 
    callee_save_regs; 
    argument_regs;
    all_regs
  }

let reset_id vec ptbl =
  let new_vec = Vector.empty () in
  let new_ptbl = Hashtbl.create 0 in
  let rtbl = 
    let res = Hashtbl.create (Hashtbl.length ptbl) in
    Hashtbl.iter (fun x y -> Hashtbl.replace res y x) ptbl;
    res
  in
  let rewrite_tbl instr i =
    let id = get_instr_id instr in
    if Hashtbl.mem rtbl id then begin
      let s = Hashtbl.find rtbl id in
      Hashtbl.add new_ptbl s i
    end else
      ()
  in
  for i = 0 to (Vector.length vec) - 1 do
    let instr = Vector.get vec i in
    rewrite_tbl instr i;
    Vector.push_back new_vec (replace_id instr i)
  done;
  (new_vec, new_ptbl)

let split_program (sigtbl, ptbl, vec) =
  let funcs =
    sigtbl
    |> fun s -> Hashtbl.fold (fun x _ l -> x :: l) s []
    |> List.map (fun x -> (x, Hashtbl.find ptbl x))
    |> List.sort (fun x y -> (snd x) - (snd y))
  in
  let order, idxlis = List.split funcs in
  let rec split idx idxlis new_vec =
    if idx = Vector.length vec then
      [ new_vec ]
    else begin
      match idxlis with
        | x :: xs when x = idx ->
            new_vec :: (split idx xs (Vector.empty ()))
        | _ ->
            Vector.push_back new_vec (Vector.get vec idx);
            split (idx + 1) idxlis new_vec
    end
  in
  let flis = split 0 idxlis (Vector.empty ()) in
  let rec reset names bodies = match (names, bodies) with
    | (x :: xs, y :: ys) ->
        let y, z = reset_id x ptbl in
        (x, y, z) :: (reset xs ys)
    | ([], []) -> []
    | _ -> raise (Invalid_argument ("reset id"))
  in
  reset order flis 

let insert_caller_save rnum vec ptbl =
  let new_vec = Vector.empty () in
  let new_ptbl = Hashtbl.create (Hashtbl.length ptbl) in
  let sv = Vector.empty () in
  for i = 0 to rnum - 1 do
    let reg = SlotNumber.fresh vreg_slot in
    let instr = Move (reg, Reg (CallerSave i), i) in
    Vector.push_back sv reg;
    Vector.push_back new_vec instr
  done;
  let aux instr =
    let id = get_instr_id instr in
    let id = id + rnum in
    replace_id instr id
  in
  Vector.iter (fun x -> Vector.push_back new_vec (aux x)) vec;
  let len = Vector.length new_vec in
  for i = 0 to rnum - 1 do
    let reg = Vector.get sv i in
    let instr = Move (CallerSave i, Reg reg, i + len) in
    Vector.push_back new_vec instr
  done;
  Hashtbl.iter (fun x y -> Hashtbl.add new_ptbl x (y + rnum)) ptbl;
  (new_vec, new_ptbl)

let precolor ras g =
  let rec add x lis = match lis with
    | y :: ys -> Graph.add_edge g x y; add x ys
    | [] -> ()
  in
  let rec loop lis = match lis with
    | x :: xs -> add x xs; loop xs
    | [] -> ()
  in
  loop ras.all_regs

let is_precolored alloc n =
  let g = alloc.intrf_g in
  let rec aux lis = match lis with
    | x :: xs ->
        if Graph.is_same_group g x n then true else aux xs
    | [] -> false
  in
  aux alloc.regs.all_regs

(* Build interference graph *)
let build art vec ptbl spilled =
  let g = Graph.make false in
  let liveness = Liveness.analyze vec ptbl in
  let mp = Hashtbl.create 8 in
  let add_edges r i =
    let live_out = Liveness.live_out liveness i in
    Hashtbl.iter (fun x _ -> Graph.add_edge g r x) live_out
  in
  let f instr = match instr with
    | Load (r, _, _, i) -> add_edges r i
    | PrimCall (r, _, _, i) -> add_edges r i
    | Move (lhs, s, i) ->
        let rhs = match s with
            | Reg reg -> reg
            | _ -> dummy_reg
        in
        let live_out = Liveness.live_out liveness i in
        Hashtbl.iter (fun x _-> if x != rhs then Graph.add_edge g lhs x else ()) live_out;
    | _ -> ()
  in
  precolor art g;
  Graph.rm_node g dummy_reg;
  let mp =
    let new_mp = Hashtbl.create 8 in
    let add (x, y) _ = if Graph.has_edge g x y then () else Hashtbl.add new_mp (x, y) () in
    Hashtbl.iter add mp; new_mp
  in
  let mr =
    let tbl = Hashtbl.create (Hashtbl.length mp) in
    let add x =
      let y = if Hashtbl.mem tbl x then Hashtbl.find tbl x else 0 in
      Hashtbl.replace tbl x (y + 1)
    in
    Hashtbl.iter (fun (x, y) _ -> begin add x; add y end) mp; tbl
  in
  {
    regs = art;
    proc = vec;
    intrf_g = g;
    move_pair = mp;
    move_related = mr;
    freezed = Hashtbl.create 2;
    spilled = spilled;
    stack = Stack.create ()
  }

(* FIXME *)
let check_coalesce alloc n1 n2 =
  if Graph.has_edge alloc.intrf_g n1 n2 then
    false
  else begin
    let upper = alloc.regs.rsum in
    let tbl = Hashtbl.create upper in
    let add s = List.iter (fun x -> Hashtbl.replace tbl x ()) s in
    add (Graph.succ alloc.intrf_g n1);
    add (Graph.succ alloc.intrf_g n2);
    Hashtbl.length tbl < upper
  end

let rm_move_pair alloc dst src =
  Hashtbl.remove alloc.move_pair (dst, src);
  let dec x =
    let y = Hashtbl.find alloc.move_related x in
    let y = y - 1 in
    if y = 0 then
      Hashtbl.remove alloc.move_related x
    else
      Hashtbl.replace alloc.move_related x y
  in
  dec dst; dec src

let exec_coalesce alloc dst src =
  Graph.contraction alloc.intrf_g dst src;
  rm_move_pair alloc dst src

let sort_by_deg alloc set =
  set |> fun s -> Hashtbl.fold (fun x _ l -> (x, Graph.degree alloc.intrf_g x) :: l) s []
      |> List.sort (fun x y -> (snd x) - (snd y))

let simplify alloc =
  let rsum = alloc.regs.rsum in
  let nodes = Graph.nodes alloc.intrf_g in
  let deg = 
    nodes
    |> List.filter (fun x -> not (Hashtbl.mem alloc.move_related x))
    |> List.filter (fun x -> not (is_precolored alloc x))
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
  let lis = 
    alloc.move_related
    |> sort_by_deg alloc
    |> List.split
    |> fst
    |> List.filter (fun x -> not (is_precolored alloc x))
  in
  if List.length lis = 0 then
    None
  else
    Some (List.hd lis)

let rebuild_move_pair alloc =
  let should_remove a b = Graph.has_edge alloc.intrf_g a b in
  let mp = alloc.move_pair in
  let rmlis = Hashtbl.fold (fun (a, b) _ l -> if should_remove a b then (a, b) :: l else l) mp [] in
  List.iter (fun (x, y) -> rm_move_pair alloc x y) rmlis

let coalesce alloc =
  let mplis = Hashtbl.fold (fun x _ l -> x :: l) alloc.move_pair [] in
  let rec col lis = match lis with
    | (dst, src) :: xs ->
        if check_coalesce alloc src dst then begin
          exec_coalesce alloc dst src;
          rebuild_move_pair alloc;
          true
        end else
          col xs
    | [] -> false
  in
  col mplis

(* FIXME *)
let pickup_spill alloc =
  let g = alloc.intrf_g in
  (Graph.nodes g)
  |> List.map (fun x -> (x, Graph.degree g x))
  |> List.sort (fun x y -> (snd x) - (snd y))
  |> List.rev
  |> List.hd
  |> fst

let filter_move_pair f alloc =
  let mp = Hashtbl.copy alloc.move_pair in
  let rmlis = Hashtbl.fold (fun (x, y) _ l -> if (f x) && (f y) then l else (x, y) :: l) mp [] in
  List.iter (fun (x, y) -> rm_move_pair alloc x y) rmlis

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

let remove_nodes alloc =
  let step () =
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
  while alloc.regs.rsum < Graph.length (alloc.intrf_g) do
    step ()
  done

let pre_coalesce alloc =
  let mp_lis = Hashtbl.fold (fun x _ l -> x :: l) alloc.move_pair [] in
  let try_coalesce (dst, src) =
    let ok = true in
    let ok = ok && (Hashtbl.mem alloc.spilled dst) in
    let ok = ok && (Hashtbl.mem alloc.spilled src) in
    if ok then
      exec_coalesce alloc dst src
    else
      ()
  in
  let rec aux lis = match lis with
    | x :: xs ->
        try_coalesce x;
        aux xs
    | [] -> ()
  in
  aux mp_lis
 
let iota n =
  let rec aux i = if i = n then [] else i :: (aux (i + 1)) in
  aux 0

let init_cmapping alloc =
  let k = alloc.regs.rsum in
  let ns = 
    alloc.regs.all_regs
    |> List.map (Graph.represent alloc.intrf_g)
    |> fun x -> List.combine x (iota k)
  in
  let coloring = Hashtbl.create k in
  List.iter (fun (x, y) -> Hashtbl.add coloring x y) ns;
  { k; coloring }

let assign_color mapping alloc n =
  let adj =
    let succ = 
      n |> Graph.succ alloc.intrf_g
        |> List.filter (Hashtbl.mem mapping.coloring)
        |> List.map (Hashtbl.find mapping.coloring)
    in
    let res = Hashtbl.create (List.length succ) in
    List.iter (fun c -> Hashtbl.replace res c ()) succ;
    res
  in
  let rec aux i =
    if i = mapping.k then
      None
    else if Hashtbl.mem adj i then
      aux (i + 1)
    else
      Some i
  in
  match aux 0 with
    | Some c ->
        Hashtbl.add mapping.coloring n c; true
    | None -> false

let coloring alloc =
  let cm = init_cmapping alloc in
  let spilled = Hashtbl.create 0 in
  while not (Stack.is_empty alloc.stack) do
    let n = Stack.pop alloc.stack in
    let res = assign_color cm alloc n in
    if res then () else Hashtbl.add spilled n ()
  done;
  (cm, spilled)

type replace_reg_type = {
  def : reg_type list;
  use : reg_type list;
  replace : (reg_type * reg_type) list;
}

let make_rrt def use =
  let lis = List.append def use in
  let rec aux lis = match lis with
    | x :: xs -> (x, SlotNumber.fresh vreg_slot) :: (aux xs)
    | [] -> []
  in
  { def; use; replace = aux lis }

let convert_reg rrt r =
  let rec aux lis = match lis with
    | (x, y) :: xs -> if x = r then y else aux xs
    | [] -> r
  in
  aux rrt.replace

let rewrite_proc vec ptbl spill =
  let new_vec = Vector.empty () in
  let ptbl_lis = list_of_ptbl ptbl in
  let new_ptbl = Hashtbl.create (Hashtbl.length ptbl) in
  let rewrite_ptbl idx sum = match ptbl_lis with
    | (x, y) :: xs when y = idx ->
        Hashtbl.add new_ptbl x (y + sum);
        xs
    | _ -> ptbl_lis
  in
  let was_spilled x = Hashtbl.mem spill x in
  let extract_regs lis =
    let rec aux lis = match lis with
      | (Reg x) :: xs -> x :: (aux xs)
      | _ :: xs -> aux xs
      | [] -> []
    in
    aux lis
  in
  let make instr = 
    let def, use = match instr with
      | Move (r, s, _) -> ([ r ], extract_regs [ s ])
      | Test (r1, r2, _) -> ([], extract_regs [ r1; r2 ])
      | Jump (r, _) -> ([], extract_regs [ r ])
      | Load (r, s, _, _) -> ([ r ], extract_regs [ s ])
      | Store (r, s, _, _) -> ([], r :: (extract_regs [ s ]))
      | PrimCall (r, _, l, _) -> ([ r ], extract_regs l)
      | _ -> ([], [])
    in
    make_rrt def use
  in
  let push_lw idx sum rrt =
    let rec aux i l = match l with
      | x :: xs -> 
          let load = Load (x, Reg mem_reg, 0, i + idx + sum) in
          Vector.push_back new_vec load;
          aux (i + 1) xs
      | [] -> sum + i
    in
    aux 0 rrt.use
  in
  let push_sw idx sum rrt =
    let rec aux i l = match l with
      | x :: xs -> 
          let store = Store (mem_reg, Reg x, 0, i + idx + sum) in
          Vector.push_back new_vec store;
          aux (i + 1) xs
      | [] -> sum + i
    in
    aux 0 rrt.def
  in
  let rewrite_instr instr idx sum =
    let rrt = make instr in
    let sum = push_lw idx sum rrt in
    let cr1 r = convert_reg rrt r in
    let cr2 v = match v with
      | Reg x -> Reg (cr1 x)
      | _ -> v
    in
    let id = idx + sum in
    let instr = match instr with
      | Move (r, s, _) -> Move (cr1 r, cr2 s, id)
      | Test (v1, v2, _) -> Test (cr2 v1, cr2 v2, id)
      | Jump (v, _) -> Jump (cr2 v, id)
      | Return _ -> Return id
      | Load (r, v, i, _) -> Load (cr1 r, cr2 v, i, id)
      | Store (r, v, i, _) -> Store (cr1 r, cr2 v, i, id)
      | PrimCall (r, p, vl, _) -> PrimCall (cr1 r, p, List.map cr2 vl, id)
    in
    Vector.push_back new_vec instr;
    push_sw (idx + 1) sum rrt
  in
  let rec loop idx sum plis =
    let plis = rewrite_ptbl idx sum in
    if idx = Vector.length vec then
      ()
    else begin
      let instr = Vector.get vec idx in
      let sum = rewrite_instr instr idx sum in
      let idx = idx + 1 in
      loop idx sum plis
    end
  in
  loop 0 0 ptbl_lis;
  (new_vec, new_ptbl)

let allocate_func_aux vec ptbl art =
  let cm = ref None in
  let alloc = ref None in
  let vec = ref vec in
  let ptbl = ref ptbl in
  let spilled = ref (Hashtbl.create 0) in
  let init = ref true in
  let rec loop () =
    if (not !init) && Hashtbl.length !spilled = 0 then
      ()
    else begin
      init := true;
      let new_alloc = build art !vec !ptbl !spilled in
      remove_nodes new_alloc;
      let new_cm, new_spilled = coloring new_alloc in
      let new_vec, new_ptbl = rewrite_proc !vec !ptbl new_spilled in
      cm := Some new_cm;
      alloc := Some new_alloc;
      spilled := new_spilled;
      vec := new_vec;
      ptbl := new_ptbl;
      loop ()
    end
  in
  loop ();
  (Option.get !alloc, Option.get !cm, !vec, !ptbl)

let allocate_func vec ptbl art =
  let alloc, cm, vec, ptbl = allocate_func_aux vec ptbl art in
  let reg_of_color =
    let all = art.all_regs in
    let color =
      all |> List.map (Graph.represent alloc.intrf_g)
          |> List.map (Hashtbl.find cm.coloring)
    in
    let lis = List.combine color all in
    let tbl = Hashtbl.create (List.length lis) in
    List.iter (fun (x, y) -> Hashtbl.add tbl x y) lis;
    fun r -> Hashtbl.find tbl r
  in
  let color r =
    let r = Graph.represent alloc.intrf_g r in
    Hashtbl.find cm.coloring r
  in
  let act_reg r = 
    if r = mem_reg then mem_reg else reg_of_color (color r) 
  in
  let new_vec = Vector.empty () in
  let ptbl = list_of_ptbl ptbl in
  let new_ptbl = Hashtbl.create (List.length ptbl) in
  let rec rewrite_ptbl idx sum lis = match lis with
    | (s, i) :: xs when i <= idx + sum ->
        Hashtbl.add new_ptbl s (idx + sum);
        rewrite_ptbl idx sum xs
    | _ -> lis
  in
  let rewrite_if_reg v = match v with
    | Reg r -> Reg (act_reg r)
    | _ -> v
  in
  let rec rewrite_vec idx sum plis =
    if idx = Vector.length vec then
      ()
    else begin
      let plis = rewrite_ptbl idx sum plis in
      let instr = Vector.get vec idx in
      let id = idx + sum in
      let tail ip =
        Vector.push_back new_vec ip;
        rewrite_vec (idx + 1) sum plis
      in
      match instr with
        | Move (s, r, _) ->
            let s = act_reg s in
            let r = rewrite_if_reg r in
            if (Reg s) = r then
              rewrite_vec (idx + 1) (sum - 1) plis
            else
              tail (Move (s, r, id))
        | Test (a, b, _) ->
            let a = rewrite_if_reg a in
            let b = rewrite_if_reg b in
            tail (Test (a, b, id))
        | Jump (a, _) ->
            let a = rewrite_if_reg a in
            Vector.push_back new_vec (Jump (a, id));
            tail (Jump (a, id))
        | Return _ -> tail (Return id)
        | Load (a, b, i, _) ->
            let a = act_reg a in
            let b = rewrite_if_reg b in
            tail (Load (a, b, i, id))
        | Store (a, b, i, _) ->
            let a = act_reg a in
            let b = rewrite_if_reg b in
            tail (Store (a, b, i, id))
        | PrimCall (a, p, lis, _) ->
            let a = act_reg a in
            let lis = List.map rewrite_if_reg lis in
            tail (PrimCall (a, p, lis, id))
    end
  in
  rewrite_vec 0 0 ptbl;
  (new_vec, new_ptbl)

let allocate_experiment reg_num vec ptbl =
  let art = make_act_regs reg_num in
  let caller_sn = List.length art.caller_save_regs in
  let vec, ptbl = insert_caller_save caller_sn vec ptbl in
  let ptbl =
    let lis = Hashtbl.fold (fun x y l -> (x, y) :: l) ptbl [] in
    Hashtbl.clear ptbl;
    List.iter (fun (x, y) -> Hashtbl.add ptbl x y) lis;
    ptbl
  in
  allocate_func vec ptbl art

(*
 (* FIXME : remove function from ptbl *)
let allocate program reg_num =
  let art = make_act_regs reg_num in
  let sigtbl, _, _ = program in
  let names, procs =
    let caller_save_num = List.length art.caller_save_regs in
    program 
    |> split_program
    |> List.map (fun (x, y, z) -> (x, (insert_caller_save caller_save_num y, z, art)))
    |> List.split
  in
  let procs = List.map (fun (x, y, z) -> allocate_func x y z) procs in
*)
