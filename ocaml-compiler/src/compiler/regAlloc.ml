open Util
open ThreeAddressCodeType
open ThreeAddressCode
open RegsType
open Regs

type t = {
  regs : reg_set;
  intrf_g : reg_type Graph.t;
  proc : (instr_type * string option) Vector.t;
  move_pair : (reg_type * reg_type, unit) Hashtbl.t;
  move_related : (reg_type, unit) Hashtbl.t;
  spilled : reg_table;
  stack : (reg_type * reg_type list * reg_type list) Stack.t;
}

type color_mapping = {
  k : int;
  coloring : (reg_type, int) Hashtbl.t;
}

let make_vreg =
  let offset = 10000 in
  let vreg_slot = SlotNumber.make (fun x -> Virtual (x + offset)) in
  fun () -> SlotNumber.fresh vreg_slot

let get_memreg =
  let slot = SlotNumber.make (fun x -> Virtual (-2 - x)) in
  let tbl = Hashtbl.create 8 in
  let fn s =
    if Hashtbl.mem tbl s then
      Hashtbl.find tbl s
    else begin
      let v = SlotNumber.fresh slot in
      Hashtbl.replace tbl s v;
      v
    end
  in
  fn

let is_memreg r = match r with
  | Virtual x when x <= (-2) -> true
  | _ -> false
 
let list_of_ltbl ltbl =
  ltbl |> fun p -> Hashtbl.fold (fun x y l -> (x, y) :: l) p []
       |> List.sort (fun x y -> (snd x) - (snd y))

let update_state alloc =
  Graph.rm_self_loop alloc.intrf_g;
  let g = alloc.intrf_g in
  let all =
    let all = Graph.all_nodes g in
    let tbl = Hashtbl.create (List.length all) in
    List.iter (fun x -> Hashtbl.add tbl x ()) all;
    tbl
  in
  let mr = Hashtbl.copy alloc.move_related in
  let mp = Hashtbl.copy alloc.move_pair in
  Hashtbl.clear alloc.move_related;
  Hashtbl.clear alloc.move_pair;
  let validate x y =
    Hashtbl.mem all x &&
    Hashtbl.mem all y &&
    Hashtbl.mem mr x &&
    Hashtbl.mem mr y &&
    not (Graph.is_same_group g x y) &&
    not (Graph.has_edge g x y)
  in
  let upd x y =
    if validate x y then begin
      let x = Graph.represent g x in
      let y = Graph.represent g y in
      Hashtbl.add alloc.move_related x ();
      Hashtbl.add alloc.move_related y ();
      Hashtbl.add alloc.move_pair (x, y) ()
    end else
      ()
  in
  Hashtbl.iter (fun (x, y) _ -> upd x y) mp

let insert_callee_save rnum vec =
  let new_vec = Vector.empty () in
  let sv = Vector.empty () in
  let name = snd (Vector.get vec 0) in
  Vector.set vec 0 (fst (Vector.get vec 0), None);
  for i = 0 to rnum - 1 do
    let reg = make_vreg () in
    let instr = Move (reg, CalleeSaved i, -1) in
    Vector.push_back sv reg;
    Vector.push_back new_vec (instr, (if i = 0 then name else None))
  done;
  let pushback e = match e with
    | (Return _, _) ->
        for i = 0 to rnum - 1 do
          let reg = Vector.get sv i in
          let instr = Move (CalleeSaved i, reg, -1) in
          Vector.push_back new_vec (instr, None)
        done;
        Vector.push_back new_vec e
    | _ -> Vector.push_back new_vec e
  in
  Vector.iter pushback vec;
  reset_id new_vec

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

let print_instr vec ltbl =
  print_endline ""; print_endline ""; print_endline "";
  Hashtbl.iter (fun x y -> Printf.printf "%s ---> %d\n" x y) ltbl;
  for i = 0 to (Vector.length vec) - 1 do
    let instr, _ = Vector.get vec i in
    Printf.printf "%d : %s\n" i (string_of_instr instr) 
  done

(* Build interference graph *)
let build regs vec ltbl spilled =
  let g = Graph.make false in
  print_instr vec ltbl;
  let liveness = Liveness.analyze regs vec ltbl is_memreg in
  let mp = Hashtbl.create 8 in
  let add_edges r i =
    let live_out = Liveness.live_out liveness i in
    Hashtbl.iter (fun x _ -> Graph.add_edge g r x) live_out
  in
  let prod t1 t2 =
    let t = Hashtbl.create (Hashtbl.length t1) in
    let add x = if Hashtbl.mem t2 x then Hashtbl.add t x () else () in
    Hashtbl.iter (fun x _ -> add x) t1;
    t
  in
  let saved = List.append regs.caller_saved_regs regs.argument_regs in
  let f instr = match instr with
    | Bind (r, _, i) -> add_edges r i
    | Load (r, _, _, i) -> add_edges r i
    | Call (v, _, i) ->
        let live_in = Liveness.live_in liveness i in
        let live_out = Liveness.live_out liveness i in
        let live = prod live_in live_out in
        List.iter (fun x -> 
          Hashtbl.iter (fun y _ -> Graph.add_edge g x y) live) saved
    | Move (d, s, i) ->
        let live_out = Liveness.live_out liveness i in
        Hashtbl.iter (fun x _-> if x != s then Graph.add_edge g d x else ()) live_out;
        (match (d, s) with (Virtual _, Virtual _) -> Hashtbl.add mp (d, s) () | _ -> ())
    | _ -> ()
  in
  Vector.iter (fun (x, _) -> f x) vec;
  (*
  g |> Graph.all_nodes
    |> List.filter is_memreg
    |> List.iter (Graph.rm_node g);
  *)
  precolor regs g;
  let mp =
    let new_mp = Hashtbl.create 8 in
    let add (x, y) _ = if Graph.has_edge g x y then () else Hashtbl.add new_mp (x, y) () in
    Hashtbl.iter add mp; new_mp
  in
  let res = {
    regs = regs;
    proc = vec;
    intrf_g = g;
    move_pair = mp;
    move_related = Hashtbl.create 0;
    spilled = spilled;
    stack = Stack.create ()
  }
  in
  update_state res;
  res

let briggs_strategy alloc n1 n2 =
  let k = alloc.regs.reg_sum in
  let copy = Graph.copy alloc.intrf_g in
  Graph.contraction copy n1 n2;
  let neig = Graph.succ copy n1 in
  let neig =
    neig |> List.map (fun x -> (x, Graph.degree copy x))
         |> List.filter (fun (_, d) -> k <= d) 
  in
  List.length neig < k

(* memo : george_strategy alloc n1 n2 = george_stragety alloc n2 n1 *)
let george_strategy alloc n1 n2 =
  let k = alloc.regs.reg_sum in
  let g = alloc.intrf_g in
  let neig = Graph.succ g n1 in
  let check x = 
    if Graph.has_edge g x n2 then
      true
    else if Graph.degree g n2 < k then
      true
    else
      false
  in
  neig
  |> List.map check
  |> List.fold_left (fun x y -> x && y) true

let check_coalesce alloc n1 n2 =
  if Graph.has_edge alloc.intrf_g n1 n2 then
    false
  else if briggs_strategy alloc n1 n2 then
    true
  else if george_strategy alloc n1 n2 then
    true
  else
    false

let exec_coalesce alloc dst src =
  Graph.contraction alloc.intrf_g dst src;
  update_state alloc;
  Printf.printf "Coalesce : %s %s\n" (string_of_reg dst) (string_of_reg src)

let sort_by_deg alloc set =
  set |> fun s -> Hashtbl.fold (fun x _ l -> (x, Graph.degree alloc.intrf_g x) :: l) s []
      |> List.sort (fun x y -> (snd x) - (snd y))

let rm_node alloc n =
  let g = alloc.intrf_g in
  let n = Graph.represent g n in
  let adj = Graph.succ g n in
  let grp = Graph.group g n in
  Graph.rm_node g n;
  update_state alloc;
  Stack.push (n, adj, grp) alloc.stack

let simplify alloc =
  let reg_sum = alloc.regs.reg_sum in
  let nodes = Graph.nodes alloc.intrf_g in
  let deg = 
    nodes
    |> List.filter (fun x -> not (Hashtbl.mem alloc.move_related x))
    |> List.filter (fun x -> not (is_precolored alloc x))
    |> List.map (fun x -> (x, Graph.degree alloc.intrf_g x))
    |> List.sort (fun x y -> (snd x) - (snd y))
  in
  let rec aux lis update = match lis with
    | (n, d) :: xs when d < reg_sum ->
        Printf.printf "Remove %s\n" (string_of_reg n);
        rm_node alloc n;
        aux xs true
    | _ -> update
  in
  let res = aux deg false in
  update_state alloc;
  res

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
  else begin
    let r = List.hd lis in
    Printf.printf "Freezed %s\n" (string_of_reg r);
    Some (List.hd lis)
  end

let coalesce alloc =
  let mplis = Hashtbl.fold (fun x _ l -> x :: l) alloc.move_pair [] in
  let rec col lis = match lis with
    | (dst, src) :: xs ->
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
  let g = alloc.intrf_g in
  let r = 
    (Graph.nodes g)
    |> List.filter (fun x -> not (is_precolored alloc x)) 
    |> List.map (fun x -> (x, Graph.degree g x))
    |> List.sort (fun x y -> (snd x) - (snd y))
    |> List.rev
    |> List.hd
    |> fst
  in
  Printf.printf "Potential spill %s\n" (string_of_reg r);
  r

let freeze alloc =
  let fr = pickup_freeze alloc in
  match fr with
    | Some r ->
        Hashtbl.remove alloc.move_related r;
        update_state alloc;
        true
    | None -> false

let spill alloc =
  let r = pickup_spill alloc in
  Hashtbl.add alloc.spilled r ();
  rm_node alloc r

let remove_nodes alloc =
  let step () =
    if simplify alloc then
      ()
    else if coalesce alloc then
      ()
    else if freeze alloc then
      ()
    else
      spill alloc
  in
  while alloc.regs.reg_sum < Graph.length (alloc.intrf_g) do
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
  let k = alloc.regs.reg_sum in
  let ns = 
    alloc.regs.all_regs
    |> List.map (Graph.represent alloc.intrf_g)
    |> fun x -> List.combine x (iota k)
  in
  let coloring = Hashtbl.create k in
  List.iter (fun (x, y) -> Hashtbl.add coloring x y) ns;
  { k; coloring }

let assign_color mapping alloc n =
  Printf.printf "Assign Start %s\n" (string_of_reg n);
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

let restore_node alloc =
  let n, adj, grp = Stack.pop alloc.stack in
  let g = alloc.intrf_g in
  List.iter (Graph.add_node g) grp;
  List.iter (fun x -> if x != n then Graph.contraction g n x else ()) grp;
  List.iter (Graph.add_edge g n) adj;
  n

let coloring alloc =
  let cm = init_cmapping alloc in
  let spilled = Hashtbl.create 0 in
  while not (Stack.is_empty alloc.stack) do
    let n = restore_node alloc in
    let res = assign_color cm alloc n in
    if res then
      Printf.printf "%s was colored to %d\n" (string_of_reg n) (Hashtbl.find cm.coloring n)
    else begin
      Printf.printf "Actual spill %s\n" (string_of_reg n);
      Hashtbl.add spilled n ()
    end
  done;
  (cm, spilled)
  
let extract_reg r = match r with Reg r -> r | _ -> raise (Invalid_argument "extract_reg")

let rewrite_proc seq spill =
  let new_vec = Vector.empty () in
  let was_spilled x = Hashtbl.mem spill x in
  let rec replace_reg v label = match v with
    | Reg r when was_spilled r ->
        let vr = make_vreg () in
        let mr = get_memreg r in
        Vector.push_back new_vec (Bind (vr, Reg mr, -1), label);
        (Reg vr, None)
    | PrimCall (p, vl) ->
        let rec aux lis label = match lis with
          | x :: xs ->
              let x, label = replace_reg x label in
              let xs, label = aux xs label in
              (x :: xs, label)
          | [] -> ([], label)
        in
        let vl, label = aux vl label in
        (PrimCall (p, vl), label)
    | _ -> (v, label)
  in
  let rewrite_instr instr label = match instr with
    | Bind (r, s, _) ->
        let s, label = replace_reg s label in
        if was_spilled r then
          Vector.push_back new_vec (Bind (get_memreg r, s, -1), label)
        else
          (match s with
            | Reg s -> Vector.push_back new_vec (Move (r, s, -1), label)
            | _ -> Vector.push_back new_vec (Bind (r, s, -1), label))
    | Move (d, s, _) ->
        let s, label = replace_reg (Reg s) label in
        if was_spilled d then
          Vector.push_back new_vec (Bind (get_memreg d, s, -1), label)
        else
          (match s with
            | Reg s -> Vector.push_back new_vec (Move (d, s, -1), label)
            | _ -> Vector.push_back new_vec (Bind (d, s, -1), label))
    | Test (a, b, _) ->
        let a, label = replace_reg a label in
        let b, label = replace_reg b label in
        Vector.push_back new_vec (Test (a, b, -1), label)
    | Jump (a, _) ->
        let a, label = replace_reg a label in
        Vector.push_back new_vec (Jump (a, -1), label)
    | Call (a, b, _) ->
        let a, label = replace_reg a label in
        Vector.push_back new_vec (Call (a, b, -1), label)
    | Return i -> Vector.push_back new_vec (Return (-1), label)
    | Load (dst, base, i, _) ->
      let base, label = replace_reg (Reg base) label in
      let base = extract_reg base in
      if was_spilled dst then begin
        let mem = get_memreg dst in
        let v = make_vreg () in
        Vector.push_back new_vec (Load (v, base, i, -1), label);
        Vector.push_back new_vec (Bind (mem, Reg v, -1), None)
      end else begin
        Vector.push_back new_vec (Load (dst, base, i, -1), label)
      end
    | Store (base, src, i, _) ->
        let base, label = replace_reg (Reg base) label in
        let base = extract_reg base in
        if was_spilled src then begin
          let mem = get_memreg src in
          let v = make_vreg () in
          Vector.push_back new_vec (Bind (v, Reg mem, -1), label);
          Vector.push_back new_vec (Store (base, v, i, -1), None)
        end else begin
          Vector.push_back new_vec (Store (base, src, i, -1), label);
        end
  in
  let rec rewrite i =
    if i = Vector.length seq then
      ()
    else begin
      let instr, label = Vector.get seq i in
      rewrite_instr instr label;
      rewrite (i + 1)
    end
  in
  rewrite 0;
  reset_id new_vec

let dump_colormap cm =
  Hashtbl.fold (fun x y l -> (x, y) :: l) cm.coloring []
  |> List.map (fun (x, y) -> Printf.sprintf "[ %s ] ---> %d" (string_of_reg x) y)
  |> String.concat "\n"

let dump_spilled sp =
  Hashtbl.fold (fun x _ l -> x :: l) sp []
  |> List.map string_of_reg
  |> String.concat " "
  |> Printf.sprintf "[ %s ]"

let allocate_func_aux vec ltbl regs =
  let cm = ref None in
  let alloc = ref None in
  let vec = ref vec in
  let ltbl = ref ltbl in
  let spilled = ref (Hashtbl.create 0) in
  let init = ref true in
  let body () =
    if (not !init) && Hashtbl.length !spilled = 0 then
      ()
    else begin
    let new_alloc = build regs !vec !ltbl !spilled in
    init := false;
    remove_nodes new_alloc;
    let new_cm, new_spilled = coloring new_alloc in
    let new_vec, new_ltbl = rewrite_proc !vec new_spilled in
    cm := Some new_cm;
    alloc := Some new_alloc;
    spilled := new_spilled;
    vec := new_vec;
    ltbl := new_ltbl
  end
  in
  let rec loop () =
    if (not !init) && Hashtbl.length !spilled = 0 then
      ()
    else begin
      body ();
      loop ()
    end
  in
  loop ();
  (Option.get !alloc, Option.get !cm, !vec, !ltbl)

let remove_redundant vec =
  let rec aux pl lis = match lis with
    | x :: xs ->
        let instr, label = x in
        let label = match (pl, label) with
          | (Some _, None) -> pl
          | (None, Some _) -> label
          | (None, None) -> None
          | _ -> raise (Invalid_argument "Labeling")
        in
        (match instr with
          | Move (a, b, _) when a = b -> aux label xs
          | _ -> (instr, label) :: (aux None xs))
    | [] -> []
  in
  vec |> Vector.list_of_vector
      |> aux None
      |> Vector.vector_of_list

let allocate_func seq ltbl regs =
  let alloc, cm, seq, ltbl = allocate_func_aux seq ltbl regs in
  let reg_of_color =
    let all = regs.all_regs in
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
    if is_memreg r then get_memreg r else reg_of_color (color r)
  in
  let rec rewrite_if_reg v = match v with
    | Reg r -> Reg (act_reg r)
    | PrimCall (p, l) -> PrimCall (p, List.map rewrite_if_reg l)
    | _ -> v
  in
  let rec rewrite_vec i new_vec =
    if i = Vector.length seq then
      new_vec
    else begin
      let instr, label = Vector.get seq i in
      let instr = match instr with
        | Bind (r, v, i) -> Bind (act_reg r, rewrite_if_reg v, i)
        | Move (d, s, i) -> Move (act_reg d, act_reg s, i)
        | Test (a, b, i) -> Test (rewrite_if_reg a, rewrite_if_reg b, i)
        | Jump (a, i) -> Jump (rewrite_if_reg a, i)
        | Call (a, b, i) -> Call (rewrite_if_reg a, b, i)
        | Return i -> Return i
        | Load (r, v, offset, i) -> Load (act_reg r, extract_reg (rewrite_if_reg (Reg v)), offset, i)
        | Store (r, v, offset, i) -> Store (act_reg r, extract_reg (rewrite_if_reg (Reg v)), offset, i)
      in
      Vector.push_back new_vec (instr, label);
      rewrite_vec (i + 1) new_vec
    end
  in
  let new_vec = rewrite_vec 0 (Vector.empty ()) in
  reset_id (remove_redundant new_vec)

let allocate_experiment reg_num program =
  let regs = make_reg_set reg_num in
  let { signature; ltbl; seq } = program in
  let callee_sn = List.length regs.callee_saved_regs in
  let seq, ltbl = insert_callee_save callee_sn seq in
  let seq, ltbl = allocate_func seq ltbl regs in
  { signature; ltbl; seq }

let split_program funcs seq =
  let rec aux cur res rem = match rem with
    | ((_, Some l) as x) :: xs ->
        let cur = x :: cur in
        if Hashtbl.mem funcs l then
          aux [] (cur :: res) xs
        else
          aux cur res xs
    | x :: xs -> aux (x :: cur) res xs
    | [] -> res
  in
  seq 
  |> Vector.list_of_vector
  |> List.rev
  |> aux [] []
  |> List.map Vector.vector_of_list

let allocate reg_num program =
  let regs = make_reg_set reg_num in
  let callee_sn = List.length regs.callee_saved_regs in
  let { signature; ltbl; seq } = program in
  let allocate_aux v =
    let v, ltbl = insert_callee_save callee_sn v in
    let v, _ = allocate_func v ltbl regs in
    v
  in
  let append v1 v2 =
    let res = Vector.empty () in
    Vector.iter (fun x -> Vector.push_back res x) v1;
    Vector.iter (fun x -> Vector.push_back res x) v2;
    res
  in
  let funcs =
    let tbl = Hashtbl.create (Hashtbl.length signature) in
    Hashtbl.iter (fun x _ -> Hashtbl.add tbl x ()) signature;
    tbl
  in
  let seq, ltbl =
    seq
    |> split_program funcs 
    |> List.map allocate_aux
    |> List.fold_left (fun x y -> append x y) (Vector.empty ())
    |> reset_id
  in
  { signature; ltbl; seq }
