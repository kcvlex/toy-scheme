open Util
open ThreeAddressCode
open ThreeAddressCodeType

type t = {
  regs : reg_set;
  intrf_g : reg_type Graph.t;
  proc : (instr_type * string option) Vector.t;
  move_pair : (reg_type * reg_type, unit) Hashtbl.t;
  move_related : (reg_type, int) Hashtbl.t;
  freezed : reg_table;
  spilled : reg_table;
  stack : (reg_type * reg_type list * reg_type list) Stack.t;
}

type color_mapping = {
  k : int;
  coloring : (reg_type, int) Hashtbl.t;
}

let offset = 10000
let vreg_slot = SlotNumber.make (fun x -> Virtual (x + offset))
let dummy_reg = Virtual (-1)
let mem_reg = Virtual (-2)
  
let list_of_label_tbl label_tbl =
  label_tbl |> fun p -> Hashtbl.fold (fun x y l -> (x, y) :: l) p []
       |> List.sort (fun x y -> (snd x) - (snd y))
  

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

let insert_callee_save rnum vec =
  let new_vec = Vector.empty () in
  let sv = Vector.empty () in
  let name = snd (Vector.get vec 0) in
  Vector.set vec 0 (fst (Vector.get vec 0), None);
  for i = 0 to rnum - 1 do
    let reg = SlotNumber.fresh vreg_slot in
    let instr = Move (reg, Reg (CalleeSaved i), -1) in
    Vector.push_back sv reg;
    Vector.push_back new_vec (instr, (if i = 0 then name else None))
  done;
  let pushback e = match e with
    | (Return _, _)->
        for i = 0 to rnum - 1 do
          let reg = Vector.get sv i in
          let instr = Move (CalleeSaved i, Reg reg, -1) in
          Vector.push_back new_vec (instr, None)
        done;
        Vector.push_back new_vec e
    | _ -> Vector.push_back new_vec e
  in
  Vector.iter pushback vec;
  reset_id new_vec

let split_program program =
  let { signature; label_tbl; seq } = program in
  let funcs =
    signature
    |> fun s -> Hashtbl.fold (fun x _ l -> x :: l) s []
    |> List.map (fun x -> (x, Hashtbl.find label_tbl x))
    |> List.sort (fun x y -> (snd x) - (snd y))
  in
  let order, idxlis = List.split funcs in
  let rec aux i idxlis vec =
    if i = Vector.length seq then
      [ vec ]
    else begin
      let instr = Vector.get seq i in
      match idxlis with
        | x :: xs when x = i ->
            let hd = Vector.copy vec in
            let vec = Vector.empty () in
            let idxlis = xs in
            Vector.push_back vec instr;
            hd :: (aux (i + 1) idxlis vec)
        | _ ->
            Vector.push_back vec instr;
            aux (i + 1) idxlis vec
    end
  in
  let seq_per_func = 
    let s = aux 0 idxlis (Vector.empty ()) in
    s |> List.tl
      |> List.map reset_id
  in
  (order, seq_per_func)

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

let print_instr vec label_tbl =
  print_endline ""; print_endline ""; print_endline "";
  Hashtbl.iter (fun x y -> Printf.printf "%s ---> %d\n" x y) label_tbl;
  for i = 0 to (Vector.length vec) - 1 do
    let instr, _ = Vector.get vec i in
    Printf.printf "%d : %s\n" i (string_of_instr instr) 
  done

(* Build interference graph *)
let build regs vec label_tbl spilled =
  let g = Graph.make false in
  Graph.add_node g dummy_reg;
  print_instr vec label_tbl;
  let liveness = Liveness.analyze regs vec label_tbl in
  print_endline ""; print_endline (Liveness.dump liveness);
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
  Vector.iter (fun (x, _) -> f x) vec;
  precolor regs g;
  Graph.rm_node g dummy_reg;
  print_endline ""; print_endline (Graph.dump g string_of_reg);
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
    regs = regs;
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
    let upper = alloc.regs.reg_sum in
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

let rm_self_loop alloc =
  let nodes = Graph.nodes alloc.intrf_g in
  List.iter (fun x -> Graph.rm_edge alloc.intrf_g x x) nodes

let exec_coalesce alloc dst src =
  Graph.contraction alloc.intrf_g dst src;
  Printf.printf "Coalesce : %s %s" (string_of_reg dst) (string_of_reg src);
  rm_self_loop alloc;
  rm_move_pair alloc dst src

let sort_by_deg alloc set =
  set |> fun s -> Hashtbl.fold (fun x _ l -> (x, Graph.degree alloc.intrf_g x) :: l) s []
      |> List.sort (fun x y -> (snd x) - (snd y))

let rm_node alloc n =
  let g = alloc.intrf_g in
  let adj = Graph.succ g n in
  let grp = Graph.group g n in
  Graph.rm_node g n;
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
  rm_self_loop alloc;
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
  rm_node alloc r;
  filter_move_pair (fun x -> r != x) alloc

let remove_nodes alloc =
  let step () =
    let update = simplify alloc in
    let update = update || (coalesce alloc) in
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
    Printf.printf "Adj : [ %s ]\n" (String.concat " " (List.map string_of_int succ));
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
  List.iter (Graph.contraction g n) grp;
  List.iter (Graph.add_edge g n) adj;
  n

let coloring alloc =
  let cm = init_cmapping alloc in
  let spilled = Hashtbl.create 0 in
  while not (Stack.is_empty alloc.stack) do
    let n = restore_node alloc in
    let res = assign_color cm alloc n in
    if res then () else Hashtbl.add spilled n ()
  done;
  (cm, spilled)

let rewrite_proc seq spill =
  let new_vec = Vector.empty () in
  let was_spilled x = Hashtbl.mem spill x in
  let replace_reg v label = match v with
    | Reg r when was_spilled r ->
        let vr = SlotNumber.fresh vreg_slot in
        Vector.push_back new_vec (Load (vr, Reg mem_reg, 0, -1), label);
        (Reg vr, None)
    | _ -> (v, label)
  in
  let rewrite_instr instr label = match instr with
    | Move (r, s, i) ->
        let s, label = replace_reg s label in
        if was_spilled r then
          Vector.push_back new_vec (Store (mem_reg, s, 0, i), label)
        else
          Vector.push_back new_vec (Move (r, s, i), label)
    | Test (a, b, i) ->
        let a, label = replace_reg a label in
        let b, label = replace_reg b label in
        Vector.push_back new_vec (Test (a, b, i), label)
    | Jump (a, i) ->
        let a, label = replace_reg a label in
        Vector.push_back new_vec (Jump (a, i), label)
    | Return i -> Vector.push_back new_vec (Return i, label)
    | Load (dst, base, i, j) ->
      let base, label = replace_reg base label in
      if was_spilled dst then begin
        let dst = SlotNumber.fresh vreg_slot in
        Vector.push_back new_vec (Load (dst, base, i, j), label);
        Vector.push_back new_vec (Store (dst, Reg mem_reg, 0, -1), None)
      end else begin
        Vector.push_back new_vec (Load (dst, base, i, j), label)
      end
    | Store (src, base, i, j) ->
        let base, label = replace_reg base label in
        if was_spilled src then begin
          let src = SlotNumber.fresh vreg_slot in
          Vector.push_back new_vec (Load (src, Reg mem_reg, 0, -1), label);
          Vector.push_back new_vec (Store (src, base, i, j), None)
        end else begin
          Vector.push_back new_vec (Store (src, base, i, j), label);
        end
    | PrimCall (r, p, vl, i) ->
        let rec aux lis label = match lis with
          | x :: xs ->
              let x, label = replace_reg x label in
              let xs, label = aux xs label in
              (x :: xs, label)
          | [] -> ([], label)
        in
        let vl, label = aux vl label in
        if was_spilled r then begin
          let r = SlotNumber.fresh vreg_slot in
          Vector.push_back new_vec (PrimCall (r, p, vl, i), label);
          Vector.push_back new_vec (Store (r, Reg mem_reg, 0, -1), None);
        end else begin
          Vector.push_back new_vec (PrimCall (r, p, vl, i), label)
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

let allocate_func_aux vec label_tbl regs =
  let cm = ref None in
  let alloc = ref None in
  let vec = ref vec in
  let label_tbl = ref label_tbl in
  let spilled = ref (Hashtbl.create 0) in
  let init = ref true in
  let body () =
    if (not !init) && Hashtbl.length !spilled = 0 then
      ()
    else begin
    let new_alloc = build regs !vec !label_tbl !spilled in
    init := false;
    remove_nodes new_alloc;
    let new_cm, new_spilled = coloring new_alloc in
    print_endline ""; print_endline (dump_colormap new_cm);
    print_endline ""; print_endline (dump_spilled new_spilled);
    let new_vec, new_label_tbl = rewrite_proc !vec new_spilled in
    cm := Some new_cm;
    alloc := Some new_alloc;
    spilled := new_spilled;
    vec := new_vec;
    label_tbl := new_label_tbl
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
  (Option.get !alloc, Option.get !cm, !vec, !label_tbl)

let allocate_func seq label_tbl regs =
  let alloc, cm, seq, label_tbl = allocate_func_aux seq label_tbl regs in
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
    if r = mem_reg then mem_reg else reg_of_color (color r)
  in
  let rewrite_if_reg v = match v with
    | Reg r -> Reg (act_reg r)
    | _ -> v
  in
  let rec rewrite_vec i new_vec =
    if i = Vector.length seq then
      new_vec
    else begin
      let instr, label = Vector.get seq i in
      let instr = match instr with
        | Move (r, v, i) -> Move (act_reg r, rewrite_if_reg v, i)
        | Test (a, b, i) -> Test (rewrite_if_reg a, rewrite_if_reg b, i)
        | Jump (a, i) -> Jump (rewrite_if_reg a, i)
        | Return i -> Return i
        | Load (r, v, offset, i) -> Load (act_reg r, rewrite_if_reg v, offset, i)
        | Store (r, v, offset, i) -> Store (act_reg r, rewrite_if_reg v, offset, i)
        | PrimCall (r, p, vl, i) -> PrimCall (act_reg r, p, List.map rewrite_if_reg vl, i)
      in
      Vector.push_back new_vec (instr, label);
      rewrite_vec (i + 1) new_vec
    end
  in
  let new_vec = rewrite_vec 0 (Vector.empty ()) in
  reset_id new_vec

let allocate_experiment reg_num program =
  let regs = make_reg_set reg_num in
  let { signature; label_tbl; seq } = program in
  let callee_sn = List.length regs.callee_saved_regs in
  let seq, label_tbl = insert_callee_save callee_sn seq in
  let seq, label_tbl = allocate_func seq label_tbl regs in
  { signature; label_tbl; seq }

(*
 (* FIXME : remove function from label_tbl *)
let allocate program reg_num =
  let regs = make_act_regs reg_num in
  let sigtbl, _, _ = program in
  let names, procs =
    let caller_save_num = List.length regs.caller_save_regs in
    program 
    |> split_program
    |> List.map (fun (x, y, z) -> (x, (insert_callee_save caller_save_num y, z, regs)))
    |> List.split
  in
  let procs = List.map (fun (x, y, z) -> allocate_func x y z) procs in
*)
