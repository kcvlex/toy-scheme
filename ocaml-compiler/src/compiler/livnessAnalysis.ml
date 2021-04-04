open Util
open ThreeAddressCodeType

type t = {
  in_set : reg_set Vector.t;
  out_set : reg_set Vector.t;
  instr_vec : instr_type Vector.t;
  def : reg_set Vector.t;
  use : reg_set Vector.t;
  cfg : (int, unit) Graph.t;
}

let make_cfg labeling vec =
  let len = Vector.length vec in
  let cfg = Graph.make true in
  for i = 0 to len - 1 do
    Graph.add_node cfg i ()
  done;
  for i = 0 to len - 1 do
    let instr = Vector.get vec i in
    match instr with
      | Return _ -> ()
      | Test (_, JumpLabel s, _) -> 
          Graph.add_edge cfg i (Hashtbl.find labeling s);
          Graph.add_edge cfg i (i + 1)
      | Jump (JumpLabel s, _) ->
          Graph.add_edge cfg i (Hashtbl.find labeling s);
          Graph.add_edge cfg i (i + 1)
      | _ -> Graph.add_edge cfg i (i + 1)
  done;
  cfg

let calc_use_def vec =
  let len = Vector.length vec in
  let def = Vector.make len (Hashtbl.create 2) in
  let use = Vector.make len (Hashtbl.create 2) in
  let update_def i v = Hashtbl.add (Vector.get def i) v () in
  let update_use i v = match v with
    | Reg r -> Hashtbl.add (Vector.get use i) r ()
    | _ -> ()
  in
  for i = 0 to len - 1 do
    let instr = Vector.get vec i in
    match instr with
      | Bind (r, v, _) -> update_def i r; update_use i v
      | Move (r, v, _) -> update_def i r; update_use i v
      | Test (v, u, _) -> update_use i v; update_use i u;
      | Jump (v, _) -> update_use i v;
      | Return _ -> ()
      | Load (r, v, _, _) -> update_def i r; update_use i v;
      | Store (r, v, _, _) -> update_def i r; update_use i v;
      | PrimCall (r, _, vl, _) -> update_def i r; List.iter (fun v -> update_use i v) vl
  done;
  (def, use)

let comp_reg_set tbl1 tbl2 =
  if Hashtbl.length tbl1 != Hashtbl.length tbl2 then
    false
  else
    Hashtbl.fold (fun x _ acc -> acc && (Hashtbl.mem tbl2 x)) tbl1 true

let union tbl1 tbl2 =
  Hashtbl.iter (fun x y -> Hashtbl.add tbl1 x y) tbl2

let calc_liveness use def cfg =
  let len = Graph.length cfg in
  let in_set = Vector.make len (Hashtbl.create 2) in
  let out_set = Vector.make len (Hashtbl.create 2) in
  let rec step_i i update =
    let pis = Hashtbl.copy (Vector.get in_set i) in
    let pos = Hashtbl.copy (Vector.get out_set i) in
    let is = Hashtbl.copy (Vector.get out_set i) in
    let os = Hashtbl.create 2 in
    Hashtbl.iter (fun x _ -> Hashtbl.remove is x) def;
    Hashtbl.iter (fun x _ -> Hashtbl.add is x ()) use;
    let succ = Graph.succ cfg i in
    List.iter (fun s -> union os (Vector.get in_set s)) succ;
    Vector.set in_set i is; Vector.set out_set i os;
    let i = i + 1 in
    if i = len then
      update
    else
      let update = update || not (comp_reg_set pis is) || not (comp_reg_set pos os) in
      step_i i update
  in
  let rec step () =
    if step_i 0 false then
      step ()
    else
      ()
  in
  step ();
  (in_set, out_set)

let analyze (_, labeling, instr_vec) =
  let cfg = make_cfg labeling instr_vec in
  let use, def = calc_use_def instr_vec in
  let in_set, out_set = calc_liveness use def cfg in
  { in_set; out_set; instr_vec; use; def; cfg; }

let live_in lv i = Vector.get lv.in_set i

let live_out lv i = Vector.get lv.out_set i

let length lv = Graph.length lv.cfg
