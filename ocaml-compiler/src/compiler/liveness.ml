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
    let add_seq () =
      if i + 1 < len then Graph.add_edge cfg i (i + 1) else ()
    in
    let instr = Vector.get vec i in
    match instr with
      | Return _ -> ()
      | Test (_, JumpLabel s, _) -> 
          Graph.add_edge cfg i (Hashtbl.find labeling s);
          add_seq ()
      | Jump (JumpLabel s, _) ->
          Graph.add_edge cfg i (Hashtbl.find labeling s);
          add_seq ()
      | _ -> add_seq ()
  done;
  cfg

let calc_use_def vec =
  let len = Vector.length vec in
  let def = Vector.empty () in
  let use = Vector.empty () in
  for i = 0 to len - 1 do
    Vector.push_back def (Hashtbl.create 2);
    Vector.push_back use (Hashtbl.create 2);
    let update_def v = Hashtbl.replace (Vector.get def i) v () in
    let update_use v = match v with
      | Reg r -> Hashtbl.replace (Vector.get use i) r ()
      | _ -> ()
    in
    let instr = Vector.get vec i in
    match instr with
      | Bind (r, v, _) -> update_def r; update_use v
      | Move (r, v, _) -> update_def r; update_use v
      | Test (v, u, _) -> update_use v; update_use u
      | Jump (v, _) -> update_use v
      | Return _ -> ()
      | Load (r, v, _, _) -> update_def r; update_use v
      | Store (r, v, _, _) -> update_def r; update_use v
      | PrimCall (r, _, vl, _) -> update_def r; List.iter (fun v -> update_use v) vl
  done;
  (use, def)

let comp_reg_set tbl1 tbl2 =
  if Hashtbl.length tbl1 != Hashtbl.length tbl2 then
    false
  else
    Hashtbl.fold (fun x _ acc -> acc && (Hashtbl.mem tbl2 x)) tbl1 true

let union tbl1 tbl2 =
  Hashtbl.iter (fun x y -> Hashtbl.replace tbl1 x y) tbl2

let calc_liveness use def cfg =
  let len = Graph.length cfg in
  let in_set = Vector.make len (Hashtbl.create 2) in
  let out_set = Vector.make len (Hashtbl.create 2) in
  let rec step_i i update =
    let pis = Hashtbl.copy (Vector.get in_set i) in
    let pos = Hashtbl.copy (Vector.get out_set i) in
    let is = Hashtbl.copy (Vector.get out_set i) in
    let os = Hashtbl.create 2 in
    Hashtbl.iter (fun x _ -> Hashtbl.remove is x) (Vector.get def i);
    Hashtbl.iter (fun x _ -> Hashtbl.replace is x ()) (Vector.get use i);
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

let analyze vec ptbl =
  let cfg = make_cfg instr_vec ptbl in
  let use, def = calc_use_def instr_vec in
  let in_set, out_set = calc_liveness use def cfg in
  { in_set; out_set; instr_vec; use; def; cfg; }

let live_in lv i = Vector.get lv.in_set i

let live_out lv i = Vector.get lv.out_set i

let get_use lv i = Vector.get lv.use i

let get_def lv i = Vector.get lv.def i

let length lv = Graph.length lv.cfg
