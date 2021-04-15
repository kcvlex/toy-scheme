open Util
open ThreeAddressCodeType
open ThreeAddressCode
open RegsType
open Regs

(* FIXME : when call instr *)

type t = {
  regs : reg_set;
  in_set : reg_table Vector.t;
  out_set : reg_table Vector.t;
  instr_vec : instr_type Vector.t;
  def : reg_table Vector.t;
  use : reg_table Vector.t;
  cfg : int Graph.t;
}

let make_cfg labeling vec =
  let len = Vector.length vec in
  let cfg = Graph.make true in
  for i = 0 to len - 1 do
    let add_seq () =
      if i + 1 < len then Graph.add_edge cfg i (i + 1) else ()
    in
    let instr = Vector.get vec i in
    match instr with
      | Return _ -> ()
      | Test (_, Label s, _) -> 
          Graph.add_edge cfg i (Hashtbl.find labeling s);
          add_seq ()
      | _ -> add_seq ()
  done;
  cfg

let calc_use_def vec regs ignore =
  let len = Vector.length vec in
  let def = Vector.empty () in
  let use = Vector.empty () in
  let rec extract_regs l = match l with
    | (Reg r) :: xs -> r :: (extract_regs xs)
    | _ :: xs -> extract_regs xs
    | [] -> []
  in
  for i = 0 to len - 1 do
    Vector.push_back def (Hashtbl.create 2);
    Vector.push_back use (Hashtbl.create 2);
    let update_def v =
      if ignore v then () else Hashtbl.replace (Vector.get def i) v () 
    in
    let update_use v = 
      let used = match v with
        | Reg r -> [ r ]
        | PrimCall (_, l) -> extract_regs l
        | _ -> []
      in
      let used = List.filter (fun x -> not (ignore x)) used in
      let aux r = Hashtbl.replace (Vector.get use i) r () in
      List.iter aux used
    in
    let instr = Vector.get vec i in
    match instr with
      | Bind (r, v, _) -> update_def r; update_use v
      | Move (r, s, _) -> update_def r; update_use (Reg s)
      | Test (v, u, _) -> update_use v; update_use u
      | Jump (v, _) -> update_use v
      | Call (v, _, _) -> update_use v;
      | Return _ -> ()
      | Load (r, v, _, _) -> update_def r; update_use (Reg v)
      | Store (r, v, _, _) -> update_def r; update_use (Reg v)
  done;
  (use, def)

let comp_reg_table tbl1 tbl2 =
  if Hashtbl.length tbl1 != Hashtbl.length tbl2 then
    false
  else
    Hashtbl.fold (fun x _ acc -> acc && (Hashtbl.mem tbl2 x)) tbl1 true

let union tbl1 tbl2 =
  Hashtbl.iter (fun x y -> Hashtbl.replace tbl1 x y) tbl2
  
let calc_liveness use def cfg instr_vec regs =
  let len = Graph.length cfg in
  let in_set = Vector.make len (Hashtbl.create 2) in
  let out_set = Vector.make len (Hashtbl.create 2) in
  let rec body i =
    let pis = Hashtbl.copy (Vector.get in_set i) in
    let pos = Hashtbl.copy (Vector.get out_set i) in
    let is = Hashtbl.copy (Vector.get out_set i) in
    let os = Hashtbl.create 2 in
    Hashtbl.iter (fun x _ -> Hashtbl.remove is x) (Vector.get def i);
    Hashtbl.iter (fun x _ -> Hashtbl.replace is x ()) (Vector.get use i);
    let succ = Graph.succ cfg i in
    List.iter (fun s -> union os (Vector.get in_set s)) succ;
    let os = match (Vector.get instr_vec i) with
      | Return _ -> 
          List.iter (fun x -> Hashtbl.replace os x ()) regs.callee_saved_regs;
          Hashtbl.replace os (Argument 0) ();
          os
      | _ -> os
    in
    Vector.set in_set i is; Vector.set out_set i os;
    let update = not (comp_reg_table pis is) || not (comp_reg_table pos os) in
    let i = i + 1 in
    if i = len then update else (update || (body i))
  in
  let rec loop () =
    let update = body 0 in
    if update then loop () else ()
  in
  loop ();
  (in_set, out_set)

let analyze regs instr_vec ptbl ignore =
  let instr_vec =
    instr_vec 
    |> Vector.list_of_vector
    |> List.split
    |> fst
    |> Vector.vector_of_list
  in
  let cfg = make_cfg ptbl instr_vec in
  let use, def = calc_use_def instr_vec regs ignore in
  let in_set, out_set = calc_liveness use def cfg instr_vec regs in
  { regs; in_set; out_set; instr_vec; use; def; cfg; }

let live_in lv i = Vector.get lv.in_set i

let live_out lv i = Vector.get lv.out_set i

let get_use lv i = Vector.get lv.use i

let get_def lv i = Vector.get lv.def i

let length lv = Graph.length lv.cfg

let dump lv =
  let rec aux i =
    if i = Vector.length lv.instr_vec then
      []
    else begin
      let rlis = [ 
        Vector.get lv.in_set i;
        Vector.get lv.out_set i;
        Vector.get lv.use i;
        Vector.get lv.def i
      ]
      in
      let label = [
        "in";
        "out";
        "use";
        "def"
      ]
      in
      let s = 
        rlis |> List.map string_of_regtbl
             |> List.combine label
             |> List.map (fun (x, y) -> Printf.sprintf "  %s : %s" x y)
             |> String.concat "\n"
             |> Printf.sprintf "%d :\n%s" i
      in
      s :: (aux (i + 1))
    end
  in 
  String.concat "\n" (aux 0)
