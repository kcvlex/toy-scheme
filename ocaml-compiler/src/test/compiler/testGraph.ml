open Util
open Testtool

let a = "a"
let b = "b"
let c = "c"
let d = "d"
let e = "e"
let r1 = "r1"
let r2 = "r2"
let r3 = "r3"

let edge_lis = [
  (r1, r2); (r1, r3);
  (r2, r3); (r2, a); (r2, c);
  (a, b); (a, c); (a, d);
  (b, c); (b, d); (b, e);
  (c, d); (c, e);
  (d, e)
]

let tbl_of_edges g lis =
  let tbl = Hashtbl.create (List.length lis) in
  let rec aux lis = match lis with
    | (a, b) :: xs -> 
        let a = Graph.represent g a in
        let b = Graph.represent g b in
        Hashtbl.replace tbl (a, b) ();
        Hashtbl.replace tbl (b, a) ();
        aux xs
    | [] -> ()
  in
  aux lis; tbl

let validate_edges g tbl n =
  let n = Graph.represent g n in
  let succ = Graph.succ g n in
  List.iter (fun s -> assert (Hashtbl.mem tbl (n, s))) succ

let validate_group g group = match group with
  | [] -> ()
  | x :: xs -> List.iter (fun s -> assert (Graph.is_same_group g x s)) xs

let validate_graph g edges groups =
  let nodes = Graph.nodes g in
  let tbl = tbl_of_edges g edges in
  List.iter (fun n -> validate_edges g tbl n) nodes;
  List.iter (validate_group g) groups

let validate1 g =
  let edges = [
    (r1, r2); (r1, r3); (r1, d);
    (r2, r3); (r2, d)
  ]
  in
  let groups = [
    [ r1; a; e ];
    [ r2; b ];
    [ r3 ];
    [ d ]
  ]
  in
  validate_graph g edges groups

let validate2 g =
  let edges = [ 
    (r1, r2); (r1, r3);
    (r2, r3)
  ]
  in
  let groups = [
    [ r1; a; e ];
    [ r2; b ];
    [ r3 ]
  ]
  in
  validate_graph g edges groups

let () =
  let g = Graph.make false in
  List.iter (fun (x, y) -> Graph.add_edge g x y) edge_lis;
  Graph.rm_node g c;
  Graph.contraction g a e;
  Graph.contraction g r2 b;
  Graph.contraction g r1 a;
  validate1 g;
  Graph.rm_node g d;
  validate2 g;
  TestUtil.print_label "test passed : Graph"
