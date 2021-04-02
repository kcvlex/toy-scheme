type ('a, 'b) t = {
  directed : bool;
  nodes : ('a, int) Hashtbl.t;
  rnodes : 'a Vector.t;
  edges : edge_set Vector.t;
  redges : edge_set Vector.t;
  attr : 'b Vector.t;
}
and edge_set = (int, unit) Hashtbl.t

let make d = 
  {
    directed = d;
    nodes = Hashtbl.create 2;
    rnodes = Vector.empty ();
    edges = Vector.empty ();
    redges = Vector.empty ();
    attr = Vector.empty ();
  }

let add_node g a b =
  let id = Hashtbl.length g.nodes in
  Hashtbl.add g.nodes a id;
  Vector.push_back g.rnodes a;
  Vector.push_back g.edges (Hashtbl.create 2);
  Vector.push_back g.redges (Hashtbl.create 2);
  Vector.push_back g.attr b

let add_edge_aux g id1 id2 = 
  Hashtbl.add (Vector.get g.edges id1) id2 ();
  Hashtbl.add (Vector.get g.redges id2) id1 ()

let rm_edge_aux g id1 id2 = 
  Hashtbl.remove (Vector.get g.edges id1) id2;
  Hashtbl.remove (Vector.get g.redges id2) id1

let get_id g a = Hashtbl.find g.nodes a

let add_edge g src dst =
  let id1 = get_id g src in
  let id2 = get_id g dst in
  if g.directed then
    add_edge_aux g id1 id2
  else
    add_edge_aux g id1 id2;
    add_edge_aux g id2 id1

let rm_edge g src dst =
  let id1 = get_id g src in
  let id2 = get_id g dst in
  if g.directed then
    rm_edge_aux g id1 id2
  else
    rm_edge_aux g id1 id2;
    rm_edge_aux g id2 id1

let succ g a =
  let id = get_id g a in
  let edges = Vector.get g.edges id in
  let edges = Hashtbl.fold (fun x _ y -> (Vector.get g.rnodes x) :: y) edges [] in
  edges

let pred g a =
  let id = get_id g a in
  let edges = Vector.get g.redges id in
  let edges = Hashtbl.fold (fun x _ y -> (Vector.get g.rnodes x) :: y) edges [] in
  edges

let get g a =
  let id = get_id g a in
  Vector.get g.attr id

let length g = Hashtbl.length g.nodes
