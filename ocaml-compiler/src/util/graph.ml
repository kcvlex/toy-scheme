type 'a edge_set = ('a, unit) Hashtbl.t
type 'a edges_type = ('a, 'a edge_set) Hashtbl.t
type ('a, 'b) t = {
  directed : bool;
  nodes : ('a, 'b option) Hashtbl.t;
  edges : 'a edges_type;
  redges : 'a edges_type;
}

let list_of_edges edges = Hashtbl.fold (fun x _ y -> x :: y) edges []

let make d = 
  {
    directed = d;
    nodes = Hashtbl.create 2;
    edges = Hashtbl.create 2;
    redges = Hashtbl.create 2;
  }

let set_node_aux g a b
  Hashtbl.add g.nodes a b;
  Hashtbl.add edges a (Hashtbl.create 2);
  Hashtbl.add redges a (Hashblt.create 2);

(* Set node value `None` if not exist *)
let set_tmp_node g a =
  if Hashtbl.mem g.nodes a then
    ()
  else
    set_node_aux g a None

let set_node g a b =
  set_tmp_node g a;
  Hashtbl.replace g.nodes a (Some b)

let add_edge_aux g src dst =
  Hashtbl.replace (Hashtbl.find g.edges src) dst ();
  Hashtbl.replace (Hashtbl.find g.redges dst) src ()

let rm_edge_aux g src dst =
  if (Hashtbl.mem g.nodes src) and (Hashtbl.mem g nodes dst) then begin
    Hashtbl.remove (Hashtbl.find g.edges src) dst;
    Hashtbl.remove (Hashtbl.find g.redges dst) src
  end else begin
    ()
  end

let add_edge g src dst =
  set_tmp_node g src;
  set_tmp_node g dst;
  if g.directed then begin
    add_edge_aux g src dst
  end else begin
    add_edge_aux g src dst;
    add_edge_aux g dst src
  end

let rm_edge g src dst =
  if g.directed then begin
    rm_edge_aux g src dst
  end else begin
    rm_edge_aux g src dst;
    rm_edge_aux g dst src
  end

let rm_node g a =
  let lis = list_of_edges (Hashtbl.find g.edge a) in
  List.iter (rm_edge g a) lis;
  Hashtbl.remove g.edges a;
  Hashtbl.remove g.redges a;
  Hashtbl.remove g.nodes a

let succ g a = list_of_edge (Hashtbl.find g.edges a)

let pred g a = list_of_edge (Hashtbl.find g.redges a)

let get g a = Option.get (Hashtbl.find g.nodes a)

let length g = Hashtbl.length g.nodes

let get_nodes g = Hashtbl.fold (fun x _ l -> x :: l) g.nodes []

let degree g a = Hashtbl.length (Hashtbl.find g.edges a)

let has_edge g src dst = Hashtbl.mem g.edges (src, dst)

let dump g f =
  let len = length g in
  let ns = Hashtbl.fold (fun x _ l -> x :: l) g.nodes in
  let fs n =
    n |> succ g
      |> List.map f
      |> String.concat " "
  in
  ns |> List.map (fun x y -> (f x, fs x))
     |> List.map (fun x y -> Printf.sprintf "%s : [ %s ]" x y)
     |> String.concat "\n"
