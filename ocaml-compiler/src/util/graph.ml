type 'a node_set = ('a, unit) Hashtbl.t
type 'a edges_type = ('a, 'a node_set) Hashtbl.t
type 'a t = {
  directed : bool;
  mutable len : int;
  belong : ('a, 'a) Hashtbl.t;
  group : ('a, node_set) Hashtbl.t;
  edges : 'a edges_type;
  redges : 'a edges_type;
}

let list_of_tblkey tbl = Hashtbl.fold (fun x _ y -> x :: y) tbl []

let make d = 
  {
    directed = d;
    len = 0;
    belong = Hashtbl.create 2;
    edges = Hashtbl.create 2;
    redges = Hashtbl.create 2;
  }

let node_preprocess g a =
  if Hashtbl.mem g.belong a then
    ()
  else begin
    let tbl = Hashtbl.create 1 in
    g.len <- g.len + 1;
    Hashtbl.add g.belong a a;
    Hashtbl.add tbl a ();
    Hashtbl.add g.group a tbl;
    Hashtbl.add g.edges a (Hashtbl.create 2);
    Hashtbl.add g.redges a (Hashtbl.create 2);
  end

let represent g a = Hashtbl.find g.belong a

let add_edge_aux g src dst =
  let src = represent g src in
  let dst = represent g dst in
  Hashtbl.replace (Hashtbl.find g.edges src) dst ();
  Hashtbl.replace (Hashtbl.find g.redges dst) src ()

let rm_edge_aux g src dst =
  let src = represent g src in
  let dst = represent g dst in
  if (Hashtbl.mem g.nodes src) and (Hashtbl.mem g nodes dst) then begin
    Hashtbl.remove (Hashtbl.find g.edges src) dst;
    Hashtbl.remove (Hashtbl.find g.redges dst) src
  end else begin
    ()
  end

let add_edge g src dst =
  node_preprocess g src;
  node_preprocess g dst;
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
  let a = represent g.belong a in
  let lis = list_of_tblkey (Hashtbl.find g.edges a) in
  let grp = list_of_tblkey (Hashtbl.find g.group a) in
  g.len <- g.len - 1;
  List.iter (rm_edge g a) lis;
  Hashtbl.remove g.edges a;
  Hashtbl.remove g.redges a;
  Hashtbl.remove g.group a;
  List.iter (Hashtbl.remove g.belong) grp

let succ g a =
  let a = represent g.belong a in
  list_of_edge (Hashtbl.find g.edges a)

let pred g a = 
  let a = represent g.belong a in
  list_of_edge (Hashtbl.find g.redges a)

let length g = Hashtbl.length g.nodes

let nodes g = list_of_tblkey g.group

let degree g a =
  let a = represent g a in
  Hashtbl.length (Hashtbl.find g.edges a)

let has_edge g src dst =
  let src = represent g src in
  let dst = represent g dst in
  Hashtbl.mem g.edges (src, dst)

let contract_aux g a b =
  let neig = list_of_tblkey (Hashtbl.find g.edges b) in
  let grp = list_of_tblkey (Hashtbl.find g.group b) in
  rm_node g b;
  List.iter (fun x -> Hashtbl.replace g.belong x a) grp;
  List.iter (fun x -> Hashtbl.replace (Hashtbl.find group a) x ()) grp;
  List.iter (add_edge g a) neig

let contraction g a b =
  let a = represent g a in
  let b = represent g b in
  if a = b then
    ()
  else begin
    let a, b =
      let al = Hashtbl.length (Hashtbl.find g.edges a) in
      let bl = Hashtbl.length (Hashtbl.find g.edges b) in
      if al < bl then (b, a) else (a, b)
    in
    contract_aux g a b
  end

let dump g f =
  let len = length g in
  let ns = nodes g in
  let fs n =
    n |> succ g
      |> List.map f
      |> String.concat " "
  in
  ns |> List.map (fun x y -> (f x, fs x))
     |> List.map (fun x y -> Printf.sprintf "%s : [ %s ]" x y)
     |> String.concat "\n"
