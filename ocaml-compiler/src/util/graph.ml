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

let get_represent g a = Hashtbl.find g.belong a

let add_edge_aux g src dst =
  let src = get_represent g src in
  let dst = get_represent g dst in
  Hashtbl.replace (Hashtbl.find g.edges src) dst ();
  Hashtbl.replace (Hashtbl.find g.redges dst) src ()

let rm_edge_aux g src dst =
  let src = get_represent g src in
  let dst = get_represent g dst in
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
  let a = get_represent g.belong a in
  let lis = list_of_tblkey (Hashtbl.find g.edge a) in
  List.iter (rm_edge g a) lis;
  g.len <- g.len - 1;
  Hashtbl.remove g.edges a;
  Hashtbl.remove g.redges a;
  Hashtbl.iter (fun x _ -> Hashtbl.remove g.belong x) (Hashtbl.find g.group a);
  Hashtbl.remove g.group a

let succ g a =
  let a = get_represent g.belong a in
  list_of_edge (Hashtbl.find g.edges a)

let pred g a = 
  let a = get_represent g.belong a in
  list_of_edge (Hashtbl.find g.redges a)

let length g = Hashtbl.length g.nodes

let nodes g =
  let tbl = Hashtbl.create (Hashtbl.length g.belong) in
  let ns = list_of_tblkey g.belong in
  let rec aux lis = match lis with
    | x :: xs ->
        if Hashtbl.mem tbl x then
          aux xs
        else begin
          Hashtbl.replace tbl x ();
          x :: (aux xs)
        end
    | [] -> []
  in
  aux ns

let degree g a =
  let a = get_represent g a in
  Hashtbl.length (Hashtbl.find g.edges a)

let has_edge g src dst =
  let src = get_represent g src in
  let dst = get_represent g dst in
  Hashtbl.mem g.edges (src, dst)

let add_edge_set g src dstset =
  let src = get_represent g src in
  Hashtbl.iter (fun x _ -> add_edge g src (get_represent g x)) dstset

(* FIXME subete *)
let contraction g a b =
  let a = get_represent g a in
  let b = get_represent g b in
  if a = b then
    ()
  else begin
    let a, b =
      let al = Hashtbl.length (Hashtbl.find g.edges a) in
      let bl = Hashtbl.length (Hashtbl.find g.edges b) in
      if al < bl then (b, a) else (a, b)
    in
    let gr = Hashtbl.find g.group b in
    List.iter (fun x -> add_edge_set g a (Hashtbl.find g.edges x))
    List.iter (add_edge g a) edges;
    rm_node g b
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
