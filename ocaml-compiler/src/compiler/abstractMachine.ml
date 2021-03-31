open ClosureType
open AbstractMachineType

type bblock = AbstractMachineType.t list

type program_type = bblock Vector.t

type t = {
  mutable heap : (string * value_type) Hashtbl.t;
  mutable local : (string * vlaue_type) Hashtbl.t;
  mutable cur : bblock;
  mutable args : value_type list;
  mutable return : bblock option;
  program : program_type;
}

let rec translate_proc clo fname2label = match clo with
  | ClosureType.Term t -> [] (* Ignore unnecessary term *)
  | ClosureType.Bind (l, body) ->
      let fn st = 
        let s, t = st in
        let t = match t with
          | Term (ClosureType.Int i) -> Move (s, Int i)
          | Term (ClosureType.Bool b) -> Move (s, Bool b)
          | Term (ClosureType.Primitive p) -> raise (Invalid_argument "UNIMPLED")
          | Term (ClosureType.Closure (s, cl)) -> Bind (s, Allocate (List.map translate_term cl))
          | Term (ClosureType.Quote a) -> Bind (s, Quote a)
          | Term (ClosureType.ClosureRef (c, i)) -> Bind (s, translate_closure c [ i ])
          | _ -> Bind (s, translate_proc t program fn2label)
        in
        (s, t)
      in
      let binds = List.map fn l in
      let body = translate_proc body fname2label in
      List.append binds body
  | ClosureType.Branch (p, t1, t2) ->
      let p = translate_term p in
      let f t = match t with
        | Call (Var f, args) -> 
            let args = List.map translate_term args in
            let label = Hashtbl.find fname2label f in
            (label, args)
        | Call (Primitive s, args) ->
            let args = List.map translate_term args in
            (Primitive s, args)
      in
      let t1 = f t1 in
      let t2 = f t2 in
      [ Test (p, t1, t2) ]
  | ClosureType.Call (t, args) -> 
      let args = List.map translate_term args in
      match t with
        | Var s ->
            let label = Hashtbl.find fname2label s in
            [ Call (lable, args) ]
        | ClosureType.Primitive s -> [ Call (Primitive s, args) ]
and translate_term term = match term with
  | ClosureType.Int i -> Int i
  | ClosureType.Bool b -> Bool b
  | ClosureType.Primitive s -> Primitive s
  | ClosureType.Var s -> Ref s
  | ClosureType.Nil -> Nil
  | ClosureType.Quote a -> Quote a
  | ClosureType.ClosureRef (c, i) -> translate_closure c [ i ]
  | _ -> raise (Invalid_argument "UNIMPLED")
and translate_closure c il = match c with
  | (ClosureType.ClosureRef (cn, i) -> translate_closure cn (i :: il)
  | Var s -> AccessClosure (s, il)
  | _ -> raise (Invalid_argument "AAA")


let translate clsr_prog =
  let sz = List.length clsr_prog.procs in
  let fname2label =
    let tbl = Hashtbl.create sz in
    let sl = fst (List.split clsr_prog.procs) in
    let rec upd sl i = match sl with
      | x :: xs -> Hashtbl.add tbl x (Label i); upd xs (i + 1)
      | [] -> ()
    in
    upd sl 0;
    Hashtbl.add tbl "entry" sz;
    tbl
  in
  let program =
    let tp = List.append (translate_proc p tbl) [ Return ] in
    let vec Vector.make sz + 1 in
    let rec translate_aux pl = match pl with
      | (_, p) :: xs -> Vector.push_back vec (tp p); translate_aux xs
      | [] -> ()
    in
    translate_aux clsr_prog.procs;
    Vector.push_back vec (tp clsr_proc.body);
    vec
  in
  {
    heap = ref Hashtbl.create 8;
    local = ref Hashtbl.create 8;
    cur = ref Vector.rget program;
    args = ref [];
    return = ref None;
    program = program;
  }
