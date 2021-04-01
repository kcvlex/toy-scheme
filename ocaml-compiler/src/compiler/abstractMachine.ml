open AbstractMachineType
open Util

type bblock_type = AbstractMachineType.t list
type proc_type = string option * string list * string option * bblock_type
type program_type = proc_type Vector.t

type frame_type = {
  mutable cur : bblock_type;
  mutable mem : (string, value_type) Hashtbl.t;
}

type t = {
  mutable heap : value_type list; (* Excpect Cons *)
  mutable frames : frame_type list;
  mutable ra : value_type option;
  program : program_type;
}

let rec translate_proc fname2label clo = match clo with
  | ClosureType.Term t -> [] (* Ignore unnecessary term *)
  | ClosureType.Bind (l, body) ->
      let fn st = 
        let s, t = st in
        match t with
          | ClosureType.Term (ClosureType.Int i) -> [ Move (s, Int i) ]
          | ClosureType.Term (ClosureType.Bool b) -> [ Move (s, Bool b) ]
          | ClosureType.Term (ClosureType.Primitive p) -> [ Move (s, Primitive p) ]
          | ClosureType.Term (ClosureType.Quote a) -> [ Bind (s, Quote a) ]
          | ClosureType.Term ((ClosureType.ClosureRef _) as c) -> [ Bind (s, translate_closure c []) ]
          | ClosureType.Term (ClosureType.Closure (s, cl)) ->
              [ Bind (s, Allocate (List.map translate_value cl)) ]
          | _ ->
              let rv = translate_proc fname2label t in
              List.append rv [ Bind (s, RA) ]
      in
      let binds = l |> List.map fn |> List.flatten in
      let body = translate_proc fname2label body in
      List.append binds body
  | ClosureType.Branch (p, t1, t2) ->
      let p = translate_value p in
      let t1 = translate_proc fname2label t1 in
      let t2 = translate_proc fname2label t2 in
      [ Test (p, t1, t2) ]
  | ClosureType.Call (t, args) -> 
      let args = List.map translate_value args in
      match t with
        | ClosureType.Var s ->
            let label = Hashtbl.find fname2label s in
            [ Call (label, args); Return ]
        | ClosureType.Primitive s -> [ Call (Primitive s, args); Return ]
        | _ -> raise (Invalid_argument "Not function")
and translate_value term = match term with
  | ClosureType.Int i -> Int i
  | ClosureType.Bool b -> Bool b
  | ClosureType.Primitive s -> Primitive s
  | ClosureType.Closure (s, l) -> Cons (Ref s, Allocate (List.map translate_value l))
  | ClosureType.Var s -> Ref s
  | ClosureType.Nil -> Nil
  | ClosureType.Quote a -> Quote a
  | ClosureType.ClosureRef _ -> translate_closure term []
  | _ -> raise (Invalid_argument "UNIMPLED")
and translate_closure c il = match c with
  | ClosureType.ClosureRef (cn, i) -> translate_closure cn (i :: il)
  | ClosureType.Var s -> AccessClosure (s, il)
  | _ -> raise (Invalid_argument "AAA")


let translate (clsr_prog: ClosureType.t) =
  let sz = List.length clsr_prog.procs in
  let fname2label =
    let tbl = Hashtbl.create sz in
    let sl = fst (List.split clsr_prog.procs) in
    let rec upd sl i = match sl with
      | x :: xs -> Hashtbl.add tbl x (Label i); upd xs (i + 1)
      | [] -> ()
    in
    upd sl 0;
    Hashtbl.add tbl "entry" (Label sz);
    tbl
  in
  let program =
    let trans body =
      body |> translate_proc fname2label
           |> fun x -> List.append x [ Return ]
    in
    let tp = 
      clsr_prog.procs 
      |> List.split 
      |> snd
      |> List.map (fun (c, l, opt, body) -> (Some c, l, opt, trans body))
    in
    let vec = Vector.make (sz + 1) (None, [], None, []) in
    List.iter (Vector.push_back vec) tp;
    Vector.push_back vec (None, [], None, trans clsr_prog.body);
    vec
  in
  let frame = {
    cur = (fun (_, _, _, body) -> body) (Vector.rget program 0);
    mem = Hashtbl.create 8;
  }
  in
  {
    heap = [];
    frames = [ frame ];
    ra = None;
    program = program;
  }

let push_mem machine n v = 
  let frames = machine.frames in
  let frames = match frames with
    | x :: xs ->
        let { mem; cur } = x in
        Hashtbl.add mem n v;
        let x = { mem; cur } in
        x :: xs
    | [] -> raise (Invalid_argument "Empty frame")
  in
  machine.frames <- frames

let advance machine =
  let hd, tl = (List.hd machine.frames, List.tl machine.frames) in
  (match hd.cur with
    | x :: xs -> hd.cur <- xs
    | [] -> raise (Invalid_argument "must return"));
  machine.frames <- (hd :: tl)

let consume_result machine =
  let r = Option.get machine.ra in
  machine.ra <- None; r

let produce_result machine v = machine.ra <- Some v

let restrict_1arg lis = match lis with
  | x :: [] -> x
  | _ -> raise (Invalid_argument "must be 1 argument")
      
let restrict_2arg lis = match lis with
  | x1 :: x2 :: [] -> (x1, x2)
  | _ -> raise (Invalid_argument "must be 2 argument")

let insert_instr machine instr =
  let fh, ft = ((List.hd machine.frames), (List.tl machine.frames)) in
  fh.cur <- instr :: fh.cur;
  machine.frames <- fh :: ft

let rec follow_cons cons idxlis = match idxlis with
  | [] -> cons
  | x :: xs -> (match cons with
    | Cons (car, cdr) -> if x = 0 then follow_cons car xs else follow_cons cdr ((x - 1) :: xs)
    | _ -> raise (Invalid_argument "cannot follow"))

let rec flat_cons cons = match cons with
  | Cons (car, cdr) -> car :: (flat_cons cdr)
  | Nil -> []
  | _ -> raise (Invalid_argument "must be cons")

let rec cons_of_list lis = match lis with
  | x :: xs -> Cons (x, cons_of_list xs)
  | [] -> Nil

let rec eval_step machine = match machine.frames with
  | [] -> ()
  | f :: fx -> 
      let blk = f.cur in 
      match blk with
        | [] -> raise (Invalid_argument "must return")
        | hd :: tl ->
          f.cur <- tl;
          machine.frames <- f :: fx;
          match hd with
            | Bind (s, v) ->
                eval_value machine v;
                let v = consume_result machine in
                push_mem machine s v; advance machine
            | Move (s, v) ->
                eval_value machine v;
                let v = consume_result machine in
                push_mem machine s v; advance machine
            | Test (p, t1, t2) ->
                eval_value machine p;
                let p = consume_result machine in
                let p = match p with
                  | Bool b -> b
                  | _ -> raise (Invalid_argument "Condition must be Bool")
                in
                advance machine;
                List.iter (fun x -> insert_instr machine x) (List.rev (if p then t1 else t2))
            | Return -> machine.frames <- List.tl machine.frames
            | Call (f, args) -> eval_call machine f args
and eval_value machine v = match v with
  | Int _ | Bool _  | Primitive _ | Label _ | Nil | Quote _ | Cons _-> produce_result machine v
  | Ref s -> 
      let v = Hashtbl.find (List.hd machine.frames).mem s in
      produce_result machine v
  | Allocate vl ->
      let rec fn l = match l with
        | x :: xs ->
            eval_value machine x;
            let car = consume_result machine in
            let cdr = fn xs in
            let cdr = consume_result machine in
            let cons = Cons (car, cdr) in
            produce_result machine cons
        | [] -> produce_result machine Nil
      in
      fn vl
  | RA -> ()
  | AccessClosure (c, ilis) ->
      c |> Hashtbl.find (List.hd machine.frames).mem
        |> fun x -> follow_cons x ilis
        |> produce_result machine
and eval_call machine f args =
  let eval_args lis =
    let f v = 
      eval_value machine v; 
      consume_result machine 
    in
    List.map f lis
  in
  let f = 
    eval_value machine f; 
    consume_result machine
  in
  let args = eval_args args in
  match f with
    | Primitive s -> (match s with
      | "+" | "-" | "*" | "/" | "<" | "eq?" ->
        (* FIXME : bool *)
        let f x = match x with
          | Int i -> i
          | _ -> raise (Invalid_argument "must be int")
        in
        let args = List.map f args in
        let fold f lis = List.fold_left f (List.hd lis) (List.tl lis) in
        (match s with
          | "+" -> produce_result machine (Int (fold (fun x y -> x + y) args))
          | "-" -> produce_result machine (Int (fold (fun x y -> x - y) args))
          | "*" -> produce_result machine (Int (fold (fun x y -> x * y) args))
          | "/" -> produce_result machine (Int (fold (fun x y -> x / y) args))
          | "<" -> let a, b = restrict_2arg args in produce_result machine (Bool (a < b))
          | "eq?" -> let a, b = restrict_2arg args in produce_result machine (Bool (a = b))
          | _ -> raise (Invalid_argument ("Unknown op " ^ s)))
      | "apply" ->
          let f, args = restrict_2arg args in
          produce_result machine args;
          let args = consume_result machine in
          insert_instr machine (Call (f, (flat_cons args)));
          eval_step machine
      | "car" | "cdr" ->
          let arg = restrict_1arg args in
          produce_result machine arg;
          let arg = consume_result machine in
          (match arg with
            | Cons (car, cdr) -> produce_result machine (if s = "car" then car else cdr)
            | _ -> raise (Invalid_argument "must be cons"))
      | _ -> raise (Invalid_argument ("Unkonwn function " ^ s)))
    | Cons (Label i, clsr) ->
        let carg, largs, optarg, proc = Vector.get machine.program i in
        let mem = Hashtbl.create 8 in
        let binding carg largs optarg lis =
          let preprocess () = match carg with
            | Some c -> Hashtbl.add mem c (List.hd lis); List.tl lis
            | None -> lis
          in
          let rec process names args = match (names, args) with
            | (x :: xs, y :: ys) -> Hashtbl.add mem x y; process xs ys
            | ([], []) -> ()
            | ([], l) -> (match optarg with
              | Some o -> Hashtbl.add mem o (cons_of_list l)
              | None -> raise (Invalid_argument "Arguments is too much to error"))
            | (l, []) -> raise (Invalid_argument "Arguments is too less to error")
          in
          let lis = preprocess () in
          process largs lis
        in
        binding carg largs optarg args;
        let frame = { 
          cur = (fun (_, _, _, body) -> body) (Vector.rget machine.program i);
          mem = mem;
        } 
        in
        machine.frames <- frame :: machine.frames;
        eval_step machine

let rec eval machine =
  if List.length machine.frames = 0 then 
    () 
  else begin
    eval_step machine;
    eval machine
  end
