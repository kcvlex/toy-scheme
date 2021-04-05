open AbstractMachineType

type frame_type = {
  mutable cur : AbstractMachineType.instr_type list;
  mutable mem : (string, value_type) Hashtbl.t;
}

type t = {
  mutable heap : value_type list; (* Excpect Cons *)
  mutable frames : frame_type list;
  mutable ra : value_type option;
  program : AbstractMachineType.t;
}

let entry_label = "entry"

let rec translate_proc labels clo =
  let recf c = translate_proc labels c in
  let recv v = translate_value labels v in
  match clo with
    | ClosureType.Term t -> [ Value (recv t) ]
    | ClosureType.Bind (l, body) ->
        let fn st = 
          let s, t = st in
          match t with
            | ClosureType.Term (ClosureType.Int i) -> [ Move (s, Int i) ]
            | ClosureType.Term (ClosureType.Bool b) -> [ Move (s, Bool b) ]
            | ClosureType.Term (ClosureType.Primitive p) -> [ Move (s, Primitive p) ]
            | ClosureType.Term (ClosureType.Quote a) -> [ Bind (s, Quote a) ]
            | ClosureType.Term ((ClosureType.ClosureRef _) as c) -> [ Bind (s, translate_closure c []) ]
            | ClosureType.Term (ClosureType.Closure (c, cl)) ->
                let c = recv (ClosureType.Var c) in
                [ Bind (s, Cons (c, Allocate (List.map recv cl))) ]
            | _ ->
                let rv = recf t in
                List.append rv [ Bind (s, RA) ]
        in
        let binds = l |> List.map fn |> List.flatten in
        let body = translate_proc labels body in
        List.append binds body
    | ClosureType.Branch (p, t1, t2) ->
        let p = recv p in
        let t1 = recf t1 in
        let t2 = recf t2 in
        [ Test (p, t1, t2) ]
    | ClosureType.Call (f, args) -> [ Call (recv f, List.map recv args) ]
and translate_value labels value =
  let recf v = translate_value labels v in
  match value with
    | ClosureType.Int i -> Int i
    | ClosureType.Bool b -> Bool b
    | ClosureType.Primitive s -> Primitive s
    | ClosureType.Closure (s, l) -> Cons (Ref s, Allocate (List.map recf l))
    | ClosureType.Var "DUMMY" -> Nil
    | ClosureType.Var s ->
        if Hashtbl.mem labels s then
          Label s
        else
          Ref s
    | ClosureType.Nil -> Nil
    | ClosureType.Quote a -> Quote a
    | ClosureType.ClosureRef _ -> translate_closure value []
    | _ -> raise (Invalid_argument "UNIMPLED")
and translate_closure c il = match c with
  | ClosureType.ClosureRef (cn, i) -> translate_closure cn (i :: il)
  | ClosureType.Var s -> AccessClosure (s, il)
  | _ -> raise (Invalid_argument "AAA")


let rec relocate_return seq = match seq with
  | Test (p, t1, t2) :: Return :: xs ->
      let t1 = relocate_return (List.append t1 [ Return ]) in
      let t2 = relocate_return (List.append t2 [ Return ]) in
      let test = Test (p, t1, t2) in
      let xs = relocate_return xs in
      test :: xs
  | x :: xs -> x :: (relocate_return xs)
  | [] -> []



let translate (clsr_prog: ClosureType.t) =
  let sz = List.length clsr_prog.procs in
  let names = fst (List.split clsr_prog.procs) in
  let labels =
    let tbl = Hashtbl.create (sz + 1) in
    List.iter (fun x -> Hashtbl.add tbl x ()) (entry_label :: names); tbl
  in
  let program =
    let trans body =
      body |> translate_proc labels
           |> fun x -> List.append x [ Return ]
           |> relocate_return
    in
    let tp = 
      clsr_prog.procs 
      |> List.split 
      |> snd
      |> List.map (fun (c, l, opt, body) -> (Some c, l, opt, trans body))
    in
    let program =
      let tbl = Hashtbl.create sz in
      let program = List.combine names tp in
      List.iter (fun (x, y) -> Hashtbl.add tbl x y) program;
      tbl
    in
    let body = match clsr_prog.body with
      | ClosureType.Bind (l, Term t) -> ClosureType.Bind (l, Call (t, []))
      | _ -> raise (Invalid_argument "body")
    in
    Hashtbl.add program entry_label (None, [], None, trans body);
    program
  in
  let frame = {
    cur = (fun (_, _, _, body) -> body) (Hashtbl.find program entry_label);
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
    | _ :: xs -> hd.cur <- xs
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
  | _ -> []

let rec cons_of_list lis = match lis with
  | x :: xs -> Cons (x, cons_of_list xs)
  | [] -> Nil

let rec eval_step machine = match machine.frames with
  | [] -> ()
  | f :: _ -> 
      let blk = f.cur in 
      match blk with
        | [] -> raise (Invalid_argument "must return")
        | hd :: _ ->
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
            | Value v -> advance machine; eval_value machine v
            | Return -> machine.frames <- List.tl machine.frames
            | Call (f, args) -> advance machine; eval_call machine f args
and eval_value machine v = match v with
  | Int _ | Bool _  | Primitive _ | Label _ | Nil | Quote _ -> produce_result machine v
  | Cons (car, cdr) ->
      let car = 
        eval_value machine car; consume_result machine
      in
      let cdr =
        eval_value machine cdr; consume_result machine
      in
      produce_result machine (Cons (car, cdr))
  | Ref s ->
      let v = Hashtbl.find (List.hd machine.frames).mem s in
      produce_result machine v
  | Allocate vl ->
      let rec fn l = match l with
        | x :: xs ->
            eval_value machine x;
            let car = consume_result machine in
            let _ = fn xs in
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
    | Primitive p -> (match p with
      | ADD | SUB | MUL | DIV | LESS | EQ ->
        (* FIXME : bool *)
        let f x = match x with
          | Int i -> i
          | _ -> raise (Invalid_argument "must be int")
        in
        let args = List.map f args in
        let fold f lis = List.fold_left f (List.hd lis) (List.tl lis) in
        (match p with
          | ADD -> produce_result machine (Int (fold (fun x y -> x + y) args))
          | SUB -> produce_result machine (Int (fold (fun x y -> x - y) args))
          | MUL -> produce_result machine (Int (fold (fun x y -> x * y) args))
          | DIV -> produce_result machine (Int (fold (fun x y -> x / y) args))
          | LESS -> let a, b = restrict_2arg args in produce_result machine (Bool (a < b))
          | EQ -> let a, b = restrict_2arg args in produce_result machine (Bool (a = b)))
      | APPLY ->
          let f, args = restrict_2arg args in
          eval_call machine f (flat_cons args)
      | CAR | CDR ->
          let arg = restrict_1arg args in
          (match arg with
            | Cons (car, cdr) -> produce_result machine (if p = CAR then car else cdr)
            | _ -> raise (Invalid_argument "must be cons"))
      | DISPLAY ->
          let arg = restrict_1arg args in
          (match arg with
            | Int i -> print_endline (string_of_int i)
            | Bool true -> print_endline "#t"
            | Bool false -> print_endline "#f"
            | _ -> raise (Invalid_argument "display"))
      | CONS ->
          let car, cdr = restrict_2arg args in
          produce_result machine (Cons (car, cdr))
      | LIST -> produce_result machine (cons_of_list args)
      | MAP -> raise (Invalid_argument "UNIMPLED : MAP")
      | LISTREF ->
          let l, i = restrict_2arg args in
          let rec dfs l i = match (l, i) with
            | Cons (car, _), 0 -> car
            | Cons (_, cdr), _ -> dfs cdr (i - 1)
            | _ -> raise (Invalid_argument "dfs of LISTREF")
          in
          (match i with
            | Int i -> produce_result machine (dfs l i)
            | _ -> raise (Invalid_argument "LISTREF"))
      | NULL ->
          let arg = restrict_1arg args in
          (match arg with
            | Nil -> produce_result machine (Bool true)
            | _ -> produce_result machine (Bool false))
      | _ -> raise (Invalid_argument ("Unknown function " ^ (Symbol.string_of_sym (PrimitiveSym p)))))
    | Cons (Label l, clsr) ->
        let args = clsr :: args in
        let carg, largs, optarg, _ = Hashtbl.find machine.program l in
        let mem = Hashtbl.create 8 in
        let binding carg largs optarg lis =
          let preprocess () = match carg with
            | Some c -> Hashtbl.add mem c (List.hd lis); List.tl lis
            | None -> lis
          in
          let rec process names args = match (names, args) with
            | (x :: xs, y :: ys) -> Hashtbl.add mem x y; process xs ys
            | ([], l) -> (match optarg with
              | Some o -> Hashtbl.add mem o (cons_of_list l)
              | None -> (match l with
                | [] -> ()
                | _ -> raise (Invalid_argument "Arguments is so much that error occured")))
            | (_, []) -> raise (Invalid_argument "Arguments is so less that error occured")
          in
          let lis = preprocess () in
          process largs lis
        in
        binding carg largs optarg args;
        let frame = { 
          cur = (fun (_, _, _, body) -> body) (Hashtbl.find machine.program l);
          mem = mem;
        } 
        in
        machine.frames <- frame :: machine.frames

let rec eval machine =
  if List.length machine.frames = 0 then 
    ()
  else begin
    eval_step machine;
    eval machine
  end

let make_instr_str label lis =
  lis |> String.concat ", "
      |> fun x -> label ^ " (" ^ x ^ ")"

let rec string_of_abs abs = match abs with
  | Bind (s, v) -> make_instr_str "BIND" [ s; string_of_value v ]
  | Move (s, v) -> make_instr_str "MOVE" [ s; string_of_value v ]
  | Test (p, t1, t2) ->
      let p = string_of_value p in
      let indent = "      " in
      let f t = 
        t |> List.map string_of_abs
          |> String.concat ", "
          |> fun x -> indent ^ "[ " ^ x ^ " ]"
      in
      let t1 = f t1 in
      let t2 = f t2 in
      String.concat "\n" [ "TEST (" ^ p; t1; (t2 ^ ")") ]
  | Return -> "RETURN"
  | Value v -> make_instr_str "VALUE" [ string_of_value v ]
  | Call (f, args) ->
      let lis = f :: args in
      lis |> List.map string_of_value
          |> make_instr_str "CALL"
and string_of_value value = match value with
  | Int i -> string_of_int i
  | Bool true -> "TRUE"
  | Bool false -> "FALSE"
  | Primitive p -> Symbol.string_of_sym (PrimitiveSym p)
  | Label l -> "LABEL (" ^ l ^ ")"
  | Ref s -> s
  | Allocate lis -> make_instr_str "ALLOCATE" (List.map string_of_value lis)
  | RA -> "RA"
  | Nil -> "NULL"
  | Quote a -> "QUOTE (" ^ (Ast.code_of_ast a) ^ ")"
  | Cons (car, cdr) -> make_instr_str "CONS" (List.map string_of_value [ car; cdr ])
  | AccessClosure (s, il) -> 
      il |> List.map string_of_int
         |> fun x -> s :: x
         |> make_instr_str "ACCESS"

let string_of_program program =
  let program = Hashtbl.fold (fun k (_, _, _, body) l -> (k, body) :: l) program [] in
  program
  |> List.map (fun (x, y) -> (x, List.map string_of_abs y))
  |> List.map (fun (x, y) -> ("Label (" ^ x ^ "):", y))
  |> List.map (fun (x, xs) -> x :: xs)
  |> List.map (String.concat "\n")
  |> List.fold_left (fun x y -> x ^ "\n\n" ^ y) ""

let string_of_machine machine = string_of_program machine.program
