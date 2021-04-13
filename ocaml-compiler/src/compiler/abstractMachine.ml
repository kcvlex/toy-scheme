open SymbolType
open Symbol
open AbstractMachineType

type vstore = (string, data_type) Hashtbl.t

type frame_type = {
  cur : AbstractMachineType.instr_type list;
  mem : vstore;
}

type t = {
  frames : frame_type Stack.t;
  globs : vstore;
  funcs : func_table;
  jumps : jump_table;
}

let entry_label = "entry"
let rv_sym = "__RV"

let rec translate_proc labels clo = match clo with
  | ClosureType.Term t ->
      let v, vl = translate_value labels t in
      List.append vl [ Return v ]
  | ClosureType.Bind (lis, body) ->
      let rec aux lis = match lis with
        | (s, t) :: xs ->
            let v, vl = translate_value labels t in
            let xs = aux xs in
            List.flatten [ vl; [ Bind (Local s, v) ]; xs ]
        | [] -> []
      in
      let lis = aux lis in
      let body = translate_proc labels body in
      List.append lis body
  | ClosureType.Branch (p, e1, e2) ->
      let p, pl = translate_value labels p in
      let e1 = translate_proc labels e1 in
      let e2 = translate_proc labels e2 in
      List.append pl [ Test (p, e1, e2) ]
and translate_value labels value = match value with
  | ClosureType.Int i -> (Int i, [])
  | ClosureType.Bool b -> (Bool b, [])
  | ClosureType.Primitive s -> (Primitive s, [])
  | ClosureType.Call (f, args) ->
      (* Call doesn't nest *)
      let f, fl = translate_value labels f in
      let a, al =
        let l = List.map (translate_value labels) args in
        let b, bl = List.split l in
        (b, List.flatten bl)
      in
      let lis = List.append fl al in
      (match f with
        | Primitive p when p != APPLY -> (PrimCall (p, a), lis)
        | _ -> (Ref RV, List.append lis [ Call (f, a) ]))
  | ClosureType.Closure (s, l) ->
      let a, al =
        let l = List.map (translate_value labels) l in
        let b, bl = List.split l in
        (b, List.flatten bl)
      in
      assert (List.length al = 0);
      (Cons (Ref (Local s), Allocate a), [])
  | ClosureType.Var "DUMMY" -> (Nil, [])
  | ClosureType.Var s ->
      let s = 
        if Hashtbl.mem labels s then Label s else Ref (Local s)
      in
      (s, [])
  | ClosureType.Nil -> (Nil, [])
  | ClosureType.Quote a -> (Quote a, [])
  | ClosureType.ClosureRef _ -> (translate_closure value [], [])
  | _ -> raise (Invalid_argument "UNIMPLED")
and translate_closure c il = match c with
  | ClosureType.ClosureRef (cn, i) -> translate_closure cn (i :: il)
  | ClosureType.Var s -> AccessClosure (s, il)
  | _ -> raise (Invalid_argument "AAA")

let make_machine (funcs, jumps) =
  let frame = {
    cur = (fun (_, _, _, body) -> body) (Hashtbl.find funcs entry_label);
    mem = Hashtbl.create 8;
  }
  in
  let frames = Stack.create () in
  Stack.push frame frames;
  {
    frames;
    globs = Hashtbl.create 0;
    funcs;
    jumps;
  }

let translate (clsr_prog: ClosureType.t) =
  let sz = List.length clsr_prog.procs in
  let names = fst (List.split clsr_prog.procs) in
  let labels =
    let tbl = Hashtbl.create (sz + 1) in
    List.iter (fun x -> Hashtbl.add tbl x ()) (entry_label :: names); tbl
  in
  let funcs, jumps =
    let format_args c l opt =
      let c = Local c in
      let l = List.map (fun x -> Local x) l in
      let opt = Option.map (fun x -> Local x) opt in
      (Some c, l, opt)
    in
    let format_proc (c, l, opt, body) =
      let c, l, opt = format_args c l opt in
      let proc = translate_proc labels body in
      (c, l, opt, proc)
    in
    let tp =
      clsr_prog.procs
      |> List.split
      |> snd
      |> List.map format_proc
    in
    let funcs =
      let tbl = Hashtbl.create sz in
      let funcs = List.combine names tp in
      List.iter (fun (x, y) -> Hashtbl.add tbl x y) funcs;
      tbl
    in
    let body = match clsr_prog.body with
      | ClosureType.Bind (l, ClosureType.Term t) ->
          let t = ClosureType.Call (t, []) in
          ClosureType.Bind (l, ClosureType.Term t)
      | _ -> raise (Invalid_argument "body")
    in
    let entry = (None, [], None, translate_proc labels body) in
    Hashtbl.replace funcs entry_label entry;
    (funcs, Hashtbl.create 0)
  in
  make_machine (funcs, jumps)

let restrict_1arg lis = match lis with
  | x :: [] -> x
  | _ -> raise (Invalid_argument "must be 1 argument")
      
let restrict_2arg lis = match lis with
  | x1 :: x2 :: [] -> (x1, x2)
  | _ -> raise (Invalid_argument "must be 2 argument")

let rec follow_dlist v idxlis = match (v, idxlis) with
  | (DList (x :: xs), y :: ys) ->
      if y = 0 then follow_dlist x ys else follow_dlist (DList xs) ((y - 1) :: ys)
  | (_, []) -> v
  | _ -> raise (Invalid_argument "follow_dlist")

let rec follow_dcons v idxlis = match (v, idxlis) with
  | (DCons (car, cdr), x :: xs) ->
      if x = 0 then follow_dcons car xs else follow_dcons cdr ((x - 1) :: xs)
  | (_, []) -> v
  | _ -> raise (Invalid_argument "follow_dcons")

let list_of_dcons cons =
  let rec aux cons = match cons with
    | DCons (car, cdr) -> car :: (aux cdr)
    | _ -> []
  in
  match cons with
    | DCons _ -> aux cons
    | _ -> raise (Invalid_argument "not dcons")

let rec dcons_of_list lis = match lis with
  | x :: xs -> DCons (x, dcons_of_list xs)
  | [] -> DNil

let rec string_of_data d = match d with
  | DInt i -> string_of_int i
  | DBool true -> "#t"
  | DBool false -> "#f"
  | DLabel s -> Printf.sprintf "LABEL(%s)" s
  | DNil -> "()"
  | DCons _ ->
      let rec aux lis = match lis with
        | x :: DNil :: [] -> [ string_of_data x ]
        | x :: y :: [] -> [ string_of_data x; "."; string_of_data y ]
        | x :: xs -> (string_of_data x) :: (aux xs)
        | [] -> raise (Invalid_argument "DCons")
      in
      d |> list_of_dcons
        |> aux 
        |> String.concat " "
        |> Printf.sprintf "(%s)"
  | DList l -> l |> List.map string_of_data |> String.concat " " |> Printf.sprintf "[ %s ]"
  | DPrim p -> string_of_sym (PrimitiveSym p)
  | DQuote a -> Ast.code_of_ast a

let make_instr_str indent label lis =
  lis |> String.concat ", "
      |> Printf.sprintf "%s%s (%s)" indent label

let string_of_ref r = match r with
  | Global s -> s
  | Local s -> s
  | RV -> rv_sym

let rec string_of_value value = match value with
  | Int i -> string_of_int i
  | Bool true -> "#t"
  | Bool false -> "#f"
  | Primitive p -> string_of_sym (PrimitiveSym p)
  | PrimCall (p, vl) ->
      let p = string_of_sym (PrimitiveSym p) in
      let vl = List.map string_of_value vl in
      (p :: vl) |> String.concat " " |> Printf.sprintf "(%s)"
  | Label s -> Printf.sprintf "LABEL (%s)" s
  | Ref r -> string_of_ref r
  | Allocate lis -> make_instr_str "" "ALLOCATE" (List.map string_of_value lis)
  | Nil -> "nil"
  | Quote a -> Printf.sprintf "QUOTE (%s)" (Ast.code_of_ast a)
  | Cons (car, cdr) -> make_instr_str "" "CONS" (List.map string_of_value [ car; cdr ])
  | AccessClosure (s, il) -> 
      il |> List.map string_of_int
         |> fun x -> s :: x
         |> make_instr_str "" "ACCESS"

let rec string_of_instr indent instr = match instr with
  | Bind (s, v) -> make_instr_str indent "BIND" [ string_of_ref s; string_of_value v ]
  | Test (p, e1, e2) ->
      let p = string_of_value p in
      let nindent = "  " ^ indent in
      let lk = indent ^ "[" in
      let rk = indent ^ "]" in
      let test_w = "     " in
      let f e =
        e |> List.map (string_of_instr nindent)
          |> fun x -> List.append (lk :: x) [ rk ]
          |> List.map (fun x -> test_w ^ x)
          |> String.concat "\n"
      in
      let e1 = f e1 in
      let e2 = f e2 in
      let lis = [
        Printf.sprintf "%s%s (%s" indent "TEST" p;
        e1;
        e2;
        Printf.sprintf "%s     )" indent
      ]
      in
      String.concat "\n" lis
  | Return v -> make_instr_str indent "RETURN" [ string_of_value v ]
  | Jump v -> make_instr_str indent "JUMP" [ string_of_value v ]
  | Call (f, args) -> make_instr_str indent "CALL" (List.map string_of_value (f :: args))

let string_of_abs abs = string_of_instr "" abs

let string_of_program program =
  let funcs, jumps = program in
  let seq = Hashtbl.fold (fun k (_, _, _, body) l -> (k, body) :: l) funcs [] in
  let seq = Hashtbl.fold (fun k body l -> (k, body) :: l) jumps seq in
  seq
  |> List.map (fun (x, y) -> (x, List.map string_of_abs y))
  |> List.map (fun (x, y) -> ("Label (" ^ x ^ "):", y))
  |> List.map (fun (x, xs) -> x :: xs)
  |> List.map (String.concat "\n")
  |> List.fold_left (fun x y -> x ^ "\n\n" ^ y) ""

let string_of_machine machine = string_of_program (machine.funcs, machine.jumps)

let call_prim p args =
  let extract_int v = match v with
    | DInt i -> i
    | _ -> raise (Invalid_argument "must be DInt")
  in
  match p with
    | ADD | SUB | MUL | DIV ->
        let args = List.map extract_int args in
        let fold = match p with
          | ADD -> fun a b -> a + b
          | SUB -> fun a b -> a - b
          | MUL -> fun a b -> a * b
          | DIV -> fun a b -> a / b
        in
        let hd, tl = (List.hd args, List.tl args) in
        let res = List.fold_left (fun a b -> fold a b) hd tl in
        DInt res
    | EQ ->
        let a, b = restrict_2arg args in
        DBool (a = b)
    | LESS ->
        let args = List.map extract_int args in
        let a, b = restrict_2arg args in
        DBool (a < b)
    | NULL ->
        let a = restrict_1arg args in
        DBool (a = DNil)
    | CONS ->
        let a, b = restrict_2arg args in
        DCons (a, b)
    | CAR -> (match args with
      | (DCons (car, _)) :: [] -> car
      | _ -> raise (Invalid_argument "CAR"))
    | CDR -> (match args with
      | (DCons (_, cdr)) :: [] -> cdr
      | _ -> raise (Invalid_argument "CDR"))
    | LIST -> dcons_of_list args
    | LISTREF ->
        let a, b = restrict_2arg args in
        let b = extract_int b in
        follow_dcons a [ b ]
    | APPLY | MAP -> raise (Invalid_argument "APPLY")
    | DISPLAY ->
        let a = restrict_1arg args in
        print_endline (string_of_data a);
        DNil

let rec eval_call machine f args = match f with
  | DPrim APPLY ->
      let f, args = restrict_2arg args in
      let args = list_of_dcons args in
      eval_call machine f args
  | DPrim p -> 
      let v = call_prim p args in
      Hashtbl.replace machine.globs rv_sym v
  | DLabel f ->
      let (cl, cargs, earg, body) = Hashtbl.find machine.funcs f in
      let cargs = match cl with
        | Some c -> c :: cargs
        | None -> cargs
      in
      let mem = Hashtbl.create (List.length cargs) in
      let rec aux clis alis = match (clis, alis) with
        | (x :: xs, y :: ys) ->
            let () = match x with
              | Global s -> Hashtbl.replace machine.globs s y
              | Local s -> Hashtbl.replace mem s y
              | RV -> Hashtbl.replace machine.globs rv_sym y
            in
            aux xs ys
        | ([], ys) -> ys
        | _ -> raise (Invalid_argument "eval_call_frame1")
      in
      let rem = aux cargs args in
      let () = match (earg, rem) with
        | (Some (Local c), _) -> Hashtbl.replace mem c (dcons_of_list rem)
        | (None, []) -> ()
        | _ -> raise (Invalid_argument "eval_call_frame2")
      in
      let frame = { mem; cur = body } in
      Stack.push frame machine.frames
  | DCons (f, (DList _ as c)) -> eval_call machine f (c :: args)
  | _ -> raise (Invalid_argument "eval_call_frame3")

let rec eval_value machine mem v = match v with
  | Int i -> DInt i
  | Bool b -> DBool b
  | Primitive p -> DPrim p
  | Label s -> DLabel s
  | Nil -> DNil
  | Quote a -> DQuote a
  | PrimCall (p, args) ->
      let args = List.map (eval_value machine mem) args in
      let v = call_prim p args in
      v
  | Ref (Global s) ->
      (* FIXME *)
      if Hashtbl.mem machine.funcs s then
        DLabel s
      else if Hashtbl.mem machine.jumps s then
        DLabel s
      else if Hashtbl.mem machine.globs s then
        Hashtbl.find machine.globs s 
      else 
        DNil
  | Ref (Local s) -> 
      if Hashtbl.mem machine.funcs s then
        DLabel s
      else if Hashtbl.mem machine.jumps s then
        DLabel s
      else 
        Hashtbl.find mem s
  | Ref RV -> if Hashtbl.mem machine.globs rv_sym then Hashtbl.find machine.globs rv_sym else DNil
  | Allocate l -> DList (List.map (eval_value machine mem) l)
  | Cons (car, cdr) ->
      let car = eval_value machine mem car in
      let cdr = eval_value machine mem cdr in
      DCons (car, cdr)
  | AccessClosure (s, il) ->
      let s = Hashtbl.find mem s in
      follow_dlist s il

let eval_step machine = 
  let { mem; cur } = Stack.pop machine.frames in
  match cur with
    | [] -> raise (Invalid_argument "must return")
    | hd :: tl -> match hd with
      | Bind (s, v) ->
          let v = eval_value machine mem v in
          let () = match s with
            | Global s -> Hashtbl.replace machine.globs s v
            | Local s -> Hashtbl.replace mem s v
            | RV -> Hashtbl.replace machine.globs rv_sym v
          in
          Stack.push { mem; cur = tl } machine.frames
      | Test (p, e1, e2) -> (match tl with
        | [] ->
            let p = match eval_value machine mem p with
              | DBool b -> b
              | _ -> raise (Invalid_argument "Condition must be bool")
            in
            let next = if p then e1 else e2 in
            Stack.push { mem; cur = next } machine.frames
        | _ -> raise (Invalid_argument "Test"))
      | Return v ->
          let v = eval_value machine mem v in
          Hashtbl.replace machine.globs rv_sym v
      | Jump v ->
          let v = eval_value machine mem v in
          (match v with
            | DLabel s ->
                let body = Hashtbl.find machine.jumps s in
                Stack.push { mem; cur = body } machine.frames
            | _ -> raise (Invalid_argument "Jump"))
      | Call (f, args) ->
          let f = eval_value machine mem f in
          let args = List.map (eval_value machine mem) args in
          Stack.push { mem; cur = tl } machine.frames;
          eval_call machine f args

let rec eval machine =
  if Stack.is_empty machine.frames then
    ()
  else begin
    eval_step machine;
    eval machine
  end


(* FIXME : duplicate labels *)
let link p1 p2 =
  let f1, j1 = p1 in
  let f2, j2 = p2 in
  let f = Hashtbl.create (Hashtbl.length f1 + Hashtbl.length f2) in
  let j = Hashtbl.create (Hashtbl.length j1 + Hashtbl.length j2) in
  let add_f h =
    Hashtbl.iter (fun x y -> Hashtbl.add f x y) h
  in
  let add_j h = 
    Hashtbl.iter (fun x y -> Hashtbl.add j x y) h
  in
  add_f f1; add_f f2;
  add_j j1; add_j j2;
  (f, j)

let call_and_print name f args =
  let call = Call (Label f, args) in
  let display = Call (Primitive DISPLAY, [ Ref RV ]) in
  let seq = [ call; display; Return (Int 0) ] in
  let ftbl = Hashtbl.create 1 in
  Hashtbl.add ftbl name (None, [], None, seq);
  let jtbl = Hashtbl.create 0 in
  (ftbl, jtbl)
