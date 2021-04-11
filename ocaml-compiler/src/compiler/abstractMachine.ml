open SymbolType
open Symbol
open AbstractMachineType

type frame_type = {
  cur : AbstractMachineType.instr_type list;
  mem : (string, data_type) Hashtbl.t;
}

type t = {
  frames : frame_type Stack.t;
  mutable ra : data_type option;
  funcs : func_table;
  jumps : jump_table;
}

let entry_label = "entry"

let rec translate_proc labels clo = match clo with
  | ClosureType.Term t ->
      let v, vl = translate_value labels t in
      List.append [ Return v ] vl
  | ClosureType.Bind (lis, body) ->
      let rec aux lis = match lis with
        | (s, t) :: xs ->
            let v, vl = translate_value labels t in
            let xs = aux xs in
            List.flatten [ vl; [ Bind (s, v) ]; xs ]
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
        | _ -> (RV, List.append lis [ Call (f, a) ]))
  | ClosureType.Closure (s, l) ->
      let a, al =
        let l = List.map (translate_value labels) l in
        let b, bl = List.split l in
        (b, List.flatten bl)
      in
      assert (List.length al = 0);
      (Cons (Ref s, Allocate a), [])
  | ClosureType.Var "DUMMY" -> (Nil, [])
  | ClosureType.Var s ->
      let s = 
        if Hashtbl.mem labels s then Label s else Ref s
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

let translate (clsr_prog: ClosureType.t) =
  let sz = List.length clsr_prog.procs in
  let names = fst (List.split clsr_prog.procs) in
  let labels =
    let tbl = Hashtbl.create (sz + 1) in
    List.iter (fun x -> Hashtbl.add tbl x ()) (entry_label :: names); tbl
  in
  let funcs, jumps =
    let tp =
      clsr_prog.procs
      |> List.split
      |> snd
      |> List.map (fun (c, l, opt, body) -> (Some c, l, opt, translate_proc labels body))
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
  let frame = {
    cur = (fun (_, _, _, body) -> body) (Hashtbl.find funcs entry_label);
    mem = Hashtbl.create 8;
  }
  in
  let frames = Stack.create () in
  Stack.push frame frames;
  {
    frames;
    ra = None;
    funcs;
    jumps;
  }

let restrict_1arg lis = match lis with
  | x :: [] -> x
  | _ -> raise (Invalid_argument "must be 1 argument")
      
let restrict_2arg lis = match lis with
  | x1 :: x2 :: [] -> (x1, x2)
  | _ -> raise (Invalid_argument "must be 2 argument")

let rec follow_dcons v idxlis = match (v, idxlis) with
  | (DCons (car, cdr), x :: xs) -> 
      if x = 0 then follow_dcons car xs else follow_dcons cdr ((x - 1) :: xs)
  | (_, []) -> v
  | _ -> raise (Invalid_argument "follow_cons")

let rec list_of_dcons cons = match cons with
  | DCons (car, cdr) -> car :: (list_of_dcons cdr)
  | _ -> [ cons ]

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
  | DPrim p -> string_of_sym (PrimitiveSym p)
  | DQuote a -> Ast.code_of_ast a

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
        let res = List.fold_left (fun a b -> fold b a) hd tl in
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

let make_call_frame machine f args = match f with
  | DLabel f ->
      let (cl, cargs, earg, body) = Hashtbl.find machine.funcs f in
      let cargs = match cl with
        | Some c -> c :: cargs
        | None -> cargs
      in
      let mem = Hashtbl.create (List.length cargs) in
      let rec aux clis alis = match (clis, alis) with
        | (x :: xs, y :: ys) ->
            Hashtbl.replace mem x y;
            aux xs ys
        | ([], []) -> []
        | ([], ys) -> ys
        | _ -> raise (Invalid_argument "make_call_frame")
      in
      let rem = aux cargs args in
      let () = match (earg, rem) with
        | (Some c, _) -> Hashtbl.replace mem c (dcons_of_list args)
        | (None, []) -> ()
        | _ -> raise (Invalid_argument "make_call_frame")
      in
      { mem; cur = body }
  | _ -> raise (Invalid_argument "make_call_frame")

let rec eval_value machine mem v = match v with
  | Int i -> DInt i
  | Bool b -> DBool b
  | Primitive p -> DPrim p
  | Label s -> DLabel s
  | Nil -> DNil
  | Quote a -> DQuote a
  | PrimCall (p, args) ->
      let args = List.map (eval_value machine mem) args in
      call_prim p args
  | Ref s -> Hashtbl.find mem s
  | Allocate l -> dcons_of_list (List.map (eval_value machine mem) l)
  | RV -> Option.get machine.ra
  | Cons (car, cdr) ->
      let car = eval_value machine mem car in
      let cdr = eval_value machine mem cdr in
      DCons (car, cdr)
  | AccessClosure (s, il) ->
      let s = Hashtbl.find mem s in
      follow_dcons s il

let eval_step machine = 
  let { mem; cur } = Stack.pop machine.frames in
  let frame = match cur with
    | [] -> raise (Invalid_argument "must return")
    | hd :: tl -> match hd with
      | Bind (s, v) ->
          let v = eval_value machine mem v in
          Hashtbl.replace mem s v;
          { mem; cur = tl }
      | Test (p, e1, e2) -> (match tl with
        | [] ->
            let p = match eval_value machine mem p with
              | DBool b -> b
              | _ -> raise (Invalid_argument "Condition must be bool")
            in
            let next = if p then e1 else e2 in
            { mem; cur = next }
        | _ -> raise (Invalid_argument "Test"))
      | Return v ->
          let v = eval_value machine mem v in
          machine.ra <- Some v;
          { mem; cur = [] }
      | Jump v ->
          let v = eval_value machine mem v in
          (match v with
            | DLabel s ->
                let body = Hashtbl.find machine.jumps s in
                { mem; cur = body }
            | _ -> raise (Invalid_argument "Jump"))
      | Call (f, args) ->
          let f = eval_value machine mem f in
          (match f with
            | DPrim APPLY ->
                let args = List.map (eval_value machine mem) args in
                let f, args = restrict_2arg args in
                let args = list_of_dcons args in
                Stack.push { mem; cur = tl } machine.frames;
                make_call_frame machine f args
            | DPrim p ->
                let v = eval_value machine mem (PrimCall (p, args)) in
                machine.ra <- Some v;
                { mem; cur = tl }
            | _ ->
              let args = List.map (eval_value machine mem) args in
              Stack.push { mem; cur = tl } machine.frames;
              make_call_frame machine f args)
  in
  Stack.push frame machine.frames

let rec eval machine =
  if Stack.is_empty machine.frames then
    ()
  else begin
    eval_step machine;
    eval machine
  end

let make_instr_str indent label lis =
  lis |> String.concat ", "
      |> Printf.sprintf "%s%s (%s)" indent label

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
  | Ref s -> s
  | Allocate lis -> make_instr_str "" "ALLOCATE" (List.map string_of_value lis)
  | RV -> "RV"
  | Nil -> "nil"
  | Quote a -> Printf.sprintf "QUOTE (%s)" (Ast.code_of_ast a)
  | Cons (car, cdr) -> make_instr_str "" "CONS" (List.map string_of_value [ car; cdr ])
  | AccessClosure (s, il) -> 
      il |> List.map string_of_int
         |> fun x -> s :: x
         |> make_instr_str "" "ACCESS"

let rec string_of_instr indent instr = match instr with
  | Bind (s, v) -> make_instr_str indent "BIND" [ s; string_of_value v ]
  | Test (p, e1, e2) ->
      let p = string_of_value p in
      let nindent = "  " ^ indent in
      let f e =
        e |> List.map (string_of_instr nindent)
          |> String.concat "\n"
          |> Printf.sprintf "[ %s ]"
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
