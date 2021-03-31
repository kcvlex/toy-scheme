open AnfType
open Symbol

module SS = Set.Make(String)

let extract_term term = match term with
  | Term t -> t
  | _ -> raise (Invalid_argument "term must be Term")

let rec a_normalize cps = match cps with
  | CpsType.Int i -> Term (Int i)
  | CpsType.Bool b -> Term (Bool b)
  | CpsType.AdmLambda (k, body) -> a_normalize (CpsType.Lambda (k, [], None, body))
  | CpsType.Lambda (_, args, larg, body) ->
      let args = List.map string_of_sym args in
      let larg = Option.map string_of_sym larg in
      Term (Lambda (args, larg, a_normalize body))
  | CpsType.ApplyFunc (Ref (Primitive s), k, args) ->
      let args = 
        args |> List.map a_normalize 
             |> List.map extract_term
      in
      normalize_app (AnfType.Primitive s) k args
  | CpsType.ApplyFunc (f, k, args) ->
      let f, args =
        (f :: args) |> List.map a_normalize
                    |> List.map extract_term
                    |> fun l -> (List.hd l, List.tl l)
      in
      normalize_app f k args
  | CpsType.Passing (a, b) -> (match a with
    | CpsType.Ref _ -> a_normalize b
    | _ -> raise (Invalid_argument "CCC"))
  | CpsType.Let ((s, t), body) ->
      let t = a_normalize t in
      Bind ([ (s, t) ], a_normalize body)
  | CpsType.Ref s -> Term (Ref (string_of_sym s))
  | CpsType.Branch (a, b, c) ->
      let a = a |> a_normalize |> extract_term in
      let b = a_normalize b in
      let c = a_normalize c in
      Branch (a, b, c)
  | CpsType.Quote q -> Term (Quote q)
  | CpsType.Nil -> Term Nil
  | _ -> raise (Invalid_argument "RefIndex")
and normalize_app f k args = match k with
  | CpsType.Ref _ -> TailCall (f, args)
  | CpsType.AdmLambda (t, body) ->
      TailCall (f, args)
      |> fun x -> [ (string_of_sym t, x) ]
      |> fun x -> Bind (x, a_normalize body)
  | _ -> raise (Invalid_argument "Invalid Continuation")


let branch_slot = SlotNumber.make (fun x -> "__br_" ^ (string_of_int x))

let rec lift_branch anf = match anf with
  | Term t -> Term (lift_branch_term t)
  | Bind (lis, body) ->
      let a, b = List.split lis in
      let b = List.map lift_branch b in
      let lis = List.combine a b in
      Bind (lis, lift_branch body)
  | Branch (p, t1, t2) ->
      let f t =
        let s = SlotNumber.fresh branch_slot in
        let t = 
          t |> lift_branch
            |> fun x -> Term (Lambda ([], None, x))
        in
        let call = TailCall (Ref s, []) in
        (s, t, call)
      in
      let s1, t1, c1 = f t1 in
      let s2, t2, c2 = f t2 in
      let p = lift_branch_term p in
      Bind ([ (s1, t1) ], Bind ([ (s2, t2) ], Branch (p, c1, c2)))
  | TailCall (f, args) -> TailCall (lift_branch_term f, List.map lift_branch_term args)
and lift_branch_term term = match term with
  | Lambda (a, b, body) -> Lambda (a, b, lift_branch body)
  | _ -> term

let rec merge_lets_aux anf = match anf with
  | Term t -> Term (merge_lets_term t)
  | Bind (lis, Bind ([ (s, t) ], body)) ->
      let lis = (s, t) :: lis in
      merge_lets_aux (Bind (lis, body))
  | Bind (lis, Bind _) -> raise (Invalid_argument "Bind must be 1")
  | Bind (lis, body) ->
      let a, b = List.split lis in
      let lis =
        b |> List.map merge_lets_aux
          |> List.combine a
          |> List.rev
      in
      let body = merge_lets_aux body in
      Bind (lis, body)
  | Branch (p, t1, t2) -> Branch (merge_lets_term p, merge_lets_aux t1, merge_lets_aux t2)
  | TailCall (f, args) -> TailCall (merge_lets_term f, List.map merge_lets_term args)
and merge_lets_term term = match term with
  | Lambda (a, b, body) -> Lambda (a, b, merge_lets_aux body)
  | _ -> term
let merge_lets anf = anf |> lift_branch |> merge_lets_aux


let lambda_name_slot = SlotNumber.make (fun x -> "__f_" ^ (string_of_int x))

let rec bind_lambda_aux named anf = match anf with
  | Term t -> 
      let t, l = bind_lambda_term named t in
      (Term t, l)
  | Bind (slis, body) ->
    let rec f l = match l with
      | (s, t) :: xs ->
          let t, tlis = bind_lambda_aux true t in
          let tlis = List.append tlis [ (s, t) ] in
          let xs = f xs in
          List.append tlis xs
      | [] -> []
    in
    let slis = f slis in
    let body, bf = bind_lambda_aux false body in
    (Bind (List.append slis bf, body), [])
  | Branch (p, t1, t2) ->
      let t1, l1 = bind_lambda_aux false t1 in
      let t2, l2 = bind_lambda_aux false t2 in
      let p, l3 = bind_lambda_term false p in
      (Branch (p, t1, t2), l1 |> List.append l2 |> List.append l3)
  | TailCall (f, args) ->
      let f, lf = bind_lambda_term false f in
      let args, largs = List.split (List.map (bind_lambda_term false) args) in
      let l = List.fold_left List.append lf largs in
      (TailCall (f, args), l)
and bind_lambda_term named term = match term with
  | Lambda (a, b, body) ->
      let body, bf = bind_lambda_aux false body in
      let body = match bf with | [] -> body
        | _ -> Bind (bf, body)
      in
      let lambda = Lambda (a, b, body) in
      if named then 
        (lambda, [])
      else
        let name = SlotNumber.fresh lambda_name_slot in
        (Ref name, [ (name, Term lambda) ])
  | _ -> (term, [])

let bind_lambda anf =
  let anf, l = bind_lambda_aux false anf in
  match l with
    | [] -> anf
    | _ -> Bind (l, anf)


let normalize_binds anf = anf |> merge_lets |> bind_lambda

let rec ast_of_anf an = match an with
  | Term t -> ast_of_term t
  | Bind (sl, body) ->
      let sl = List.map (fun x -> (fst x, ast_of_anf (snd x))) sl in
      AstType.Let (sl, ast_of_anf body)
  | Branch (a, b, c) -> AstType.Branch (ast_of_term a, ast_of_anf b, ast_of_anf c)
  | TailCall (a, b) -> AstType.Apply (ast_of_term a, List.map ast_of_term b)
and ast_of_term term = match term with
  | Int i -> AstType.Num i
  | Bool b -> AstType.Bool b
  | Primitive s -> AstType.Primitive s
  | Ref s -> AstType.Symbol s
  | Nil -> AstType.Nil
  | Quote q -> AstType.Quote q
  | Lambda (args, larg, body) -> AstType.Lambda (args, larg, ast_of_anf body)
