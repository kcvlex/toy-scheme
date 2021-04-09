open AnfType
open SymbolType
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
  | CpsType.ApplyFunc (f, k, args) ->
      let term x = x |> a_normalize |> extract_term in
      let f = term f in
      let args = List.map term args in
      (match k with
        | CpsType.Ref _ -> Call (f, args)
        | CpsType.AdmLambda (x, t) ->
            let x = string_of_sym x in
            let t = a_normalize t in
            let bind = BindCall (x, f, args) in
            Bind ([ bind ], t)
        | _ -> raise (Invalid_argument "DDD"))
  | CpsType.Passing (a, b) -> (match a with
    | CpsType.Ref _ -> a_normalize b
    | _ -> raise (Invalid_argument "CCC"))
  | CpsType.Let ((s, t), body) ->
      let t = t |> a_normalize |> extract_term in
      let bind = BindValue (s, t) in
      Bind ([ bind ], a_normalize body)
  | CpsType.Ref s -> Term (Ref (string_of_sym s))
  | CpsType.Branch (a, b, c) ->
      let a = a |> a_normalize |> extract_term in
      let b = a_normalize b in
      let c = a_normalize c in
      Branch (a, b, c)
  | CpsType.Quote q -> Term (Quote q)
  | CpsType.Nil -> Term Nil
  | _ -> raise (Invalid_argument "RefIndex")


let branch_slot = SlotNumber.make (fun x -> "__br_" ^ (string_of_int x))

let rec lift_branch anf = match anf with
  | Term t -> Term (lift_branch_term t)
  | Bind (lis, body) ->
      let aux1 x = match x with
        | BindValue (x, t) -> BindValue (x, lift_branch_term t)
        | BindCall (x, f, args) ->
            let f = lift_branch_term f in
            let args = List.map lift_branch_term args in
            BindCall (x, f, args)
      in
      let rec aux2 lis = match lis with
        | x :: xs -> (aux1 x) :: (aux2 xs)
        | [] -> []
      in
      let lis = aux2 lis in
      Bind (lis, lift_branch body)
  | Branch (p, e1, e2) ->
      let f e =
        let e = lift_branch e in
        match e with
          | Term t -> (None, Term t)
          | _ ->
              let s = SlotNumber.fresh branch_slot in
              let e = Lambda ([], None, e) in
              let bind = BindValue (s, e) in
              (Some bind, Call (e, []))
      in
      let binding b body = match b with
        | Some b -> Bind ([ b ], body)
        | None -> body
      in
      let p = lift_branch_term p in
      let b1, c1 = f e1 in
      let b2, c2 = f e2 in
      let res = Branch (p, c1, c2) in
      res |> binding b1 |> binding b2
  | Call (f, args) -> Call (lift_branch_term f, List.map lift_branch_term args)
and lift_branch_term term = match term with
  | Lambda (a, b, body) -> Lambda (a, b, lift_branch body)
  | _ -> term

let rec merge_lets_aux anf = match anf with
  | Term t -> Term (merge_lets_term t)
  | Bind (lis, Bind ([ b ], body)) ->
      let lis = b :: lis in
      merge_lets_aux (Bind (lis, body))
  | Bind (_, Bind _) -> raise (Invalid_argument "Bind must be 1")
  | Bind (lis, body) ->
      let aux1 b = match b with
        | BindValue (s, t) -> BindValue (s, merge_lets_term t)
        | BindCall (s, f, args) -> BindCall (s, merge_lets_term f, List.map merge_lets_term args)
      in
      let rec aux2 lis = match lis with
        | x :: xs -> (aux1 x) :: (aux2 xs)
        | [] -> []
      in
      let lis = List.rev (aux2 lis) in
      let body = merge_lets_aux body in
      Bind (lis, body)
  | Branch (p, t1, t2) -> Branch (merge_lets_term p, merge_lets_aux t1, merge_lets_aux t2)
  | Call (f, args) -> Call (merge_lets_term f, List.map merge_lets_term args)
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
      let aux1 x = match x with
        | BindValue (s, v) ->
            let v, l = bind_lambda_term true v in
            List.append l [ BindValue (s, v) ]
        | BindCall (s, f, args) ->
            let f, lf = bind_lambda_term false f in
            let a, la = 
              args
              |> List.map (bind_lambda_term false)
              |> List.split
            in
            let lis = List.flatten (lf :: la) in
            List.append lis [ BindCall (s, f, a) ]
      in
      let rec aux2 lis = match lis with
        | x :: xs -> (aux1 x) :: (aux2 xs)
        | [] -> []
      in
      let slis = List.flatten (aux2 slis) in
      let b, bl = bind_lambda_aux false body in
      let lis = List.append slis bl in
      (Bind (lis, b), [])
  | Branch (p, t1, t2) ->
      let t1, l1 = bind_lambda_aux false t1 in
      let t2, l2 = bind_lambda_aux false t2 in
      let p, l3 = bind_lambda_term false p in
      (Branch (p, t1, t2), l1 |> List.append l2 |> List.append l3)
  | Call (f, args) ->
      let f, lf = bind_lambda_term false f in
      let args, largs = List.split (List.map (bind_lambda_term false) args) in
      let l = List.fold_left List.append lf largs in
      (Call (f, args), l)
and bind_lambda_term named term = match term with
  | Lambda (a, b, body) ->
      let body, bf = bind_lambda_aux false body in
      let body = match bf with 
        | [] -> body
        | _ -> Bind (bf, body)
      in
      let lambda = Lambda (a, b, body) in
      if named then 
        (lambda, [])
      else
        let name = SlotNumber.fresh lambda_name_slot in
        (Ref name, [ BindValue (name, lambda) ])
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
      let sl = List.map ast_of_bind sl in
      AstType.Let (sl, ast_of_anf body)
  | Branch (a, b, c) -> AstType.Branch (ast_of_term a, ast_of_anf b, ast_of_anf c)
  | Call (a, b) -> AstType.Apply (ast_of_term a, List.map ast_of_term b)
and ast_of_term term = match term with
  | Int i -> AstType.Num i
  | Bool b -> AstType.Bool b
  | Primitive p -> AstType.Symbol (PrimitiveSym p)
  | Ref s -> AstType.Symbol (CommonSym s)
  | Nil -> AstType.Nil
  | Quote q -> AstType.Quote q
  | Lambda (args, larg, body) -> AstType.Lambda (args, larg, ast_of_anf body)
and ast_of_bind bind = match bind with
  | BindValue (s, v) -> (s, ast_of_term v)
  | BindCall (s, f, l) -> 
      let f = ast_of_term f in
      let l = List.map ast_of_term l in
      (s, AstType.Apply (f, l))
