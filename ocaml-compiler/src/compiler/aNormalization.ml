open ANType
open Symbol

let extract_term term = match term with
  | Term t -> t
  | _ -> raise (Invalid_argument "term must be Term")

let rec a_normalize cps = match cps with
  | CpsType.Int i -> Term (Int i)
  | CpsType.Bool b -> Term (Bool b)
  | CpsType.AdmLambda (k, body) -> a_normalize (CpsType.Lambda (k, [], None, body))
  | CpsType.Lambda (k, args, larg, body) ->
      let args = List.map string_of_sym args in
      let larg = Option.map string_of_sym larg in
      Term (Lambda (args, larg, a_normalize body))
  | CpsType.ApplyFunc (f, k, args) ->
      let f, args =
        (f :: args) |> List.map a_normalize
                    |> List.map extract_term
                    |> fun l -> (List.hd l, List.tl l)
      in
      (match k with
        | CpsType.Ref _ -> TailCall (f, args)
        | CpsType.AdmLambda (k, body) ->
            Call ((string_of_sym k, f, args), a_nomalize body)
        | CpsType.Lambda (k, lambda_args, lambda_larg, body) ->
            (* FIXME : unreachable ?? *)
            let k = string_of_sym k in
            let lambda_args = List.map string_of_sym lambda_args in
            let lambda_larg = Option.map string_of_sym larg in
            let body = a_normalize body in
            Call ((k, f, args), Lambda (lambda_args, lambda_larg, body))
        | _ -> raise (Invalid_argument "BBB"))
  | CpsType.Passing (a, b) -> (match a with
    | CpsType.Ref _ -> a_normalize b
    | _ -> raise (Invalid_argument "CCC"))
  | CpsType.Let ((s, t), body) -> 
      let t = t |> a_normalize |> extract_term in
      Bind ((s, t), a_normalize body)
  | CpsType.Ref s -> Term (Ref string_of_sym s)
  | CpsType.Branch (a, b, c) ->
      let a = a |> a_normalize |> extract_term in
      let b = a_normalize b in
      let c = a_normalize c in
      Branch (a, b, c)
  | _ -> raise (Invalid_argument "RefIndex")


let rec ast_of_anorm an = match an with
  | Term t -> ast_of_term t
  | Bind ((s, t), body) -> AstType.Let([ (s, ast_of_term t) ], ast_of_anorm body)
  | Branch (a, b, c) -> AstType.Branch (ast_of_term a, ast_of_anorm b, ast_of_anorm c)
  | TailCall (a, b) -> AstType.Apply (ast_of_term a, List.map ast_of_term b)
  | Call ((s, f, args), body) ->
      let body = ast_of_anorm body in
      let f = ast_of_term f in
      let args = List.map ast_of_term args in
      f |> fun x -> AstType.Apply (x, args)
        |> fun x -> (s, x)
        |> fun x -> AstType.Let ([ x ], body)
and ast_of_term term = match term with
  | Int i -> AstType.Num i
  | Bool b -> AstType.Bool b
  | Primitive s -> AstType.Primitive s
  | Ref s -> AstType.Symbol s
  | Lambda (args, larg, body) -> AstType.Lambda (args, larg, ast_of_anorm body)
