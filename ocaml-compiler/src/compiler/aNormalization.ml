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


let (>::) s sl = match s with
  | Some a -> a :: sl
  | None -> sl

let rec merge_lets_aux anf defined tbl = match anf with
  | Term t -> 
      let t, set = merge_lets_aux_term t defined tbl in
      (Term t, set)
  | Bind (lis, Bind ([ (s, t) ], body)) ->
      begin
        Hashtbl.add tbl s ();
        let t, tfree = merge_lets_aux t defined tbl in
        let found = List.fold_left (fun x y -> x || (SS.mem y tfree)) false (fst (List.split lis)) in
        if found then
          let body, _ = merge_lets_aux (Bind ([ (s, t) ], body)) defined tbl in
          (Bind (lis, body), SS.empty)
        else
          merge_lets_aux (Bind ((s, t) :: lis, body)) defined tbl
      end
  | Bind (lis, body) ->
      let body, _ = merge_lets_aux body SS.empty tbl in
      (match lis with
        | (s, t) :: [] ->
            if Hashtbl.mem tbl s then
              (Bind (lis, body), SS.empty)
            else begin
              Hashtbl.add tbl s ();
              let t, _ = merge_lets_aux t SS.empty tbl in
              (Bind ([ (s, t) ], body), SS.empty)
            end
        | _ -> (Bind (lis, body), SS.empty))
  | Branch (p, t1, t2) ->
      let p, s1 = merge_lets_aux_term p defined tbl in
      let t1, s2 = merge_lets_aux t1 defined tbl in
      let t2, s3 = merge_lets_aux t2 defined tbl in
      (Branch (p, t1, t2), s1 |> SS.union s2 |> SS.union s3)
  | TailCall (f, args) ->
      let f, fs = merge_lets_aux_term f defined tbl in
      let args, fargs = 
        args
        |> List.map (fun x -> merge_lets_aux_term x defined tbl)
        |> List.split
      in
      (TailCall (f, args), List.fold_left (fun x y -> SS.union x y) fs fargs)
and merge_lets_aux_term term defined tbl = match term with
  | Lambda (sl, s, body) ->
      let l = s >:: sl in
      let defined = List.fold_left (fun x y -> SS.add y x) defined l in
      let body, free = merge_lets_aux body defined tbl in
      (Lambda (sl, s, body), free)
  | Ref s ->
      let set = if SS.mem s defined then SS.empty else SS.add s SS.empty in
      (term, set)
  | _ -> (term, SS.empty)

  
let merge_lets anf =
  let tbl = Hashtbl.create 8 in
  let res, _ = merge_lets_aux anf SS.empty tbl in
  res

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
