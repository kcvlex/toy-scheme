open ClosureType
open SymbolType
open Symbol

module OrderedSym = struct
  type t = SymbolType.t

  let to_pair sym = match sym with
    | CommonSym s -> (0, s)
    | Primitive s -> (1, s)
    | ContSym (Cont n) -> (2, string_of_int n)
    | ParamSym n -> (3, string_of_int n)
    | RecordSym (Record n) -> (4, string_of_int n)

  let compare t1 t2 =
    let t1 = to_pair t1 in
    let t2 = to_pair t2 in
    match (t1, t2) with
      | ((a, b), (c, d)) -> if a != c then Int.compare a c else String.compare b d
end

module SymSet = Set.Make(OrderedSym)

let get_sym b = match b with
  | CpsType.Bind { sym = sym; body = _ } -> sym

let get_body b = match b with
  | CpsType.Bind { sym = _; body = body } -> body

let symset2list set = SymSet.fold (fun x y -> x :: y) set []

let clo_record_counter = ref 0

let fresh_clo_record_num () =
  let res = !clo_record_counter in
  incr clo_record_counter; res

let rec collect_free_vars_aux defined lambda_args is_top cps = 
  let recf c = collect_free_vars_aux defined lambda_args is_top c in
  match cps with
    | CpsType.AdmLambda (s, t) -> recf (CpsType.Lambda (s, [], t))
    | CpsType.ApplyFunc (f, k, args) -> 
        List.fold_left (fun x y -> SymSet.union x (recf y)) SymSet.empty (f :: k :: args)
    | CpsType.Passing (a, b) ->
        let s1 = recf a in
        let s2 = recf b in
        SymSet.union s1 s2
    | CpsType.Fix (binds, t) ->
        let defined = List.fold_left (fun x y -> SymSet.add (CommonSym (get_sym y)) x) defined binds in
        let f t = collect_free_vars_aux defined lambda_args is_top t in
        let setlis = List.map (fun t -> f (get_body t)) binds in
        let set = f t in
        List.fold_left SymSet.union set setlis
    | CpsType.Ref sym ->
        (match sym with
          | Primitive _ -> SymSet.empty
          | _ -> if SymSet.mem sym defined then SymSet.empty else SymSet.add sym SymSet.empty)
    | CpsType.Lambda (k, args, t) ->
        let defined = 
          if is_top then 
            List.fold_left (fun x y -> SymSet.remove y x) defined lambda_args
          else
            defined
        in
        let defined = List.fold_left (fun x y -> SymSet.add y x) defined (k :: args) in
        collect_free_vars_aux defined lambda_args false t
    | CpsType.Branch (p, t1, t2) -> 
        List.fold_left (fun x y -> SymSet.union x (recf y)) SymSet.empty [ p; t1; t2 ]
    | _ -> SymSet.empty


let collect_free_vars cps = match cps with
  | CpsType.Lambda (k, args, t) ->
      let alis = k :: args in
      let defined = List.fold_left (fun x y -> SymSet.add y x) SymSet.empty alis in 
      collect_free_vars_aux defined alis true t
  | _ -> raise (Invalid_argument "collect_free_vars")


let rec set_record cps = match cps with
  | CpsType.AdmLambda (s, t) -> set_record (CpsType.Lambda (s, [], t))
  | CpsType.ApplyFunc (f, k, args) -> Apply (set_record f, set_record k, List.map set_record args)
  | CpsType.Passing (a, b) -> Apply (set_record a, set_record b, [])
  | CpsType.Fix (binds, t) -> Fix (List.map set_record_bind binds, set_record t)
  | CpsType.Int i -> Int i
  | CpsType.Bool b -> Bool b
  | CpsType.Ref sym -> Ref sym
  | CpsType.Lambda (k, args, t) -> 
      let fv = collect_free_vars cps in
      let record = match t with
        | CpsType.Fix (binds, _) -> 
            List.fold_left (fun x y -> SymSet.add (CommonSym (get_sym y)) x) fv binds
        | _ -> fv
      in
      let record = symset2list record in
      let num = fresh_clo_record_num () in
      Lambda (Some k, RecordSym (Record num), args, set_record t, record, [])
  | CpsType.Branch (p, t1, t2) -> Branch (set_record p, set_record t1, set_record t2)
and set_record_bind b = match b with
  | CpsType.Bind { sym = sym; body = body } -> Bind { sym = sym; body = set_record body }


let get_select_idx sym lis =
  let rec get_select_idx_aux sym lis i = 
    match lis with
      | x :: xs -> if x = sym then Some i else get_select_idx_aux sym xs (i + 1)
      | [] -> None
  in
  get_select_idx_aux sym lis 0


let rec set_select clo ref_r pass_r =
  let recf c = set_select c ref_r pass_r in
  let rec_bind b = (match b with
    | Bind { sym = sym; body = body } -> Bind { sym = sym; body = (recf body) })
  in
  match clo with
    | Apply (a, b, c) -> Apply (recf a, recf b, List.map recf c)
    | Fix (binds, t) -> Fix (List.map rec_bind binds, recf t)
    | Ref sym -> (match get_select_idx sym ref_r with
      | Some i -> Select i
      | None -> Ref sym)
    | Branch (p, t1, t2) -> Branch (recf p, recf t1, recf t2)
    | Lambda (k, c, args, t, r, []) ->
        let ref_to_make_record sym = match get_select_idx sym pass_r with
          | Some i -> Some (Select i)
          | None -> None
        in
        let makes = List.map (fun sym -> (sym, ref_to_make_record sym)) r in
        Lambda (k, c, args, (set_select t pass_r r), r, makes)
    | _ -> clo


let closure_trans cps = set_select (set_record cps) [] []


let clo_make_counter = ref 0
let fresh_clo_make () = let res = !clo_make_counter in incr clo_make_counter; res
let make_clo_to_pass () = "__defc" ^ (string_of_int (fresh_clo_make ()))

let rec ast_of_clo_aux clo clo_to_ref clo_to_pass = 
  let recf c = ast_of_clo_aux c clo_to_ref clo_to_pass in
  match clo with
    | Apply (f, k, args) ->
        let f = recf f in
        let k = recf k in
        let args = List.map recf args in
        let args = k :: args in
        AstType.Apply (f, args)
    | Fix (binds, t) ->
        let binds = List.map (fun b -> ast_of_clobind b clo_to_ref clo_to_pass) binds in
        Define (binds, recf t)
    | Select i -> 
        let r = string_of_sym clo_to_ref in
        let r = AstType.Symbol r in
        AstType.Apply (AstType.Symbol "list-ref", [ r; AstType.Num i ])
    | Int i -> AstType.Num i
    | Bool b -> AstType.Bool b
    | Ref s -> AstType.Symbol (string_of_sym s)
    | Branch (p, t1, t2) -> AstType.Branch (recf p, recf t1, recf t2)
    | Lambda (k, c, args, body, _, make) ->
        let pass = make_clo_to_pass () in
        let body = ast_of_clo_aux body c pass in
        let args = List.map string_of_sym args in
        let make = ast_of_make make c pass in
        let bind_c = AstType.Bind { sym = pass; def = make } in
        let body = match body with
          | AstType.Define (binds, t) -> AstType.Define (bind_c :: binds, t)
          | _ -> AstType.Define ([ bind_c ], body)
        in
        let k = Option.map string_of_sym k in
        let c = string_of_sym c in
        let args = args in
        let args = match k with
          | Some s -> s :: args
          | None -> args
        in
        let body = AstType.Lambda (args, body) in
        let body = AstType.Lambda ([ c ], body) in
        AstType.Apply (body, [ AstType.Symbol clo_to_pass ])
and ast_of_clobind b clo_to_ref clo_to_pass = match b with
  | Bind { sym = sym; body = body } -> 
      let body = ast_of_clo_aux body clo_to_ref clo_to_pass in
      AstType.Bind { sym = sym; def = body }
and ast_of_make make clo_to_ref clo_to_pass =
  let make = List.append make [ (Primitive "`()", None) ] in
  let convert s = match s with
    | (_, Some t) -> ast_of_clo_aux t clo_to_ref clo_to_pass
    | (sym, None) -> AstType.Symbol (string_of_sym sym)
  in
  let make = List.map convert make in
  AstType.Apply (AstType.Symbol "cons*", make)

let ast_of_clo clo = 
  ast_of_clo_aux clo (CommonSym "`()") "`()"


let rec ast_of_clo_debug_aux clo = match clo with
  | Apply (a, b, c) -> AstType.Apply (ast_of_clo_debug_aux a, (ast_of_clo_debug_aux b) :: (List.map ast_of_clo_debug_aux c))
  | Fix (binds, t) -> AstType.Define (List.map clo_bind_debug binds, ast_of_clo_debug_aux t)
  | Select i -> AstType.Apply (AstType.Symbol "select", [ AstType.Num i ])
  | Int i -> AstType.Num i
  | Bool b -> AstType.Bool b
  | Ref sym -> AstType.Symbol (string_of_sym sym)
  | Branch (a, b, c) -> AstType.Branch (ast_of_clo_debug_aux a, ast_of_clo_debug_aux b, ast_of_clo_debug_aux c)
  | Lambda (k, _, args, body, _, makes) ->
      let k = Option.map string_of_sym k in
      let args = List.map string_of_sym args in
      let body = ast_of_clo_debug_aux body in
      let sym2debug m = match m with
        | (sym, Some (Select i)) -> 
            let sym = AstType.Symbol (string_of_sym sym) in
            AstType.Apply (sym, [ AstType.Symbol "is"; AstType.Num i; AstType.Symbol "th value" ])
        | (sym, None) -> 
            let sym = AstType.Symbol (string_of_sym sym) in
            AstType.Apply (sym, [ AstType.Symbol "direct" ])
        | _ -> raise (Invalid_argument "sym2debug")
      in
      let syms = List.map sym2debug makes in
      let c = "c" ^ (string_of_int (fresh_clo_make ())) in
      let bind = AstType.Bind { sym = c; def = AstType.Apply (AstType.Symbol "cons*", syms) } in
      let body = match body with
        | AstType.Define (bs, t) -> AstType.Define (bind :: bs, t)
        | _ -> AstType.Define ([ bind ], body)
      in
      let args = match k with
        | Some c -> c :: args
        | None -> args
      in
      AstType.Lambda (args, body)
and clo_bind_debug bind = match bind with
  | Bind { sym = sym; body = body } -> AstType.Bind { sym = sym; def = ast_of_clo_debug_aux body }

let ast_of_clo_debug cps = ast_of_clo_debug_aux (set_select (set_record cps) [] [])
