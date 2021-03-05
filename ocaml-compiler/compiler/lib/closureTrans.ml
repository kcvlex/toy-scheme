open Closure

module OrderedSym = struct
  type t = clo_sym

  let to_pair sym = match sym with
    | UserSym s -> (0, s)
    | Primitive s -> (1, s)
    | ContSym n -> (2, string_of_int n)
    | ParamSym n -> (3, string_of_int n)
    | ClosureSym n -> (4, string_of_int n)

  let compare t1 t2 =
    let t1 = to_pair t1 in
    let t2 = to_pair t2 in
    match (t1, t2) with
      | ((a, b), (c, d)) -> if a != c then Int.compare a c else String.compare b d
end

module SymSet = Set.Make(OrderedSym)

let sym2sym cpss = match cpss with
  | Cps.UserSym s -> UserSym s
  | Cps.Primitive s -> Primitive s
  | Cps.ContSym i -> ContSym i
  | Cps.ParamSym i -> ParamSym i

let get_sym b = match b with
  | Cps.Bind { sym = sym; body = _ } -> sym

let get_body b = match b with
  | Cps.Bind { sym = _; body = body } -> body

let symset2list set = SymSet.fold (fun x y -> x :: y) set []

(*
let eq_sym sym1 sym2 = match (sym1, sym2) in
  | (UserSym s1, UserSym s2) -> s1 == s2
  | (Primitive s1, Primitive s2) -> s1 == s2
  | (ContSym i1, ContSym i2) -> i1 == i2
  | (ParamSym i1, ParamSym i2) -> i1 == i2
  | _ -> false
*)

let clo_record_counter = ref 0

let fresh_clo_record_num () =
  let res = !clo_record_counter in
  incr clo_record_counter; res

let rec collect_free_vars_aux defined lambda_args is_top cps = 
  let recf c = collect_free_vars_aux defined lambda_args is_top c in
  match cps with
    | Cps.AdmLambda (s, t) -> recf (Cps.Lambda (s, [], t))
    | Cps.ApplyFunc (f, k, args) -> 
        List.fold_left (fun x y -> SymSet.union x (recf y)) SymSet.empty (f :: k :: args)
    | Cps.Passing (a, b) ->
        let s1 = recf a in
        let s2 = recf b in
        SymSet.union s1 s2
    | Cps.Fix (binds, t) ->
        let defined = List.fold_left (fun x y -> SymSet.add (UserSym (get_sym y)) x) defined binds in
        let f t = collect_free_vars_aux defined lambda_args is_top t in
        let setlis = List.map (fun t -> f (get_body t)) binds in
        let set = f t in
        List.fold_left SymSet.union set setlis
    | Cps.Ref sym ->
        let sym = sym2sym sym in
        (match sym with
          | Primitive _ -> SymSet.empty
          | _ -> if SymSet.mem sym defined then SymSet.empty else SymSet.add sym SymSet.empty)
    | Cps.Lambda (k, args, t) ->
        let defined = 
          if is_top then 
            List.fold_left (fun x y -> SymSet.remove y x) defined lambda_args
          else
            defined
        in
        let k = sym2sym k in
        let args = List.map sym2sym args in
        let defined = List.fold_left (fun x y -> SymSet.add y x) defined (k :: args) in
        collect_free_vars_aux defined lambda_args false t
    | Cps.Branch (p, t1, t2) -> 
        List.fold_left (fun x y -> SymSet.union x (recf y)) SymSet.empty [ p; t1; t2 ]
    | _ -> SymSet.empty


let collect_free_vars cps = match cps with
  | Cps.Lambda (k, args, t) ->
      let alis = List.map sym2sym (k :: args) in
      let defined = List.fold_left (fun x y -> SymSet.add y x) SymSet.empty alis in 
      collect_free_vars_aux defined alis true t
  | _ -> raise (Invalid_argument "collect_free_vars")


let rec set_record cps = match cps with
  | Cps.AdmLambda (s, t) -> set_record (Cps.Lambda (s, [], t))
  | Cps.ApplyFunc (f, k, args) -> Apply (set_record f, set_record k, List.map set_record args)
  | Cps.Passing (a, b) -> Apply (set_record a, set_record b, [])
  | Cps.Fix (binds, t) -> Fix (List.map set_record_bind binds, set_record t)
  | Cps.Int i -> Int i
  | Cps.Bool b -> Bool b
  | Cps.Ref sym -> Ref (sym2sym sym)
  | Cps.Lambda (k, args, t) -> 
      let fv = collect_free_vars cps in
      let record = match t with
        | Cps.Fix (binds, _) -> 
            List.fold_left (fun x y -> SymSet.add (UserSym (get_sym y)) x) fv binds
        | _ -> fv
      in
      let record = symset2list record in
      let k = sym2sym k in
      let args = List.map sym2sym args in
      let num = fresh_clo_record_num () in
      Lambda (Some k, ClosureSym num, args, set_record t, record, [])
  | Cps.Branch (p, t1, t2) -> Branch (set_record p, set_record t1, set_record t2)
and set_record_bind b = match b with
  | Cps.Bind { sym = sym; body = body } -> Bind { sym = sym; body = set_record body }


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

let rec clo2ast_aux clo clo_to_ref clo_to_pass = 
  let recf c = clo2ast_aux c clo_to_ref clo_to_pass in
  match clo with
    | Apply (f, k, args) ->
        let f = recf f in
        let k = recf k in
        let args = List.map recf args in
        let args = k :: args in
        Ast.Apply (f, args)
    | Fix (binds, t) ->
        let binds = List.map (fun b -> clo_bind2ast b clo_to_ref clo_to_pass) binds in
        Define (binds, recf t)
    | Select i -> Ast.Apply (Ast.Symbol "list-ref", [ clo_sym2ast clo_to_ref; Ast.Num i ])
    | Int i -> Ast.Num i
    | Bool b -> Ast.Bool b
    | Ref s -> clo_sym2ast s
    | Branch (p, t1, t2) -> Ast.Branch (recf p, recf t1, recf t2)
    | Lambda (k, c, args, body, _, make) ->
        let pass = make_clo_to_pass () in
        let body = clo2ast_aux body c pass in
        let args = List.map clo_sym2ast args in
        let make = make2ast make c pass in
        let bind_c = Ast.Bind { sym = pass; def = make } in
        let body = match body with
          | Ast.Define (binds, t) -> Ast.Define (bind_c :: binds, t)
          | _ -> Ast.Define ([ bind_c ], body)
        in
        let k = Option.map clo_sym2ast k in
        let c = clo_sym2ast c in
        let args = args in
        let args = match k with
          | Some s -> s :: args
          | None -> args
        in
        let body = Ast.Lambda (args, body) in
        let body = Ast.Lambda ([ c ], body) in
        Ast.Apply (body, [ Ast.Symbol clo_to_pass ])
and clo_sym2ast sym = match sym with
  | UserSym s -> Ast.Symbol s
  | Primitive "+" -> Ast.Symbol "add"
  | Primitive "<" -> Ast.Symbol "less"
  | Primitive s -> Ast.Symbol s
  | ContSym i -> Ast.Symbol ("__k" ^ (string_of_int i))
  | ParamSym i -> Ast.Symbol ("__t" ^ (string_of_int i))
  | ClosureSym i -> Ast.Symbol ("__c" ^ (string_of_int i))
and clo_bind2ast b clo_to_ref clo_to_pass = match b with
  | Bind { sym = sym; body = body } -> 
      let body = clo2ast_aux body clo_to_ref clo_to_pass in
      Ast.Bind { sym = sym; def = body }
and make2ast make clo_to_ref clo_to_pass =
  let make = List.append make [ (Primitive "`()", None) ] in
  let convert s = match s with
    | (_, Some t) -> clo2ast_aux t clo_to_ref clo_to_pass
    | (sym, None) -> clo_sym2ast sym
  in
  let make = List.map convert make in
  Ast.Apply (Ast.Symbol "cons*", make)

let clo2ast clo = 
  clo2ast_aux clo (UserSym "`()") "`()"


let rec clo_debug_aux clo = match clo with
  | Apply (a, b, c) -> Ast.Apply (clo_debug_aux a, (clo_debug_aux b) :: (List.map clo_debug_aux c))
  | Fix (binds, t) -> Ast.Define (List.map clo_bind_debug binds, clo_debug_aux t)
  | Select i -> Ast.Apply (Ast.Symbol "select", [ Ast.Num i ])
  | Int i -> Ast.Num i
  | Bool b -> Ast.Bool b
  | Ref sym -> clo_sym_debug sym
  | Branch (a, b, c) -> Ast.Branch (clo_debug_aux a, clo_debug_aux b, clo_debug_aux c)
  | Lambda (k, _, args, body, _, makes) ->
      let k = Option.map clo_sym_debug k in
      let args = List.map clo_sym_debug args in
      let body = clo_debug_aux body in
      let sym2debug m = match m with
        | (sym, Some (Select i)) -> 
            Ast.Apply (clo_sym_debug sym, [ Ast.Symbol "is"; Ast.Num i; Ast.Symbol "th value" ])
        | (sym, None) -> Ast.Apply (clo_sym_debug sym, [ Ast.Symbol "direct" ])
        | _ -> raise (Invalid_argument "sym2debug")
      in
      let syms = List.map sym2debug makes in
      let c = "c" ^ (string_of_int (fresh_clo_make ())) in
      let bind = Ast.Bind { sym = c; def = Ast.Apply (Ast.Symbol "cons*", syms) } in
      let body = match body with
        | Ast.Define (bs, t) -> Ast.Define (bind :: bs, t)
        | _ -> Ast.Define ([ bind ], body)
      in
      let args = match k with
        | Some c -> c :: args
        | None -> args
      in
      Ast.Lambda (args, body)
and clo_sym_debug sym = clo_sym2ast sym
and clo_bind_debug bind = match bind with
  | Bind { sym = sym; body = body } -> Ast.Bind { sym = sym; def = clo_debug_aux body }

let clo_debug cps = clo_debug_aux (set_select (set_record cps) [] [])
