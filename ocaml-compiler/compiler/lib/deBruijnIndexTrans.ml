open DeBruijnIndex

(******************** Indexing ********************)

let sym2key sym = match sym with
  | Cps.ContSym n -> Some (n * 2)
  | Cps.ParamSym n -> Some (n * 2 + 1)
  | _ -> None

module AdmVarMap = Map.Make(Int)

let replace_if_adm sym d var2index = 
  let key = sym2key sym in
  match key with
    | Some k -> 
        let v = AdmVarMap.find k var2index in
        let diff = d - v in
        RefIndex diff
    | None -> Ref sym

let add_if_adm sym d var2index =
  let key = sym2key sym in
  match key with
    | Some k -> (AdmVarMap.add k (d + 1) var2index, d + 1)
    | None -> (var2index, d + 1)

let rec dfs cps d var2index = match cps with
  | Cps.AdmLambda (sym, t) ->
      let added = add_if_adm sym d var2index in
      AdmLambda (sym, dfs t (snd added) (fst added))
  | Cps.ApplyFunc (f, k, args) ->
      let f = dfs f d var2index in
      let k = dfs k d var2index in
      let args = List.map (fun x -> dfs x d var2index) args in
      ApplyFunc (f, k, args)
  | Cps.Passing (x, y) -> Passing (dfs x d var2index, dfs y d var2index)
  | Cps.Bind (x, t, body) -> 
      let t = dfs t d var2index in
      let body = dfs body d var2index in
      Bind (x, t, body)
  | Cps.Int i -> Int i
  | Cps.Bool b -> Bool b
  | Cps.Ref sym -> replace_if_adm sym d var2index
  | Cps.Lambda (k, args, body) ->
      let update sym md = 
        let map = (fst md) in
        let d = (snd md) in
        add_if_adm sym d map
      in
      let cur = (var2index, d) in
      let cur = update k cur in
      let cur = List.fold_left (fun x y -> update y x) cur args in
      let body = dfs body (snd cur) (fst cur) in
      Lambda (k, args, body, (List.length args) + 1)

let de_bruijn_indexing cps = dfs cps (-1) AdmVarMap.empty


(******************** Restore ********************)
let restore dbi_cps =
  let args = Vector.empty () in
  let add_arg arg = Vector.push_back args arg in
  let rm_args n = Vector.pops args n in
  let rec restore_aux dcps =
    match dcps with
      | AdmLambda (sym, t) -> 
          add_arg sym;
          let res = Cps.AdmLambda (sym, restore_aux t) in
          begin rm_args 1; res end
      | ApplyFunc (f, k, args) -> Cps.ApplyFunc (restore_aux f, restore_aux k, List.map restore_aux args)
      | Passing (a, b) -> Cps.Passing (restore_aux a, restore_aux b)
      | Bind (x, t, body) -> Cps.Bind (x, restore_aux t, restore_aux body)
      | Int i -> Cps.Int i
      | Bool b -> Cps.Bool b
      | Ref s -> Cps.Ref s
      | RefIndex i -> Cps.Ref (Vector.rget args i)
      | Lambda (k, args, body, len) ->
          add_arg k; List.iter add_arg args;
          let res = Cps.Lambda (k, args, restore_aux body) in
          begin
            rm_args len;
            res
          end
  in
  restore_aux dbi_cps


(******************** Convert to string for debug  ********************)
let rec to_str db_cps = match db_cps with
  | AdmLambda (arg, t) -> lambda_abst arg (to_str t)
  | ApplyFunc (f, k, args) -> 
      List.fold_left (fun x y -> surround_paren (x ^ " " ^ (to_str y))) (to_str f) (k :: args)
  | Passing (x, y) -> surround_paren ((to_str x) ^ " " ^ (to_str y))
  | Bind (x, t, body) -> surround_paren ("(let " ^ (sym2str x) ^ " " ^ (to_str t) ^ ") " ^ (to_str body))
  | Int i -> string_of_int i
  | Bool true -> "#t"
  | Bool false -> "#f"
  | Ref sym -> sym2str sym
  | RefIndex i -> "_" ^ (string_of_int i)
  | Lambda (k, args, body, _) ->
      let body = to_str body in
      List.fold_right (fun x y -> lambda_abst x y) (k :: args) body
and sym2str sym = match sym with
  | UserSym s -> s
  | Primitive s -> s
  | ContSym i -> "k" ^ (string_of_int i)
  | ParamSym i -> "t" ^ (string_of_int i)
and lambda_abst arg s = "(lmd " ^ (sym2str arg) ^ ". " ^ s ^ ")"
and surround_paren x = "(" ^ x ^ ")"
