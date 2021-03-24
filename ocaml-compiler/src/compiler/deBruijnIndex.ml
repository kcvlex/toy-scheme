open DeBruijnIndexType
open Symbol
open SymbolType
open Util

(******************** Indexing ********************)

let sym2key sym = match sym with
  | ContSym (Cont n) -> Some (n * 2)
  | ParamSym n -> Some (n * 2 + 1)
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

let rec dfs cps d var2index =
  let dfs_aux cps = dfs cps d var2index in
  match cps with
  | CpsType.AdmLambda (sym, t) ->
      let added = add_if_adm sym d var2index in
      AdmLambda (sym, dfs t (snd added) (fst added))
  | CpsType.ApplyFunc (f, k, args) ->
      let f = dfs_aux f in
      let k = dfs_aux k in
      let args = List.map dfs_aux args in
      ApplyFunc (f, k, args)
  | CpsType.Passing (x, y) -> Passing (dfs_aux x, dfs_aux y)
  | CpsType.Fix (flis, t) ->
      let flis = List.map (fun x -> dbifix_of_cpsfix x d var2index) flis in
      let t = dfs_aux t in
      Fix (flis, t)
  | CpsType.Int i -> Int i
  | CpsType.Bool b -> Bool b
  | CpsType.Ref sym -> replace_if_adm sym d var2index
  | CpsType.Lambda (k, args, body) ->
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
  | CpsType.Branch (p, th, els) -> Branch (dfs_aux p, dfs_aux th, dfs_aux els)
and dbifix_of_cpsfix fix d var2index = match fix with
  | CpsType.Bind { sym = sym; body = body; } ->
      let body = dfs body d var2index in
      Bind { sym = sym; body = body; }

let dbi_of_cps cps = dfs cps (-1) AdmVarMap.empty


(******************** Restore ********************)
let cps_of_dbi dbi_cps =
  let args = Vector.empty () in
  let add_arg arg = Vector.push_back args arg in
  let rm_args n = Vector.pops args n in
  let rec cps_of_dbi_aux dcps = (match dcps with
    | AdmLambda (sym, t) -> 
        add_arg sym;
        let res = CpsType.AdmLambda (sym, cps_of_dbi_aux t) in
        begin rm_args 1; res end
    | ApplyFunc (f, k, args) -> CpsType.ApplyFunc (cps_of_dbi_aux f, cps_of_dbi_aux k, List.map cps_of_dbi_aux args)
    | Passing (a, b) -> CpsType.Passing (cps_of_dbi_aux a, cps_of_dbi_aux b)
    | Fix (flis, body) -> CpsType.Fix (List.map dbifix_of_cpsfix flis, cps_of_dbi_aux body)
    | Int i -> CpsType.Int i
    | Bool b -> CpsType.Bool b
    | Ref s -> CpsType.Ref s
    | RefIndex i -> CpsType.Ref (Vector.rget args i)
    | Branch (p, th, els) -> CpsType.Branch (cps_of_dbi_aux p, cps_of_dbi_aux th, cps_of_dbi_aux els)
    | Lambda (k, args, body, len) ->
        add_arg k; List.iter add_arg args;
        let res = CpsType.Lambda (k, args, cps_of_dbi_aux body) in
        begin
          rm_args len;
          res
        end)
  and dbifix_of_cpsfix dbi_fix = (match dbi_fix with
    | Bind { sym = sym; body = body; } ->
        let body = cps_of_dbi_aux body in
        CpsType.Bind { sym = sym; body = body; })
  in
  cps_of_dbi_aux dbi_cps


(******************** Convert to string for debug  ********************)
let rec string_of_dbi db_cps = match db_cps with
  | AdmLambda (arg, t) -> lambda_abst arg (string_of_dbi t)
  | ApplyFunc (f, k, args) -> 
      List.fold_left (fun x y -> surround_paren (x ^ " " ^ (string_of_dbi y))) (string_of_dbi f) (k :: args)
  | Passing (x, y) -> surround_paren ((string_of_dbi x) ^ " " ^ (string_of_dbi y))
  | Fix (_, _) -> "UNIMPLED";
  | Int i -> string_of_int i
  | Bool true -> "#t"
  | Bool false -> "#f"
  | Ref sym -> Symbol.string_of_sym sym
  | RefIndex i -> "_" ^ (string_of_int i)
  | Branch (_, _, _) -> "UNIMPLED"
  | Lambda (k, args, body, _) ->
      let body = string_of_dbi body in
      List.fold_right (fun x y -> lambda_abst x y) (k :: args) body
and lambda_abst arg s = "(lmd " ^ (string_of_sym arg) ^ ". " ^ s ^ ")"
and surround_paren x = "(" ^ x ^ ")"
