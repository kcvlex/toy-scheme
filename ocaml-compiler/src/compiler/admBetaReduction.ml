open CpsType
open SymbolType
open Util

module SymMap = Map.Make(Symbol)


(********** Indexing **********)

let replace_if_adm sym d map = match sym with
  | ContSym _ | ParamSym _ ->
      let v = SymMap.find sym map in
      Some (d - v)
  | _ -> None

let add_if_adm sym d map = match sym with
  | ContSym _ | ParamSym _ -> (SymMap.add sym (d + 1) map, d + 1)
  | _ -> (map, d + 1)

let rec indexing_aux cps d map =
  let recf c = indexing_aux c d map in
  match cps with
    | AdmLambda (s, t) ->
        let map, d = add_if_adm s d map in
        AdmLambda (s, indexing_aux t d map)
    | Lambda (k, args, larg, body) ->
        (* FIXME *)
        let larg_f = if Option.is_some larg then 1 else 0 in
        let map, _ = add_if_adm k d map in
        let d = d + (List.length args) + larg_f in
        Lambda (k, args, larg, indexing_aux body d map)
    | ApplyFunc (f, k, args) -> ApplyFunc (recf f, recf k, List.map recf args)
    | Passing (f, k) ->  Passing (recf f, recf k)
    | Let ((s, x), body) -> Let ((s, recf x), recf body)
    | RefDirect s -> (match replace_if_adm s d map with
      | Some d -> RefIndex d
      | None -> RefDirect s)
    | MakeBox t -> MakeBox (recf t)
    | Branch (a, b, c) -> Branch (recf a, recf b, recf c)
    | _ -> cps

let de_bruijn_indexing cps = indexing_aux cps (-1) SymMap.empty


(********** Beta reduction **********)

let replace_term target expr =
  let rec replace_aux target depth =
    let recf x = replace_aux x depth in
    (match target with
      | AdmLambda (sym, body) -> AdmLambda (sym, replace_aux body (depth + 1))
      | Lambda (k, args, larg, body) ->
          let add = (List.length args) + 1 + (if Option.is_some larg then 1 else 0) in
          Lambda (k, args, larg, replace_aux body (depth + add))
      | ApplyFunc (f, k, args) -> ApplyFunc (recf f, recf k, List.map recf args)
      | Passing (a, b) -> Passing (recf a, recf b)
      | Let ((x, t), body) -> Let ((x, recf t), recf body)
      | MakeBox t -> MakeBox (recf t)
      | Branch (a, b, c) -> Branch (recf a, recf b, recf c)
      | RefIndex i when depth = i -> fix_index expr depth (-1)  (* replace *)
      | RefIndex i when depth < i -> RefIndex (i - 1)           (* -1 means the reduction *)
      | _ -> target)
    and fix_index expr depth_when_replaced depth =
      let recf x = fix_index x depth_when_replaced depth in
      (match expr with
        | AdmLambda (sym, body) -> AdmLambda (sym, fix_index body depth_when_replaced (depth + 1))
        | Lambda (k, args, larg, body) ->
          let add = (List.length args) + 1 + (if Option.is_some larg then 1 else 0) in
          Lambda (k, args, larg, fix_index body depth_when_replaced (depth + add))
        | ApplyFunc (f, k, args) -> ApplyFunc (recf f, recf k, List.map recf args)
        | Passing (a, b) -> Passing (recf a, recf b)
        | Let ((x, t), body) -> Let ((x, recf t), recf body)
        | MakeBox t -> MakeBox (recf t)
        | Branch (a, b, c) -> Branch (recf a, recf b, recf c)
        | RefIndex i when depth < i -> RefIndex (depth_when_replaced + i)
        | _ -> expr)
  in
  replace_aux target 0

let beta_step cps = 
  let update = ref false in
  let rec beta_step_aux cps depth =
    let beta0 c = beta_step_aux c depth in
    let beta1 c = beta_step_aux c (depth + 1) in
    match cps with
      | AdmLambda (k, t) -> AdmLambda (k, beta1 t)
      | Lambda (k, args, larg, body) ->
          let add = (List.length args) + 1 + (if Option.is_some larg then 1 else 0) in
          Lambda (k, args, larg, beta_step_aux body (depth + add))
      | ApplyFunc (f, k, args) -> ApplyFunc (beta0 f, beta0 k, List.map beta0 args)
      | Passing (AdmLambda (_, body), x) -> update := true; replace_term body x
      | Passing (a, b) -> Passing (beta0 a, beta0 b)
      | Let ((x, t), body) -> Let ((x, beta0 t), beta0 body)
      | MakeBox t -> MakeBox (beta0 t)
      | Branch (a, b, c) -> Branch (beta0 a, beta0 b, beta0 c)
      | _ -> cps
  in
  let cps = beta_step_aux cps 0 in
  (cps, !update)

let rec beta_reduction cps =
  let cps, updated = beta_step cps in
  if updated then beta_reduction cps else cps


(********** Restore Indexing **********)

let restore_indexing cps =
  let args = Vector.empty () in
  let add_arg arg = Vector.push_back args arg in
  let rm_args n = Vector.pops args n in
  let rec restore_aux cps = match cps with
    | AdmLambda (k, t) ->
        add_arg k;
        let res = AdmLambda (k, restore_aux t) in
        rm_args 1; res
    | Lambda (k, args, larg, body) ->
        add_arg k;
        List.iter add_arg args;
        Option.iter add_arg larg;
        let body = restore_aux body in
        let len = (List.length args) + 1 + (if Option.is_some larg then 1 else 0) in
        rm_args len; Lambda (k, args, larg, body)
    | ApplyFunc (a, b, c) ->
        let a = restore_aux a in
        let b = restore_aux b in
        let c = List.map restore_aux c in
        ApplyFunc (a, b, c)
    | Passing (a, b) ->
        let a = restore_aux a in
        let b = restore_aux b in
        Passing (a, b)
    | Let ((x, t), body) ->
        let t = restore_aux t in
        let body = restore_aux body in
        Let ((x, t), body)
    | MakeBox t -> MakeBox (restore_aux t)
    | Branch (a, b, c) ->
        let a = restore_aux a in
        let b = restore_aux b in
        let c = restore_aux c in
        Branch (a, b, c)
    | RefIndex i -> RefDirect (Vector.rget args i)
    | _ -> cps
  in
  restore_aux cps


let normalize cps =
  cps |> de_bruijn_indexing
      |> beta_reduction 
      |> restore_indexing
