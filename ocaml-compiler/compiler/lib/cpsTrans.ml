open Cps
open Ast

exception CPSError of string

let cont_id_counter = ref 0
let cont_param_counter = ref 0

let fresh_cont_id () =
  let num = !cont_id_counter in
  begin
    incr cont_id_counter; 
    ContSym num
  end

let fresh_cont_param () =
  let num = !cont_param_counter in
  begin
    incr cont_param_counter;
    ParamSym num
  end

let rec fresh_cont_param_list num =
  if num = 0 then [] else ((fresh_cont_param ()) :: (fresh_cont_param_list (num - 1)))

let sym2sym ast = match ast with
  | Symbol s -> UserSym s
  | _ -> raise (CPSError "sym2sym")

let to_primitive ast =
  let is_primitive s = begin
    match s with
      | "+" -> true
      | "-" -> true
      | _ -> false
  end in
  match ast with
    | Symbol s when (is_primitive s) -> Some (Primitive s)
    | _ -> None

let make_cps_value value =
  let k = fresh_cont_id () in
  AdmLambda (k, Passing (Ref k, value))

(* `next` is used in Define *)
let rec cps_trans_aux ast next = match ast with
  | Num n -> make_cps_value (Int n)
  | Bool b -> make_cps_value (Bool b)
  | Symbol s -> (match to_primitive ast with
    | Some p -> Ref p
    | None -> make_cps_value (Ref (UserSym s)))
  | Lambda (args, body) ->
      let k = fresh_cont_id () in
      let args = List.map (fun x -> (sym2sym x)) args in
      let body = cps_trans_aux body None in
      make_cps_value (Lambda (k, args, Passing (body, Ref k)))
  | Apply (f, args) ->
      let merge_args body params cps_lis =
        let merge = fun cps param body -> Passing (cps, AdmLambda (param, body)) in
        let body = List.fold_right2 merge cps_lis params body in
        body
      in
      let f = cps_trans_aux f None in
      let k = fresh_cont_id () in
      let args = List.map (fun x -> cps_trans_aux x None) args in
      let params = fresh_cont_param_list (List.length args) in
      let merged = (match f with
        | Ref (Primitive s) ->
            let body = ApplyFunc (Ref (Primitive s), Ref k, List.map (fun x -> Ref x) params) in
            merge_args body params args
        | _ -> 
            let t = fresh_cont_param () in
            let body = ApplyFunc (Ref t, Ref k, List.map (fun x -> Ref x) params) in
            merge_args body (t :: params) (f :: args))
      in
      AdmLambda (k, merged)
  | Define (sym, def) ->
      let sym = sym2sym sym in
      let def = cps_trans_aux def None in
      let k = fresh_cont_id () in
      let t = fresh_cont_param () in
      let body = Option.map (fun x -> (Passing (x, Ref k))) next in
      let body = Option.value body ~default:(Ref k) in
      let body = Bind (sym, Ref t, body) in
      let body = AdmLambda (t, body) in
      let body = Passing (def, body) in
      AdmLambda (k, body)
  | Cons (car, cdr) ->
      let cdr = Option.map (fun x -> cps_trans_aux x None) cdr in
      let car = cps_trans_aux car cdr in
      car

let cps_transformation ast = cps_trans_aux ast None

let rec cps2ast cps = match cps with
  | AdmLambda (sym, body) -> Lambda ([ (cps_sym2ast sym) ], cps2ast body)
  | ApplyFunc (f, k, args) -> 
      let f = cps2ast f in
      let k = cps2ast k in
      let args = List.map cps2ast args in
      Apply(f, k :: args)
  | Passing (f, c) -> Apply (cps2ast f, [ (cps2ast c) ])
  | Bind (x, t, body) -> 
      let x = cps_sym2ast x in
      let t = cps2ast t in
      let body = cps2ast body in
      Cons (Define (x, t), Some body)
  | Int i -> Num i
  | Bool b -> Bool b
  | Ref s -> cps_sym2ast s
  | Lambda (adm, args, body) ->
      let adm = cps_sym2ast adm in
      let args = List.map cps_sym2ast args in
      let body = cps2ast body in
      Lambda (adm :: args, body)
and cps_sym2ast sym = match sym with
  | UserSym s -> Symbol s
  | Primitive "+" -> Symbol "add"
  | Primitive "-" -> Symbol "sub"
  | Primitive s -> Symbol s
  | ContSym i -> Symbol ("k" ^ (string_of_int i))
  | ParamSym i -> Symbol ("t" ^ (string_of_int i))
