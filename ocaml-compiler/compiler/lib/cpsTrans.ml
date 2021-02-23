open Cps
open Ast

exception CPSError of string

let cont_id_counter = ref 0
let cont_param_counter = ref 0

let fresh_cont_id () =
  let num = !cont_id_counter in
  begin
    incr cont_id_counter; 
    AdmCont num
  end

let fresh_cont_param () =
  let num = !cont_param_counter in
  begin
    incr cont_param_counter;
    AdmParam num
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

type ('a, 'b) either = Left of 'a | Right of 'b

let make_cps_value value =
  Value (fresh_cont_id (), value)

(* `next` is used in Define *)
let rec cps_trans_aux ast next = match ast with
  | Num n -> make_cps_value (Int n)
  | Bool b -> make_cps_value (Bool b)
  | Symbol s ->
      let s = match (to_primitive ast) with
        | Some p -> p
        | None -> UserSym s
      in
      make_cps_value (Sym s)
  | Lambda (args, body) ->
      let k = fresh_cont_id () in
      let args = List.map (fun x -> (sym2sym x)) args in
      let body = cps_trans_aux body None in
      make_cps_value (Lambda (k, args, PassCont (body, ContSym k)))
  | Apply (f, args) ->
      let f = cps_trans_aux f None in
      let pf = match f with
        | Value (_, Sym (Primitive p)) -> Left (Primitive p)
        | _ -> Right (fresh_cont_param ())
      in
      let args = List.map (fun x -> cps_trans_aux x None) args in
      let pargs = fresh_cont_param_list (List.length args) in
      let pargs_sym = List.map (fun x -> (AdmSym x)) pargs in
      let k = fresh_cont_id () in
      let body = match pf with
        | Left p -> ApplyFunc (p, ContSym k, pargs_sym)
        | Right f -> ApplyFunc (AdmSym f, ContSym k, pargs_sym)
      in
      let merge = fun cps sym body -> PassCont (cps, AdmLambda (sym, body)) in
      let body = List.fold_right2 merge args pargs body in
      let body = match pf with
        | Left _ -> body
        | Right t -> merge f t body
      in
      Cont (AdmLambda (k, body))
  | Define (sym, def) ->
      let sym = sym2sym sym in
      let def = cps_trans_aux def None in
      let k = fresh_cont_id () in
      let t = fresh_cont_param () in
      let body = Option.map (fun x -> (PassCont (x, ContSym k))) next in
      let body = Option.value body ~default:(Cont (ContSym k)) in
      let body = Bind (sym, AdmSym t, body) in
      let body = AdmLambda (t, body) in
      let body = PassCont (def, body) in
      Cont (AdmLambda (k, body))
  | Cons (car, cdr) -> 
      let cdr = Option.map (fun x -> cps_trans_aux x None) cdr in
      let car = cps_trans_aux car cdr in
      car

let cps_transformation ast = cps_trans_aux ast None

let rec cps2ast cps = match cps with
  | Value (k, v) ->
      let k = cps_adm_sym2ast k in
      let v = cps_value2ast v in
      Lambda ([ k ], Apply (k, [ v ]))
  | Cont c -> cps_cont2ast c
  | ApplyCont (cont, cps) ->
      let f = cps_cont2ast cont in
      let arg = cps2ast cps in
      Apply (f, [ arg ])
  | PassCont (cps, cont) ->
      let f = cps2ast cps in
      let arg = cps_cont2ast cont in
      Apply (f, [ arg ])
  | ApplyFunc (f, cont, args) ->
      let f = cps_sym2ast f in
      let cont = cps_cont2ast cont in
      let args = List.map (fun x -> (cps_sym2ast x)) args in
      Apply (f, cont :: args)
  | Bind (x, t, body) ->
      let x = cps_sym2ast x in
      let t = cps_sym2ast t in
      let body = cps2ast body in
      Cons (Define (x, t), Some body)
and cps_sym2ast sym = match sym with
  | UserSym s -> Symbol s
  | Primitive "+" -> Symbol "add"
  | Primitive "-" -> Symbol "sub"
  | Primitive s -> Symbol s
  | AdmSym s -> cps_adm_sym2ast s
and cps_adm_sym2ast sym = match sym with
  | AdmCont n -> Symbol ("k" ^ (string_of_int n))
  | AdmParam n -> Symbol ("t" ^ (string_of_int n))
and cps_cont2ast cont = match cont with
  | ContSym sym -> cps_adm_sym2ast sym
  | AdmLambda (sym, body) -> 
      let sym = cps_adm_sym2ast sym in
      let body = cps2ast body in
      Lambda ([ sym ], body)
and cps_value2ast value = match value with
  | Int i -> Num i
  | Bool b -> Bool b
  | Sym s -> cps_sym2ast s
  | Lambda (adm, args, body) ->
      let adm = cps_adm_sym2ast adm in
      let args = List.map (fun x -> (cps_sym2ast x)) args in
      let body = cps2ast body in
      Lambda (adm :: args, body)
