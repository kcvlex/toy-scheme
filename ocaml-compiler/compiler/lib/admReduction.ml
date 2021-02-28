(*

open Cps
let hashtbl_sz = 100

let sym2cps = Hashtbl.create hashtbl_sz

let equal_adm l r = match (l, r) with
  | (AdmCont a, AdmCont b) -> a = b
  | (AdmParam a, AdmParam b) -> a = b
  | _ -> false

let rec replace cps adm assign = match cps with
  | Value (k, v) ->
      let k = replace_cps_adm k adm assign in
      let v = replace_cps_value v adm assign in
      Value (k, v)
  | Cont c -> replace_cps_cont c adm assign
  | PassCont (t, c) -> 
      let t = replace_cps t adm assign in
      let c = replace_cps_cont c adm assign in
      PassCont (t, c)
  | ApplyFunc (f, c, args) ->
      let f = replace_cps_sym f adm assign in
      let c = replace_cps_sym c adm assign in
      let args = List.map (fun x -> replace_cps_sym x adm assign) in
      ApplyFunc (f, c, args)
  | Bind (x, t, body) -> (* x is fresh variable *)
      let t = replace_cps_sym t adm assign in
      let body = replace_cps body adm assign in
      Bind (x, t, body)
and replace_cps_sym sym adm assign = match sym with
  | AdmSym a -> replace_adm_sym a adm assign
*)
