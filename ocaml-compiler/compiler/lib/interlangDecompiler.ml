open InterlangType
open AstType
open Symbol

let select_fun = Symbol "list-ref"
let call_fun = Symbol "__call"
let cons_fun = Symbol "cons"
let cons_asta_fun = Symbol "cons*"
let nil_sym = Symbol "`()"
let make_select lis i = Apply (select_fun, [ lis; Num i ])
let make_call f args = Apply (call_fun, f :: args)
let make_cons car cdr = Apply (cons_fun, [ car; cdr ])
let make_list args = Apply (cons_asta_fun, List.append args [ nil_sym ])

let (>::) opt lis = match opt with
  | Some t -> t :: lis
  | None -> lis

let rec decompile inter = match inter with
  | Select (rsym, i) -> make_select (Symbol (string_of_rsym rsym)) i
  | Ref sym -> Symbol (string_of_sym sym)
  | Branch (p, t1, t2) ->
      let p = decompile p in
      let t1 = decompile t1 in
      let t2 = decompile t2 in
      Branch (p, t1, t2)
  | Apply (f, k, args) ->
      let f = decompile f in
      let k = decompile k in
      let args = List.map decompile args in
      make_call f (k :: args)
  | Int i -> Num i
  | Bool b -> Bool b
  | Let (blis, t) ->
      let fn pair =
        let sym, def = pair in
        let def = decompile def in
        Bind { sym = sym; def = def }
      in
      let blis = List.map fn blis in
      let t = decompile t in
      Define (blis, t)
  | Closure (psym, rsym) -> 
      make_cons (Symbol (string_of_psym psym)) (Symbol (string_of_rsym rsym))
  | Program (plis, t) ->
      let plis = List.map decompile_proc plis in
      let t = decompile t in
      Define (plis, t)
and decompile_proc p = match p with
  | Procedure (psym, args, make, body) ->
      let sym = string_of_psym psym in
      let args = decompile_args args in
      let rsym, rlis = make in
      let rlis = List.map decompile rlis in
      let rlis = make_list rlis in
      let make = Bind { sym = string_of_rsym rsym; def = rlis } in
      let body = decompile body in
      let body = Define ([ make ], body) in
      let lambda = Lambda (args, body) in
      Bind { sym = sym; def = lambda }
and decompile_args args = match args with
  | Args (c, r, lis) ->
      let c = Option.map string_of_csym c in
      let r = Option.map string_of_rsym r in
      c >:: (r >:: lis)
