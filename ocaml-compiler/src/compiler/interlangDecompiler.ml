open InterlangType
open AstType
open SymbolType
open Symbol

let select_fun = Symbol "vector-ref"
let call_fun = Symbol "__call"
let set_vec_fun = Symbol "vector-set!"
let cons_fun = Symbol "cons"
let make_vector_fun = Symbol "make-vector"
let make_select vec i = Apply (select_fun, [ vec; Num i ])
let make_call f args = Apply (call_fun, f :: args)
let make_vector k = Apply (make_vector_fun, [ Num k ])
let make_vector_set vec i ele = Apply (set_vec_fun, [ vec; Num i; ele ])
let make_cons car cdr = Apply (cons_fun, [ car; cdr ])

let (>::) opt lis = match opt with
  | Some t -> t :: lis
  | None -> lis

let rec decompile inter = match inter with
  | InterlangType.Apply (a, b, c) ->
      let a = decompile a in
      let b = decompile b in
      let c = List.map decompile c in
      make_call a (b :: c)
  | Select (s, i) ->
      let s = Symbol (string_of_sym s) in
      make_select s i
  | Ref s -> Symbol (string_of_sym s)
  | Branch (p, t1, t2) -> Branch (decompile p, decompile t1, decompile t2)
  | Int i -> Num i
  | Bool b -> Bool b
  | Closure { proc = proc; env = env } ->
      let proc = decompile proc in
      let env = Symbol (string_of_sym env) in
      make_cons proc env
  | Program (plis, body) ->
      let plis = List.map decompile_proc plis in
      let body = decompile body in
      Define (plis, body)
and decompile_proc proc =
  let { psym; args; record; body } = proc in
  let psym = string_of_sym psym in
  let args =
    let cont_arg = Option.map string_of_sym args.cont_arg in
    let closure_arg = string_of_clsym args.closure_arg in
    let usr_args = List.map string_of_sym args.usr_args in
    closure_arg :: (cont_arg >:: usr_args)
  in
  let def_record, set_record =
    let rsym, rlis = record in
    let sym = string_of_rsym rsym in
    let def_record = Bind { sym = sym; def = make_vector (List.length rlis) } in
    let set_records =
      let rec fn lis idx = match lis with
        | x :: xs ->
            let x = decompile x in
            let x = make_vector_set (Symbol sym) idx x in
            let xs = fn xs (idx + 1) in
            x :: xs
        | [] -> []
      in
      fn rlis 0
    in
    def_record, set_records
  in
  let body = decompile body in
  let body = List.append set_record [ body ] in
  let body = Statement body in
  let body = Define ([ def_record ], body) in
  let body = Lambda (args, body) in
  Bind { sym = psym; def = body }
