open Interlang

let string_of_rsym rsym = match rsym with
  | Record i -> "__r" ^ (string_of_int i)

let string_of_csym csym = match csym with
  | Cont i -> "__k" ^ (string_of_int i)

let string_of_intersym sym = match sym with
  | RecordSym r -> string_of_rsym r
  | ContSym c -> string_of_csym c
  | Primitive "+" -> "add"
  | Primitive "-" -> "sub"
  | Primitive "<" -> "less"
  | Primitive s -> s
  | CommonSym s -> s

let rec interlang_of_clo clo rsym = 
  let recf c = interlang_of_clo c rsym in
  match clo with
    | Closure.Apply (f, k, args) ->
        let f = recf f in
        let k = recf k in
        let args = List.map recf args in
        Apply (f, k, args)
    | Closure.Fix (binds, t) ->
        let b2b b = (match b with
          | Closure.Bind { sym = sym; body = body } -> (sym, recf body))
        in
        let binds = List.map b2b binds in
        let t = recf t in
        Fix (binds, t)
  | Closure.Select i -> Select (rsym, i)
  | Closure.Int i -> Int i
  | Closure.Bool b -> Bool b
  | Closure.Ref sym -> Ref (intersym_of_closym sym)
  | Closure.Branch (p, t1, t2) -> Branch (recf p, recf t1, recf t2)
  | Closure.Lambda (k, c, args, body, _, make) ->
      let k = Option.map intersym_of_closym k in
      let c = intersym_of_closym c in
      let record = match c with
        | RecordSym r -> r
        | _ -> raise (Invalid_argument "c in Closure.Lambda")
      in
      let args = List.map intersym_of_closym args in
      let k, args = match k with
        | Some (ContSym s) -> (Some s, args)
        | Some s -> (None, s :: args)
        | None -> (None, args)
      in
      let args = List.map string_of_intersym args in
      let body = interlang_of_clo body record in
      let make = intermake_of_clomake make record rsym in
      let args = Args (k, record, args) in
      Lambda (args, body, make)
and intersym_of_closym sym = match sym with
  | Closure.UserSym s -> CommonSym s
  | Closure.Primitive s -> Primitive s
  | Closure.ContSym i -> ContSym (Cont i)
  | Closure.ParamSym i -> CommonSym ("__t" ^ (string_of_int i))
  | Closure.ClosureSym i -> RecordSym (Record i)
and intermake_of_clomake make newsym rsym =
  let trans r = match r with
    | (_, Some t) -> interlang_of_clo t rsym
    | (sym, None) -> Ref (intersym_of_closym sym)
  in
  (newsym, List.map trans make)

let trans ast =
  let cps = CpsTrans.cps_transformation ast in
  let db_cps = DeBruijnIndexTrans.de_bruijn_indexing cps in
  let db_cps = AdmBetaTrans.beta_trans db_cps in
  let cps = DeBruijnIndexTrans.restore db_cps in
  let clo = ClosureTrans.closure_trans cps in
  interlang_of_clo clo (Record (-1))


let rec join args delim = match args with
  | x1 :: x2 :: xs -> x1 ^ delim ^ (join (x2 :: xs) delim)
  | x1 :: [] -> x1
  | [] -> ""

let string_of_apply f args = f ^ "(" ^ (join args ", ") ^ ")"

let rec string_of_interlang ilang = match ilang with
  | Select (r, i) ->
      let r = string_of_rsym r in
      string_of_apply "SELECT" [ r; string_of_int i ]
  | Ref sym -> string_of_intersym sym
  | Lambda (args, body, make) ->
      let args = string_of_interargs args in
      let args = "[" ^ (join args ", ") ^ "]" in
      let make = string_of_make make in
      let body = string_of_interlang body in
      let body = "{ " ^ body ^ " }" in
      let res = join [ args; make; body ] " " in
      "(" ^ res ^ ")"
  | Branch (p, t1, t2) ->
      let p = string_of_interlang p in
      let t1 = string_of_interlang t1 in
      let t2 = string_of_interlang t2 in
      string_of_apply "BRANCH " [ p; t1; t2 ]
  | Apply (f, k, args) ->
      let f = string_of_interlang f in
      let k = string_of_interlang k in
      let args = List.map string_of_interlang args in
      string_of_apply "APPLY " (f :: k :: args)
  | Fix (binds, t) ->
      let b2s b =
        let s, t = b in
        let res = s ^ " : " ^ (string_of_interlang t) in
        "[ " ^ res ^ " ]"
      in
      let binds = List.map b2s binds in
      let binds = join binds " " in
      let body = string_of_interlang t in
      let res = binds ^ " " ^ body in
      "FIX (" ^ res ^ ")"
  | Int i -> string_of_int i
  | Bool true -> "#t"
  | Bool false -> "#f"
and string_of_interargs args = match args with
  | Args (k, r, lis) ->
      let k = Option.map string_of_csym k in
      let r = string_of_rsym r in
      (match k with
        | Some s -> s :: r :: lis
        | None -> r  :: lis)
and string_of_make make =
  let r, lis = make in
  let lis = List.map string_of_interlang lis in
  let lis = (string_of_rsym r) :: lis in
  string_of_apply "RECORD " lis
