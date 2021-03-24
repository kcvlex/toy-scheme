open SymbolType
open ClosureType
open InterlangType

module StrIndexing = Map.Make(String)

let make_indexing symlis =
  let rec aux lis idx map = match lis with
    | x :: xs -> 
        let map = match x with
          | CommonSym s -> StrIndexing.add s idx map
          | _ -> map
        in
        aux xs (idx + 1) map
    | [] -> map 
  in
  aux symlis 0 StrIndexing.empty

let proc_sym_counter = ref 0

let fresh_proc_sym () =
  let res = !proc_sym_counter in
  incr proc_sym_counter; Proc res

let gen_proc_name name = match name with
  | Some s -> s
  | None -> ProcSym (fresh_proc_sym ())

let rec flatten_aux clo proc_list name =
  let rec rec_lis flis plis = match flis with
    | x :: xs ->
        let x, plis = flatten_aux x plis None in
        let xs, plis = rec_lis xs plis in
        (x :: xs, plis)
    | [] -> ([], plis)
  in
  match clo with
    | ClosureType.Apply (a, b, c) ->
        let a, proc_list = flatten_aux a proc_list None in
        let b, proc_list = flatten_aux b proc_list None in
        let c, proc_list = rec_lis c proc_list in
        (Apply (a, b, c), proc_list)
    | ClosureType.Select (sym, i) -> (Select (sym, i), proc_list)
    | ClosureType.Int i -> (Int i, proc_list)
    | ClosureType.Bool b -> (Bool b, proc_list)
    | ClosureType.Ref s -> (Ref s, proc_list)
    | ClosureType.Branch (a, b, c) ->
        let a, proc_list = flatten_aux a proc_list None in
        let b, proc_list = flatten_aux b proc_list None in
        let c, proc_list = flatten_aux c proc_list None in
        (Branch (a, b, c), proc_list)
    | ClosureType.Lambda (args, cl, body) ->
        let psym = gen_proc_name name in
        let pargs = {
          cont_arg = args.cont_arg;
          closure_arg = args.clo_arg;
          usr_args = args.args;
        }
        in
        let record, proc_list =
          let { rsym = rsym; seq = record_list } = cl in
          let fn rele plis =
            let { sym; body; index; local } = rele in
            let body, plis = flatten_aux body plis (Some sym) in
            let body = if local then Closure { proc = body; env = RecordSym rsym } else body in
            (body, plis)
          in
          let rec fnlis lis plis binds = match lis with
            | x :: xs ->
                let x, plis = fn x plis in
                let plis, binds = fnlis xs plis binds in
                (plis, x :: binds)
            | [] -> (plis, [])
          in
          let proc_list, binds = fnlis record_list proc_list [] in
          ((rsym, binds), proc_list)
        in
        let body, proc_list = flatten_aux body proc_list None in
        let proc = { psym = psym; args = pargs; record = record; body = body } in
        let proc_list = proc :: proc_list in
        (Ref psym, proc_list)

let flatten clo =
  let lang, proc_list = flatten_aux clo [] None in
  Program (proc_list, lang)

let rec func2env clo rsym =
  let recf c = func2env c rsym in
  match clo with
    | Apply (a, b, c) -> Apply (recf a, recf b, List.map recf c)
    | Ref s -> (match s with
      | ProcSym p -> Closure ({ proc = clo; env = RecordSym (Option.get rsym) })
      | _ -> Ref s)
    | Branch (a, b, c) -> Branch (recf a, recf b, recf c)
    | Closure { proc; env } -> Closure { proc = (recf proc); env = env }
    | Program (plis, body) ->
      let f p = 
        let body = func2env p.body (Some (fst p.record)) in
        { psym = p.psym; args = p.args; record = p.record; body = body }
      in
      Program (List.map f plis, body)
    | _ -> clo

let compile ast =
  let cps = Cps.cps_trans ast in
  let dbi = DeBruijnIndex.dbi_of_cps cps in
  let dbi = AdmBetaTrans.beta_trans dbi in
  let cps = DeBruijnIndex.cps_of_dbi dbi in
  let clo = Closure.closure_trans cps in
  func2env (flatten clo) None

(*
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
*)
