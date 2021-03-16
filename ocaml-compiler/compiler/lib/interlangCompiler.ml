open InterlangType
open SymbolType
open Symbol

type t =
  | Select of SymbolType.record_sym * int
  | Ref of SymbolType.t
  | Lambda of lambda_args * t * make_record
  | Branch of t * t * t
  | Apply of t * t * t list
  | Fix of (string * t) list * t
  | Int of int
  | Bool of bool
and lambda_args = Args of SymbolType.cont_sym option * SymbolType.record_sym * string list
and make_record = SymbolType.record_sym * t list

let rec lang_of_clo clo rsym = 
  let recf c = lang_of_clo c rsym in
  match clo with
    | ClosureType.Apply (f, k, args) ->
        let f = recf f in
        let k = recf k in
        let args = List.map recf args in
        Apply (f, k, args)
    | ClosureType.Fix (binds, t) ->
        let b2b b = (match b with
          | ClosureType.Bind { sym = sym; body = body } -> (sym, recf body))
        in
        let binds = List.map b2b binds in
        let t = recf t in
        Fix (binds, t)
  | ClosureType.Select i -> Select (rsym, i)
  | ClosureType.Int i -> Int i
  | ClosureType.Bool b -> Bool b
  | ClosureType.Ref sym -> Ref sym
  | ClosureType.Branch (p, t1, t2) -> Branch (recf p, recf t1, recf t2)
  | ClosureType.Lambda (k, c, args, body, _, make) ->
      let record = match c with
        | RecordSym r -> r
        | _ -> raise (Invalid_argument "c in ClosureType.Lambda")
      in
      let args = List.map (fun x -> string_of_sym x) args in
      let k, args = match k with
        | Some (ContSym s) -> (Some s, args)
        | Some s -> (None, (string_of_sym s) :: args)
        | None -> (None, args)
      in
      let body = lang_of_clo body record in
      let make = make_of_clomake make record rsym in
      let args = Args (k, record, args) in
      Lambda (args, body, make)
and make_of_clomake make newsym rsym =
  let trans r = match r with
    | (_, Some t) -> lang_of_clo t rsym
    | (sym, None) -> Ref sym
  in
  (newsym, List.map trans make)

let proc_sym_counter = ref 0

let fresh_proc_sym () =
  let res = !proc_sym_counter in
  incr proc_sym_counter; Proc res

let rec flatten_aux lang record proc_list =
  let rec f_list args lis = match args with
      | x :: xs ->
          let x, lis = flatten_aux x record lis in
          let xs, lis = f_list xs lis in
          (x :: xs, lis)
      | [] -> ([], lis)
  in
  match lang with
    | Select (sym, i) -> (InterlangType.Select (sym, i), proc_list)
    | Ref sym -> (InterlangType.Ref sym, proc_list)
    | Lambda (args, body, make) ->
        let args = match args with
          | Args (k, rsym, lis) -> 
            let rsym = match rsym with
              | Record (-1) -> None
              | Record i -> Some (Record i)
            in
            InterlangType.Args (k, rsym, lis)
        in
        let msym, mlis = make in
        let mlis, proc_list = f_list mlis proc_list in
        let make = (msym, mlis) in
        let body, proc_list = flatten_aux body msym proc_list in
        let psym = fresh_proc_sym () in
        let proc = InterlangType.Procedure (psym, args, make, body) in
        let proc_list = proc :: proc_list in
        (InterlangType.Closure (psym, record), proc_list)
    | Branch (p, t1, t2) ->
        let p, proc_list = flatten_aux p record proc_list in
        let t1, proc_list = flatten_aux t1 record proc_list in
        let t2, proc_list = flatten_aux t2 record proc_list in
        let branch = InterlangType.Branch (p, t1, t2) in
        (branch, proc_list)
    | Apply (f, k, args) ->
        let f, proc_list = flatten_aux f record proc_list in
        let k, proc_list = flatten_aux k record proc_list in
        let args, proc_list = f_list args proc_list in
        (InterlangType.Apply (f, k, args), proc_list)
    | Fix (flis, body) ->
        let l_lis = List.map fst flis in
        let r_lis = List.map snd flis in
        let r_lis, proc_list = f_list r_lis proc_list in
        let flis = List.map2 (fun x y -> (x, y)) l_lis r_lis in
        let body, proc_list = flatten_aux body record proc_list in
        (InterlangType.Let (flis, body), proc_list)
    | Int i -> (Int i, proc_list)
    | Bool b -> (Bool b, proc_list)

let dummy_record = Record (-1)

let trans ast =
  let cps = Cps.cps_trans ast in
  let dbi = DeBruijnIndex.dbi_of_cps cps in
  let dbi = AdmBetaTrans.beta_trans dbi in
  let cps = DeBruijnIndex.cps_of_dbi dbi in
  let clo = Closure.closure_trans cps in
  lang_of_clo clo dummy_record

let flatten lang = 
  let body, defs = flatten_aux lang dummy_record [] in
  Program (defs, body)

let compile ast = flatten (trans ast)


(*
let compile ast = flatten (trans ast)

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
*)
