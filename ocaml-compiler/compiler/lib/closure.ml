open ClosureType
open SymbolType
open RefType

module SymMap = Map.Make(Symbol)
module SymSet = Set.Make(Symbol)

let closure_sym_counter = ref 0
let record_counter = ref 0
        
let get_sym b = 
  let CpsType.Bind { sym = sym } = b in sym

let get_body b =
  let CpsType.Bind { body = body } = b in body

let fresh_closure_sym () =
  let res = !closure_sym_counter in
  incr closure_sym_counter; Closure res

let fresh_record_sym () =
  let r = !record_counter in
  incr record_counter; Record r

let make_defsym2def binds =
  let tbl = Hashtbl.create (List.length binds) in
  let rec aux lis = match lis with
    | x :: xs ->
        let CpsType.Bind { sym = sym; body = body } = x in
        Hashtbl.add tbl (CommonSym sym) body; aux xs
    | [] -> ()
  in
  aux binds; tbl

(* For simplicity, include defined value in FV *)
let rec collect_free_vars_aux bindmap cps is_top =
  let recf c = collect_free_vars_aux bindmap c is_top in
  match cps with
    | CpsType.AdmLambda (s, t) -> recf (CpsType.Lambda (s, [], t))
    | CpsType.ApplyFunc (f, k, args) ->
        List.fold_left (fun x y -> SymSet.union x (recf y)) SymSet.empty (f :: k :: args)
    | CpsType.Passing (a, b) ->
        let s1 = recf a in
        let s2 = recf b in
        SymSet.union s1 s2
    | CpsType.Fix (binds, t) ->
        let bindmap =
          List.fold_left (fun x y -> SymMap.add (CommonSym (get_sym y)) false x) bindmap binds in
        let recf c = collect_free_vars_aux bindmap c is_top in
        let set = List.fold_left (fun x y -> SymSet.union (recf (get_body y)) x) SymSet.empty binds in
        let set = SymSet.union set (recf t) in
        set
    | CpsType.Ref sym ->
        let s = Symbol.string_of_sym sym in
        (match sym with
          | Primitive _ -> SymSet.empty
          | _ ->
              if not (SymMap.mem sym bindmap) then
                SymSet.add sym SymSet.empty
              else if not (SymMap.find sym bindmap) then
                SymSet.empty
              else if is_top then
                SymSet.empty
              else
                SymSet.add sym SymSet.empty)
    | CpsType.Lambda (k, args, t) ->
        let bindmap = List.fold_left (fun x y -> SymMap.add y false x) bindmap (k :: args) in
        collect_free_vars_aux bindmap t false
    | CpsType.Branch (p, t1, t2) ->
        List.fold_left (fun x y -> SymSet.union x (recf y)) SymSet.empty [ p; t1; t2 ]
    | _ -> SymSet.empty


let collect_free_vars binds cps args passed_record =
  let map = List.fold_left (fun x y -> SymMap.add y true x) SymMap.empty args in
  let map = List.fold_left (fun x y -> SymMap.add y true x) map passed_record in
  let lis = List.map get_body binds in
  let lis = cps :: lis in
  let res = List.map (fun x -> collect_free_vars_aux map x true) lis in
  List.fold_left (fun x y -> SymSet.union x y) SymSet.empty res


let rec closure_trans_aux cps passed_record created_record clsr_ref =
  let recf c = closure_trans_aux c passed_record created_record clsr_ref in
  match cps with
    | CpsType.AdmLambda (s, t) -> recf (CpsType.Lambda (s, [], t))
    | CpsType.ApplyFunc (f, k, args) -> 
        Apply (recf f, recf k, List.map recf args)
    | CpsType.Passing (a, b) -> Apply (recf a, recf b, [])
    | CpsType.Fix _ -> raise (Invalid_argument "Fix must be at top of Lambda")
    | CpsType.Int i -> Int i
    | CpsType.Bool b -> Bool b
    | CpsType.Ref sym -> (match ClosureRef.reftype clsr_ref sym with 
      | LocalBind (_, rsym, i) -> 
          Select (RecordSym rsym, i)
      | PassedClosure (clsym, i) -> 
          Select (ClosureSym clsym, i)
      | PassingRecord (_, rsym, i) ->
          Select (RecordSym rsym, i)
      | Direct _ -> 
          Ref sym)
    | CpsType.Lambda (k, args, t) -> 
        let passed_record = created_record in
        let binds, body = match t with
          | Fix (a, b) -> (a, b)
          | _ -> ([], t)
        in
        let defined = List.map get_sym binds in
        let defined = List.map (fun x -> CommonSym x) defined in
        let clsym = fresh_closure_sym () in
        let rsym = fresh_record_sym () in
        let passing =
          let fv = collect_free_vars binds t (k :: args) passed_record in
          let res = List.fold_left (fun x y -> SymSet.add y x) fv defined in 
          SymSet.elements res
        in
        let created_record = passing in
        let clsr_ref =
          let passed = (clsym, passed_record) in
          let created = (rsym, created_record) in
          ClosureRef.make ~passed:passed ~created:created ~defined:defined
        in
        let recf c = closure_trans_aux c passed_record created_record clsr_ref in
        let defsym2def = make_defsym2def binds in
        let record =
          let make_element x index =
            let local = ref false in
            let body = match ClosureRef.reftype clsr_ref x with
             | LocalBind _ ->
                 let body = Hashtbl.find defsym2def x in
                 (match body with
                  | Lambda _ -> 
                      local := true; closure_trans_aux body passed_record created_record ClosureRef.empty
                  | AdmLambda _ -> local := true; closure_trans_aux body passed_record created_record ClosureRef.empty
                  | _ -> recf body)
             | PassedClosure (c, idx) -> Select (ClosureSym c, idx)
             | PassingRecord (s, rsym, idx) -> Ref s
             | Direct _ -> Ref x
            in
            { sym = x; body = body; index = index; local = !local; }
          in
          let rec make_record lis idx = match lis with
            | x :: xs ->
                let x = make_element x idx in
                let xs = make_record xs (idx + 1) in
                x :: xs
            | [] -> []
          in
          let seq = make_record passing 0 in
          { rsym = rsym; seq = seq }
        in
        let largs = {
          cont_arg = Some k;
          clo_arg = clsym;
          args = args;
          passed_record = passed_record;
        }
        in
        let body = recf body in
        Lambda (largs, record, body)
    | CpsType.Branch (p, t1, t2) -> Branch (recf p, recf t1, recf t2)


let closure_trans cps = closure_trans_aux cps [] [] ClosureRef.empty

let rec ast_of_clo clo = match clo with
  | Apply (a, b, c) -> 
      let a = ast_of_clo a in
      let b = ast_of_clo b in
      let c = List.map ast_of_clo c in
      AstType.Apply (a, b :: c)
  | Select (s, i) ->
      let s = Symbol.string_of_sym s in
      let s = AstType.Symbol s in
      let i = AstType.Num i in
      let args = [ s; i ] in
      AstType.Apply (AstType.Symbol "SELECT", args)
  | Int i -> AstType.Num i
  | Bool b -> AstType.Bool b
  | Ref s -> AstType.Symbol (Symbol.string_of_sym s)
  | Branch (p, t1, t2) ->
      let p = ast_of_clo p in
      let t1 = ast_of_clo t1 in
      let t2 = ast_of_clo t2 in
      AstType.Branch (p, t1, t2)
  | Lambda (largs, cr, body) ->
      let args = List.map Symbol.string_of_sym largs.args in
      let args = (Symbol.string_of_clsym largs.clo_arg) :: args in
      let args = match largs.cont_arg with
        | Some k -> (Symbol.string_of_sym k) :: args
        | None -> args
      in
      let cr = 
        let bodies = List.map (fun ele -> ast_of_clo ele.body) cr.seq in
        let bodies = AstType.Apply (AstType.Symbol "MAKE-RECORD", bodies) in
        let sym = Symbol.string_of_rsym cr.rsym in
        AstType.Bind { sym = sym; def = bodies }
      in
      let body = ast_of_clo body in
      let binds, body = match body with
        | AstType.Define (a, b) -> a, b
        | _ -> [], body
      in
      AstType.Lambda (args, AstType.Define (cr :: binds, body))
