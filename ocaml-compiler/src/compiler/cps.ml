open SymbolType
open Symbol
open CpsType
open AstType

module SS = Set.Make(String)

let cont_id_counter = ref 0
let cont_param_counter = ref 0

let fresh_cont_id () =
  let num = !cont_id_counter in
  begin
    incr cont_id_counter; 
    ContSym (Cont num)
  end

let fresh_cont_param () =
  let num = !cont_param_counter in
  begin
    incr cont_param_counter;
    ParamSym num
  end

let rec fresh_cont_param_list num =
  if num = 0 then [] else ((fresh_cont_param ()) :: (fresh_cont_param_list (num - 1)))

let to_primitive ast =
  let is_primitive s = begin
    match s with
      | "+" -> true
      | "-" -> true
      | "<" -> true
      | _ -> false
  end in
  match ast with
    | Symbol s when (is_primitive s) -> Some (Primitive s)
    | _ -> None

let make_cps_value value =
  let k = fresh_cont_id () in
  AdmLambda (k, Passing (Ref k, value))

(* `next` is used in Define *)
let rec cps_trans_aux ast defset =
  let recf ast = cps_trans_aux ast defset in
  match ast with
    | Num n -> make_cps_value (Int n)
    | Bool b -> make_cps_value (Bool b)
    | Symbol s -> (match to_primitive ast with
      | Some p -> Ref p
      | None -> 
          if SS.mem s defset then
            Ref (CommonSym s)
          else
            make_cps_value (Ref (CommonSym s)))
    | Lambda (args, body) ->
        let k = fresh_cont_id () in
        let args = List.map (fun x -> (CommonSym x)) args in
        let body = recf body in
        make_cps_value (Lambda (k, args, Passing (body, Ref k)))
    | Apply (f, args) ->
        let merge_args body params cps_lis =
          let merge cps param body = Passing (cps, AdmLambda (param, body)) in
          let body = List.fold_right2 merge cps_lis params body in
          body
        in
        let f = recf f in
        let k = fresh_cont_id () in
        let args = List.map (fun x -> recf x) args in
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
    | Define (blis, body) ->
        let rec add_binds blis set = (match blis with
          | (Bind { sym = sym; def = _; }) :: xs -> add_binds xs (SS.add sym set)
          | _ -> set)
        in
        let defset = add_binds blis defset in
        let blis = List.map (fun x -> binding2cps x defset) blis in
        let body = cps_trans_aux body defset in
        let k = fresh_cont_id () in
        AdmLambda (k, Fix (blis, Passing (body, Ref k)))
    | Branch (p, th, els) ->
        let k = fresh_cont_id () in
        let t = fresh_cont_param () in
        let th = recf th in
        let els = recf els in
        let br = CpsType.Branch (Ref t, Passing (th, Ref k), Passing (els, Ref k)) in
        let br = AdmLambda (t, br) in
        let br = Passing (recf p, br) in
        AdmLambda (k, br)
    | Statement (tlis) ->
        let merge a b =
          let param = fresh_cont_param () in
          Passing (a, AdmLambda (param, b))
        in
        let tlis = List.map recf tlis in
        let k = fresh_cont_id () in
        let body = List.fold_right merge tlis (Ref k) in
        AdmLambda (k, body)
and binding2cps b defset = match b with
  | Bind { sym = sym; def = def; } ->
      let def = cps_trans_aux def defset in
      CpsType.Bind { sym = sym; body = def; }

let cps_trans ast =
  let ast = AlphaTrans.trans ast in
  cps_trans_aux ast SS.empty

let rec ast_of_cps cps = match cps with
  | AdmLambda (sym, body) -> Lambda ([ string_of_sym sym ], ast_of_cps body)
  | ApplyFunc (f, k, args) -> 
      let f = ast_of_cps f in
      let k = ast_of_cps k in
      let args = List.map ast_of_cps args in
      Apply(f, k :: args)
  | Passing (f, c) -> Apply (ast_of_cps f, [ (ast_of_cps c) ])
  | Fix (blis, body) ->
      let blis = List.map ast_of_cpsfix blis in
      let body = ast_of_cps body in
      Define (blis, body)
  | Int i -> Num i
  | Bool b -> Bool b
  | Ref s -> Symbol (string_of_sym s)
  | Lambda (adm, args, body) ->
      let adm = string_of_sym adm in
      let args = List.map string_of_sym args in
      let body = ast_of_cps body in
      Lambda (adm :: args, body)
  | Branch (t, th, els) ->
      let t = ast_of_cps t in
      let th = ast_of_cps th in
      let els = ast_of_cps els in
      Branch (t, th, els)
and ast_of_cpsfix fix = match fix with
  | Bind { sym = sym; body = body; } ->
      let body = ast_of_cps body in
      Bind { sym = sym; def = body; }
