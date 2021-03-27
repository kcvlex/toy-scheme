open SymbolType
open Symbol
open CpsType

let cont_id_slot = SlotNumber.make (fun x -> ContSym (Cont x))
let cont_param_slot = SlotNumber.make (fun x -> ParamSym x)

let make_cps_value value =
  let k = SlotNumber.fresh cont_id_slot in
  AdmLambda (k, Passing (RefDirect k, value))

let decompose_bind (b: AstType.bind) = (b.sym, b.def)

let rec cps_trans ast = match ast with
  | AstType.Num n -> make_cps_value (Int n)
  | AstType.Bool b -> make_cps_value (Bool b)
  | AstType.Primitive s -> RefDirect (Primitive ("cps-" ^ s))
  | AstType.Symbol s -> RefDirect (CommonSym s)
  | AstType.Lambda (args, larg, body) ->
      let k = SlotNumber.fresh cont_id_slot in
      let args = List.map (fun x -> CommonSym x) args in
      let larg = Option.map (fun x -> CommonSym x) larg in
      let body = cps_trans body in
      make_cps_value (Lambda (k, args, larg, Passing (body, RefDirect k)))
  | AstType.Apply (f, args) ->
      let merge_args body params cps_lis =
        let merge cps param body = Passing (cps, AdmLambda (param, body)) in
        let body = List.fold_right2 merge cps_lis params body in
        body
      in
      let k = SlotNumber.fresh cont_id_slot in
      let f = cps_trans f in
      let args = List.map cps_trans args in
      let params = SlotNumber.fresh_list cont_param_slot (List.length args) in
      let merged = (match f with
        | RefDirect (Primitive _) ->
            let body = ApplyFunc (f, RefDirect k, List.map (fun x -> RefDirect x) params) in
            merge_args body params args
        | _ -> 
            let t = SlotNumber.fresh cont_param_slot in
            let body = ApplyFunc (RefDirect t, RefDirect k, List.map (fun x -> RefDirect x) params) in
            merge_args body (t :: params) (f :: args))
      in
      AdmLambda (k, merged)
  | AstType.Define _ -> raise (Invalid_argument "Define must be removed")
  | AstType.Let (blis, body) ->
      let k = SlotNumber.fresh cont_id_slot in
      let body =
        body |> cps_trans
             |> fun x -> Passing (x, RefDirect k)
      in
      let rec fn lis = match lis with
        | [] -> Passing (body, RefDirect k)
        | x :: xs ->
            let sym, def = decompose_bind x in
            let body = cps_trans def in
            let t = SlotNumber.fresh cont_param_slot in
            let xs = fn xs in
            xs |> fun xs -> Let ((sym, RefDirect t), xs)
               |> fun xs -> AdmLambda (t, xs)
               |> fun xs -> Passing (body, xs)
      in
      AdmLambda (k, fn blis)
  | AstType.Branch (p, t1, t2) ->
      let k = SlotNumber.fresh cont_id_slot in
      let t = SlotNumber.fresh cont_param_slot in
      let p = cps_trans p in
      let t1 = cps_trans t1 in
      let t2 = cps_trans t2 in
      let res = Branch (RefDirect t, Passing (t1, RefDirect k), Passing (t2, RefDirect k)) in
      res |> fun x -> AdmLambda (t, x)
          |> fun x -> Passing (p, x)
          |> fun x -> AdmLambda (k, x)
  | AstType.Statement l ->
      let merge a b = Passing (a, AdmLambda (CommonSym "_", b)) in
      let k = SlotNumber.fresh cont_id_slot in
      let body =
        l |> List.map cps_trans
          |> fun x -> List.fold_right merge x (RefDirect k)
      in
      AdmLambda (k, body)
  | AstType.RefBox s -> RefBox (CommonSym s)
  | AstType.MakeBox t -> MakeBox (cps_trans t)

let rec ast_of_cps cps = match cps with 
  | Int i -> AstType.Num i
  | Bool b -> AstType.Bool b
  | AdmLambda (k, body) ->
      let k = string_of_sym k in
      let body = ast_of_cps body in
      AstType.Lambda ([ k ], None, body)
  | Lambda (k, args, larg, body) ->
      let k = string_of_sym k in
      let args = List.map string_of_sym args in
      let larg = Option.map string_of_sym larg in
      let body = ast_of_cps body in
      AstType.Lambda (k :: args, larg, body)
  | ApplyFunc (f, k, args) ->
      let f = ast_of_cps f in
      let args = List.map ast_of_cps (k :: args) in
      AstType.Apply (f, args)
  | Passing (f, k) -> AstType.Apply (ast_of_cps f, [ ast_of_cps k ])
  | Let ((s, t), body) ->
      let t = ast_of_cps t in
      let body = ast_of_cps body in
      AstType.Let ([ { sym = s; def = t } ], body)
  | RefDirect s -> AstType.Symbol (string_of_sym s)
  | RefBox s -> AstType.RefBox (string_of_sym s)
  | MakeBox t -> AstType.MakeBox (ast_of_cps t)
  | Branch (a, b, c) -> AstType.Branch (ast_of_cps a, ast_of_cps b, ast_of_cps c)
  | _ -> raise (Invalid_argument "AAA")
