open SymbolType
open Symbol
open CpsType

let cont_id_slot = SlotNumber.make (fun x -> ContSym x)
let cont_param_slot = SlotNumber.make (fun x -> ParamSym x)
let cont_prim_slot = SlotNumber.make (fun x -> "__prim_sym_" ^ (string_of_int x))

let make_cps_value value =
  let k = SlotNumber.fresh cont_id_slot in
  AdmLambda (k, Passing (Ref k, value))

let rec cps_trans ast = match ast with
  | AstType.Num n -> make_cps_value (Int n)
  | AstType.Bool b -> make_cps_value (Bool b)
  | AstType.Symbol ((PrimitiveSym _) as p) -> Ref p
  | AstType.Symbol s -> make_cps_value (Ref s)
  | AstType.Nil -> make_cps_value (Nil)
  | AstType.Quote q -> make_cps_value (Quote q)
  | AstType.Lambda (args, larg, body) ->
      let k = SlotNumber.fresh cont_id_slot in
      let args = List.map (fun x -> CommonSym x) args in
      let larg = Option.map (fun x -> CommonSym x) larg in
      let body = cps_trans body in
      make_cps_value (Lambda (k, args, larg, Passing (body, Ref k)))
  | AstType.Apply (f, args) ->
      let is_prim = match f with
        | AstType.Symbol (PrimitiveSym _) -> true
        | _ -> false
      in
      let merge_args body params cps_lis =
        let merge cps param body = Passing (cps, AdmLambda (param, body)) in
        List.fold_right2 merge cps_lis params body
      in
      let k = SlotNumber.fresh cont_id_slot in
      let f = cps_trans f in
      let args = List.map cps_trans args in
      let params = SlotNumber.fresh_list cont_param_slot (List.length args) in
      if is_prim then
        ApplyFunc (f, Ref k, List.map (fun x -> Ref x) params)
        |> fun x -> merge_args x params args
        |> fun x -> AdmLambda (k, x)
      else
        let t = SlotNumber.fresh cont_param_slot in
        ApplyFunc (Ref t, Ref k, List.map (fun x -> Ref x) params)
        |> fun x -> merge_args x (t :: params) (f :: args)
        |> fun x -> AdmLambda (k, x)
  | AstType.Define _ -> raise (Invalid_argument "Define must be removed")
  | AstType.Let (blis, body) ->
      let rec fn lis = match lis with
        | [] -> cps_trans body
        | x :: xs ->
            let sym, def = x in
            let body = cps_trans def in
            let k = SlotNumber.fresh cont_id_slot in
            let t = SlotNumber.fresh cont_param_slot in
            let xs = fn xs in
            xs |> fun a -> Passing (a, Ref k)
               |> fun a -> Let ((sym, Ref t), a)
               |> fun a -> AdmLambda (t, a)
               |> fun a -> Passing (body, a)
               |> fun a -> AdmLambda (k, a)
      in
      fn blis
  | AstType.Branch (p, t1, t2) ->
      let k = SlotNumber.fresh cont_id_slot in
      let t = SlotNumber.fresh cont_param_slot in
      let p = cps_trans p in
      let t1 = cps_trans t1 in
      let t2 = cps_trans t2 in
      let res = Branch (Ref t, Passing (t1, Ref k), Passing (t2, Ref k)) in
      res |> fun x -> AdmLambda (t, x)
          |> fun x -> Passing (p, x)
          |> fun x -> AdmLambda (k, x)
  | AstType.Statement l ->
      (* FIXME *)
      let merge a b = Passing (a, AdmLambda (CommonSym "_", b)) in
      let k = SlotNumber.fresh cont_id_slot in
      let body =
        l |> List.map cps_trans
          |> fun x -> List.fold_right merge x (Ref k)
      in
      AdmLambda (k, body)

let gen_exargs_prim op =
  let k = SlotNumber.fresh cont_prim_slot in
  let args = SlotNumber.fresh cont_prim_slot in
  op |> fun x -> AstType.Symbol (PrimitiveSym x)
     |> fun x -> [ x; AstType.Symbol (CommonSym args) ]
     |> fun x -> AstType.Apply (AstType.Symbol (PrimitiveSym APPLY), x)
     |> fun x -> AstType.Apply (AstType.Symbol (CommonSym k), [ x ])
     |> fun x -> AstType.Lambda ([ k ], Some args, x)

let gen_unary_prim op =
  let k = SlotNumber.fresh cont_prim_slot in
  let arg = SlotNumber.fresh cont_prim_slot in
  op |> fun x -> AstType.Symbol (PrimitiveSym x)
     |> fun x -> AstType.Apply (x, [ AstType.Symbol (CommonSym arg) ])
     |> fun x -> AstType.Apply (AstType.Symbol (CommonSym k), [ x ])
     |> fun x -> AstType.Lambda ([ k; arg ], None, x)

let gen_binop_prim op =
  let k = SlotNumber.fresh cont_prim_slot in
  let arg1 = SlotNumber.fresh cont_prim_slot in
  let arg2 = SlotNumber.fresh cont_prim_slot in
  let args = [ arg1; arg2 ] in
  op |> fun x -> AstType.Symbol (PrimitiveSym x)
     |> fun x -> AstType.Apply (x, List.map (fun x -> AstType.Symbol (CommonSym x)) args)
     |> fun x -> AstType.Apply (AstType.Symbol (CommonSym k), [ x ])
     |> fun x -> AstType.Lambda (k :: args, None, x)

let gen_apply () =
  let k = SlotNumber.fresh cont_prim_slot in
  let f = SlotNumber.fresh cont_prim_slot in
  let args = SlotNumber.fresh cont_prim_slot in
  [ k; args ] |> List.map (fun x -> AstType.Symbol (CommonSym x))
              |> fun x -> AstType.Apply (AstType.Symbol (PrimitiveSym CONS), x)
              |> fun x -> [ AstType.Symbol (CommonSym f); x ]
              |> fun x -> AstType.Apply (AstType.Symbol (PrimitiveSym APPLY), x)
              |> fun x -> AstType.Lambda ([ k; f; args ], None, x)

let replace_primitive p =
  match p with
    | APPLY -> gen_apply ()
    | ADD | SUB | MUL | DIV | LIST -> gen_exargs_prim p
    | EQ | LESS | CONS | LISTREF -> gen_binop_prim p
    | CAR | CDR | NULL | DISPLAY -> gen_unary_prim p
    | MAP -> raise (Invalid_argument "MAP")

let rec ast_of_cps cps = match cps with 
  | Int i -> AstType.Num i
  | Bool b -> AstType.Bool b
  | Nil -> AstType.Nil
  | Quote q -> AstType.Quote q
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
  | ApplyFunc (Ref (PrimitiveSym p), k, args) ->
      let f = replace_primitive p in
      let args = List.map ast_of_cps (k :: args) in
      AstType.Apply (f, args)
  | ApplyFunc (f, k, args) ->
      let f = ast_of_cps f in
      let args = List.map ast_of_cps (k :: args) in
      AstType.Apply (f, args)
  | Passing (f, k) -> AstType.Apply (ast_of_cps f, [ ast_of_cps k ])
  | Let ((s, t), body) ->
      let t = ast_of_cps t in
      let body = ast_of_cps body in
      AstType.Let ([ (s, t) ], body)
  | Ref s -> AstType.Symbol s
  | Branch (a, b, c) -> AstType.Branch (ast_of_cps a, ast_of_cps b, ast_of_cps c)
  | _ -> raise (Invalid_argument "AAA")
