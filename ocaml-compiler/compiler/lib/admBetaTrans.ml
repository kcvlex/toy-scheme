open DeBruijnIndex

let replace_term dbi_cps assign =
  let rec replace_aux cps adv =
    let recf x = replace_aux x adv in
    (match cps with
      | AdmLambda (sym, body) -> AdmLambda (sym, replace_aux body (adv + 1))
      | ApplyFunc (f, k, args) -> ApplyFunc (recf f, recf k, List.map recf args)
      | Passing (a, b) -> Passing (recf a, recf b)
      | Bind (sym, a, b) -> Bind (sym, recf a, recf b)
      | RefIndex i when i = adv -> (fix_pos assign adv (-1))
      | RefIndex i when adv < i -> RefIndex (i - 1)
      | Lambda (sym, syms, t, len) -> Lambda (sym, syms, replace_aux t (adv + len), len)
      | _ -> cps)
  and fix_pos assign adv_when_replaced adv =
    let recf x = fix_pos x adv_when_replaced adv in
    (match assign with
      | AdmLambda (sym, body) -> AdmLambda (sym, fix_pos body adv_when_replaced (adv + 1))
      | ApplyFunc (f, k, args) -> ApplyFunc (recf f, recf k, List.map recf args)
      | Passing (a, b) -> Passing (recf a, recf b)
      | Bind (sym, a, b) -> Bind (sym, recf a, recf b)
      | RefIndex i when adv < i -> RefIndex (adv_when_replaced + i)
      | Lambda (sym, syms, t, len) -> Lambda (sym, syms, fix_pos t adv_when_replaced (adv + len), len)
      | _ -> assign)
  in
  replace_aux dbi_cps 0

let beta_step dbi_cps = 
  let update = ref false in
  let rec beta_step_aux dbi_cps depth = 
    let beta_rec0 cps = beta_step_aux cps depth in
    let beta_rec1 cps = beta_step_aux cps (depth + 1) in
    match dbi_cps with
      | Passing (AdmLambda (_, p), t) -> update := true; replace_term p t
      | AdmLambda (sym, t) -> AdmLambda (sym, beta_rec1 t)
      | ApplyFunc (f, k, args) -> 
          ApplyFunc (beta_rec0 f, beta_rec0 k, List.map beta_rec0 args)
      | Passing (a, b) -> Passing (beta_rec0 a, beta_rec0 b)
      | Bind (x, t, body) -> Bind (x, beta_rec0 t, beta_rec0 body)
      | Lambda (k, args, body, len) -> Lambda (k, args, beta_step_aux body (depth + len), len)
      | _ -> dbi_cps
  in
  let res = beta_step_aux dbi_cps 0 in
  (res, !update)

let rec beta_trans dbi_cps =
  let res = beta_step dbi_cps in
  if snd res then beta_trans (fst res) else (fst res)
