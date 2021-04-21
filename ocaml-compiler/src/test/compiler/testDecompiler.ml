open Compiler
open Testtool

(*
let () =
  let ast = make_ast source2 in
  let cps = Cps.cps_trans ast in
  let dbi = DeBruijnIndex.dbi_of_cps cps in
  let dbi = AdmBetaTrans.beta_trans dbi in
  let cps = DeBruijnIndex.cps_of_dbi dbi in
  let clo = Closure.closure_trans cps in
  let ast = Closure.ast_of_clo clo in
  print_label "CLO"; print_endline (Ast.code_of_ast ast)
*)

(*
let () =
  let ast = make_ast source2 in
  let cps = Cps.cps_trans ast in
  let dbi = DeBruijnIndex.dbi_of_cps cps in
  let dbi = AdmBetaTrans.beta_trans dbi in
  let cps = DeBruijnIndex.cps_of_dbi dbi in
  let ast = Cps.ast_of_cps cps in
  let code = Ast.code_of_ast ast in
  print_label "CPS"; print_endline code

let () =
  let ast = make_ast source2 in
  let compiled = InterlangCompiler.compile ast in
  let decompiled = InterlangDecompiler.decompile compiled in
  match decompiled with
    | Define (defs, body) ->
        let body = AstType.Apply (AstType.Symbol "__call", [ body; AstType.Symbol "display-f" ]) in
        let ast = AstType.Define (defs, body) in
        let ast = AstType.Lambda ([], ast) in
        let ast = AstType.Apply (ast, []) in
        let code = Ast.code_of_ast ast in
        let code = append_primitives code in
        let code = call_func ^ display_func ^ code in
        print_label "Decompiled"; print_endline code
    | _ -> raise (Invalid_argument "decompiled must be `Define`")
*)
