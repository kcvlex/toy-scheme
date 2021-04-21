open Compiler
open Testtool

let decorate clo = match clo with
  | AstType.Let (l, body) ->
      let call = SymbolType.CommonSym "__call" in
      let body = AstType.Apply (AstType.Symbol call, [ body ]) in
      AstType.Let (l, body)
  | _ -> raise (Invalid_argument "AAA")

let add_call s =
  let call = 
    "(define (__call f . args) \
       (if (pair? f) \
           (apply (car f) (cons (cdr f) args)) \
           (apply f args)))" 
  in
  let apply = 
    "(define (apply-clo f args) \
       (if (pair? f) \
           (apply (car f) (cons (cdr f) args)) \
           (apply f args)))" 
  in
  call ^ apply ^ s

let () =
  let code = 
    TestUtil.source2 
    |> Ast.make_ast
    |> Ast.normalize
    |> Cps.cps_trans
    |> AdmBetaReduction.normalize
    |> ANormalization.a_normalize
    |> ANormalization.normalize_binds
    |> Closure.closure_trans
    |> Closure.ast_of_clo
    |> decorate
    |> Ast.code_of_ast
    |> add_call
  in
  TestUtil.print_label "Closure Trans";
  print_endline code
