open Compiler
open TestUtil

let decorate clo = match clo with
  | AstType.Let (l, body) ->
      let display = AstType.Symbol "display" in
      let car = AstType.Apply (AstType.Primitive "car", [ body ]) in
      let cdr = AstType.Apply (AstType.Primitive "cdr", [ body ]) in
      let called = AstType.Apply (car, [ cdr ]) in
      let output = AstType.Apply (display, [ called ]) in
      AstType.Let (l, output)
  | _ -> raise (Invalid_argument "AAA")

let add_call s =
  let call = "(define (__call f . args) (if (pair? f) (apply (car f) (cons (cdr f) args)) (apply f args)))" in
  let apply = "(define (apply-clo f args) (if (pair? f) (apply (car f) (cons (cdr f) args)) (apply f args)))" in
  call ^ apply ^ s
 
let () =
  let code = 
    source3 |> Ast.make_ast
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
  print_label "Closure Trans"; print_endline code
