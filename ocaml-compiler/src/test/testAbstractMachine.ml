open Compiler
open TestUtil

let decorate clo = match clo with
  | ClosureType.Bind (l, body) ->
      let display = ClosureType.Primitive "display" in
      let body = ClosureType.Call (display, [ body ]) in
      ClosureType.Bind (l, body)
  | _ -> raise (Invalid_argument "AAA")

let () =
  let code = 
    source3 |> Ast.make_ast
            |> Ast.normalize
            |> Cps.cps_trans
            |> AdmBetaReduction.normalize
            |> ANormalization.a_normalize
            |> ANormalization.normalize_binds
            |> Closure.closure_trans
            |> AbstractMachine.translate
            |> decorate
            |> AbstractMachine.eval
  in
  print_label "Abstract Machine"; print_endline code
