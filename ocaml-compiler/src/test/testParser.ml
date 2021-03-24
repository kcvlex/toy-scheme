open Compiler
open TestUtil

let () =
  (*
  let ast = make_ast source2 in
  print_label "AST"; print_endline (Ast.code_of_ast ast);
  let cps = Cps.cps_trans ast in
  let show = Ast.code_of_ast (Cps.ast_of_cps cps) in
  print_label "Naive CPS"; print_endline show;
  let dbi = DeBruijnIndex.dbi_of_cps cps in
  let str = DeBruijnIndex.string_of_dbi dbi in
  print_label "De Bruijn Index"; print_endline str;
  let dbi = AdmBetaTrans.beta_trans dbi in
  let str = DeBruijnIndex.string_of_dbi dbi in
  print_label "Beta reduction to administrative redexes"; print_endline str;
  let cps = DeBruijnIndex.cps_of_dbi dbi in
  let ast = Cps.ast_of_cps cps in
  let show = Ast.code_of_ast ast in
  print_label "Restored CPS form"; print_endline show;
  let show = append_primitives show in
  print_label "Generated Code"; print_endline show;
  let clo = Closure.closure_trans cps in
  let ast = Closure.ast_of_clo clo in
  let show = Ast.code_of_ast ast in
  let show = append_primitives show in
  print_label "Closure Transformation"; print_endline show;
  *)
  ()
