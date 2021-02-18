open Scheme_compiler.Ast

let () =
  let ast = 
    Lambda(
      [ "func"; "arg1"; "arg2" ],
      Apply(
        Value("+"),
        [ Value("arg1"); Value("arg2"); Value("42") ]
      )) in
  let s = ast2str ast in
  print_endline s
