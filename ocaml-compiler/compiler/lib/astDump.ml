open Ast

let ast2str node =
  let rec repeat s n =
    if n = 0 then "" else s ^ repeat s (n - 1) in
  let rec ast2str_aux node nest = 
    let prefix = repeat "|" nest in
    let prefix = prefix ^ " " in
    let body = match node with
      | Num n -> (string_of_int n) ^ "\n"
      | Bool b -> (string_of_bool b) ^ "\n"
      | Symbol s -> s ^ "\n"
      | Lambda(args, proc) -> 
          let args = String.concat " " args in
          "LambdaNode(" ^ args ^ ")\n" ^ (ast2str_aux proc (nest + 1))
      | Apply(f, args) ->
          let label = "ApplyNode\n" in
          let f = ast2str_aux f (nest + 1) in
          let args = (List.map (fun n -> ast2str_aux n (nest + 1)) args) in
          let args = String.concat "" args in
          label ^ f ^ args
      | Define(name, def) -> "DefineNode(" ^ name ^ ")\n" ^ (ast2str_aux def (nest + 1))
      | Cons(car, cdr) -> "ConsNode\n" ^ (ast2str_aux car (nest + 1)) ^ (ast2str_aux cdr (nest + 1))
    in
    prefix ^ body in
  ast2str_aux node 0
