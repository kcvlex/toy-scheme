open AstType
  
let rec repeat s n =
  if n = 0 then "" else s ^ repeat s (n - 1)

let string_of_ast node =
  let rec string_of_ast_aux node nest = 
    let prefix = repeat "|" nest in
    let prefix = prefix ^ " " in
    let body = match node with
      | Num n -> (string_of_int n) ^ "\n"
      | Bool true -> "#t\n"
      | Bool false -> "#f\n"
      | Symbol s -> s ^ "\n"
      | Lambda (args, proc) ->
          let args = String.concat " " args in
          "LambdaNode(" ^ args ^ ")\n" ^ (string_of_ast_aux proc (nest + 1))
      | Apply (f, args) ->
          let label = "ApplyNode\n" in
          let f = string_of_ast_aux f (nest + 1) in
          let args = (List.map (fun n -> string_of_ast_aux n (nest + 1)) args) in
          let args = String.concat "" args in
          label ^ f ^ args
      | Define (_, body) -> "!!UNIMPLED!!\n" ^ (string_of_ast_aux body nest)
      | Branch (p, th, els) -> 
          let fn ast = string_of_ast_aux ast (nest + 1) in
          String.concat "\n" [ "BranchNode"; fn p; fn th; fn els ]
    in
    prefix ^ body in
  string_of_ast_aux node 0

let code_of_ast ast =
  let rec code_of_ast_aux ast = match ast with
    | Num n -> string_of_int n
    | Bool true -> "#t"
    | Bool false -> "#f"
    | Symbol s -> s
    | Lambda (args, body) ->
        let args = String.concat " " args in
        let args = "(" ^ args ^ ")" in
        let body = code_of_ast_aux body in
        "(lambda " ^ args ^ " " ^ body ^ ")"
    | Apply (f, args) ->
        let seq = f :: args in
        let seq = List.map (fun x -> code_of_ast_aux x) seq in
        let seq = String.concat " " seq in
        "(" ^ seq ^ ")"
    | Define (blis, body) ->
        let blis = List.map bind2code blis in
        let blis = String.concat " " blis in
        let body = code_of_ast_aux body in
        blis ^ " " ^ body
    | Branch (p, th, els) ->
        let p = code_of_ast_aux p in
        let th = code_of_ast_aux th in
        let els = code_of_ast_aux els in
        let seq = [ "if"; p; th; els ] in
        let seq = String.concat " " seq in
        "(" ^ seq ^ ")"
  and bind2code bind = match bind with
    | Bind { sym = sym; def = def; } -> "(define " ^ sym ^ " " ^ (code_of_ast_aux def) ^ ")"
  in
  let display = Symbol "display" in
  let ast = Apply (ast, [ display ]) in
  code_of_ast_aux ast
