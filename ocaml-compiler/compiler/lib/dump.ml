open Ast
  
let rec repeat s n =
  if n = 0 then "" else s ^ repeat s (n - 1)

exception ASTException of string

let join vs delim = match vs with
  | [] -> ""
  | x :: [] -> x
  | x :: xs -> List.fold_left (fun x y -> x ^ delim ^ y) x xs

let ast2str node =
  let sym2str node = (match node with
    | Symbol s -> s
    | _ -> raise (ASTException "Error of Symbol")) 
  in
  let rec ast2str_aux node nest = 
    let prefix = repeat "|" nest in
    let prefix = prefix ^ " " in
    let body = match node with
      | Num n -> (string_of_int n) ^ "\n"
      | Bool true -> "#t\n"
      | Bool false -> "#f\n"
      | Symbol s -> s ^ "\n"
      | Lambda (args, proc) ->
          let args = String.concat " " (List.map sym2str args) in
          "LambdaNode(" ^ args ^ ")\n" ^ (ast2str_aux proc (nest + 1))
      | Apply (f, args) ->
          let label = "ApplyNode\n" in
          let f = ast2str_aux f (nest + 1) in
          let args = (List.map (fun n -> ast2str_aux n (nest + 1)) args) in
          let args = String.concat "" args in
          label ^ f ^ args
      | Define (name, def) -> "DefineNode(" ^ (sym2str name) ^ ")\n" ^ (ast2str_aux def (nest + 1))
      | Cons (car, cdr) ->
          let cdr = Option.map (fun x -> ast2str_aux x (nest + 1)) cdr in
          let cdr = Option.value cdr ~default:"" in
          "ConsNode\n" ^ (ast2str_aux car (nest + 1)) ^ cdr
    in
    prefix ^ body in
  ast2str_aux node 0

let ast2code ast =
  let rec ast2code_aux ast = match ast with
    | Num n -> string_of_int n
    | Bool true -> "#t"
    | Bool false -> "#f"
    | Symbol s -> s
    | Lambda (args, body) ->
        let args = List.map (fun x -> (ast2code_aux x)) args in
        let args = "(" ^ (join args " ") ^ ")" in
        let body = ast2code_aux body in
        "(lambda " ^ args ^ " " ^ body ^ ")"
    | Apply (f, args) ->
        let seq = f :: args in
        let seq = List.map (fun x -> ast2code_aux x) seq in
        "(" ^ (join seq " ") ^ ")"
    | Define (sym, def) ->
        let sym = ast2code_aux sym in
        let def = ast2code_aux def in
        "(define " ^ sym ^ " " ^ def ^ ")"
    | Cons(car, cdr) ->
        let car = ast2code_aux car in
        let cdr = match cdr with
          | Some c -> ast2code_aux c
          | None -> ""
        in
        car ^ " " ^ cdr
  in
  let display = 
    begin
      let k = Symbol "k" in
      let display = Symbol "display" in
      let display = Ast.Lambda ([ k ], (Apply (display, [ k ]))) in
      display
    end
  in
  (* let ast = Ast.Cons (add, Apply (ast, [ display ])) in *)
  let ast = Apply (ast, [ display ]) in
  ast2code_aux ast
