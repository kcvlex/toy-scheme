open Ast
open Cps
  
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
      | LambdaVar (arg, proc) ->
          let arg = sym2str arg in
          "LambdaVarNode(" ^ arg ^ ")\n" ^ (ast2str_aux proc (nest + 1))
      | Apply (f, args) ->
          let label = "ApplyNode\n" in
          let f = ast2str_aux f (nest + 1) in
          let args = (List.map (fun n -> ast2str_aux n (nest + 1)) args) in
          let args = String.concat "" args in
          label ^ f ^ args
      | Define (name, def) -> "DefineNode(" ^ (sym2str name) ^ ")\n" ^ (ast2str_aux def (nest + 1))
      | Cons (car, cdr) -> "ConsNode\n" ^ (ast2str_aux car (nest + 1)) ^ (ast2str_aux cdr (nest + 1))
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
    | LambdaVar (arg, body) ->
        let arg = ast2code_aux arg in
        let body = ast2code_aux body in
        "(lambda " ^ arg ^ " " ^ body ^ ")"
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
        let cdr = ast2code_aux cdr in
        car ^ " " ^ cdr
  in
  let add = 
    begin
      let k0 = Symbol "k0" in
      let k1 = Symbol "k1" in
      let a0 = Symbol "a0" in
      let a1 = Symbol "a1" in
      let op = Symbol "+" in
      let body = Ast.Apply (op, [ a0; a1 ]) in
      let body = Ast.Lambda ([ a1 ], Ast.Lambda ([ k1 ], Ast.Apply (k1, [ body ]))) in
      let body = Ast.Lambda ([ a0 ], Ast.Lambda ([ k0 ], Ast.Apply (k0, [ body ]))) in
      Define (Symbol "add", body)
    end
  in
  let display = 
    begin
      let k = Symbol "k" in
      let display = Symbol "display" in
      let display = Ast.Lambda ([ k ], (Apply (display, [ k ]))) in
      display
    end
  in
  let ast = Ast.Cons (add, Apply (ast, [ display ])) in
  ast2code_aux ast

let rec cps2ast cps = match cps with
  | Program cont -> cps_cont2ast cont
  | PassCont (f, cont) -> 
      let f = cps2ast f in
      let cont = cps_cont2ast cont in
      Apply (f, [ cont ])
  | CallCont (cont, cps) ->
      let cont = cps_cont2ast cont in
      let arg = cps2ast cps in
      Apply (cont, [ arg ])
  | Cont c -> cps_cont2ast c
  | Triv t -> cps_triv2ast t
and cps_triv2ast triv = match triv with
  | Int i -> Num i
  | Bool b -> Bool b
  | Sym s -> cps_sym2ast s
  | Lambda (arg, body) ->
      let arg = cps_sym2ast arg in
      let body = cps2ast body in
      Ast.Lambda ([ arg ], body)
  | Cons (car, cdr) -> Cons ((cps2ast car), (cps2ast cdr))
  | Def (sym, body) -> Define ((cps_sym2ast sym), (cps2ast body))
and cps_cont2ast cont = match cont with
  | ContSym s -> cps_sym2ast s
  | ContFunc (k, cps) -> Lambda ([ (cps_sym2ast k) ], (cps2ast cps))
and cps_sym2ast sym = match sym with
  | UserSym s -> Symbol s
  | CpsContSym k -> Symbol ("k" ^ (string_of_int k))
  | CpsParamSym v -> Symbol ("v" ^ (string_of_int v))
