open AstType
open Util

module SMap = Map.Make(String)

let y_star = "Y_star"
let y_star_body =
  "(lambda l \
     ((lambda (u) (u u)) \
      (lambda (p) \
        (map (lambda (li) (lambda x (apply (apply li (p p)) x))) l))))"

let open_map = "open_map"
let open_map_body =
  "(lambda (rec f l) \
     (if (null? l) \
         () \
         (cons (f (car l)) (rec rec f (cdr l)))))"

let map = "map"
let map_body = "(lambda (f l) (open_map open_map f l))"
  
let rec code_of_ast ast =
  let make_lambda args body = "(lambda " ^ args ^ " " ^ body ^ ")" in
  let sur_paren s = "(" ^ s ^ ")" in
  match ast with
    | Num i -> string_of_int i
    | Bool true -> "#t"
    | Bool false -> "#f"
    | Symbol s -> Symbol.string_of_sym s
    | Nil -> "()"
    | Quote s -> "`" ^ (code_of_ast s)
    | Lambda ([], None, body) -> make_lambda "()" (code_of_ast body)
    | Lambda ([], Some s, body) -> make_lambda s (code_of_ast body)
    | Lambda (args, None, body) -> 
        make_lambda ("(" ^ (String.concat " " args) ^ ")") (code_of_ast body)
    | Lambda (args, Some s, body) ->
        let args =
          args |> String.concat " "
               |> fun x -> x ^ " . " ^ s
               |> sur_paren
        in
        make_lambda args (code_of_ast body)
    | Apply (f, args) -> 
        (f :: args) |> List.map code_of_ast
                    |> String.concat " "
                    |> sur_paren
    | Define (bl, body) ->
        let f b =
          let s, t = b in
          let t = code_of_ast t in
          "(define " ^ s ^ " " ^ t ^ ")"
        in
        let bl = 
          bl |> List.map f
             |> String.concat " "
        in
        let body = code_of_ast body in
        bl ^ " " ^ body
    | Let (bl, body) ->
        let f b =
          let s, t = b in
          let t = code_of_ast t in
          sur_paren (s ^ " " ^ t)
        in
        let bl =
          bl |> List.map f
             |> String.concat " "
             |> sur_paren
        in
        let body = code_of_ast body in
        "(let* " ^ bl ^ body ^ ") "
    | Branch (a, b, c) ->
        let a = code_of_ast a in
        let b = code_of_ast b in
        let c = code_of_ast c in
        Printf.sprintf "(if %s %s %s)" a b c
    | Statement l ->
        let s = 
          l |> List.map code_of_ast
            |> String.concat " "
        in
        "(begin " ^ s ^ ")"

let make_let name expr body =
  Printf.sprintf "(let ((%s %s)) %s)" name expr body

let rename =
  let sl = SlotNumber.make (fun x -> "__user" ^ (string_of_int x) ^ "_") in
  fun s -> (SlotNumber.fresh sl) ^ s

let add_defs map src dst =
  List.fold_left2 (fun x y z -> SMap.add y (if y = "Y_star" then y else z) x) map src dst

let (>::) x xs = match x with
  | Some s -> s :: xs
  | None -> xs

let rec alpha_trans_aux ast symmap =
  let recf ast = alpha_trans_aux ast symmap in
  let update_map binds map =
    let bsyms = List.map fst binds in
    let renames = List.map rename bsyms in
    add_defs map bsyms renames
  in
  let trans_binds binds map =
    let fb b = 
      let s, t = b in
      (SMap.find s map, alpha_trans_aux t map)
    in
    List.map fb binds
  in
  match ast with
    | Symbol (CommonSym s) -> (match SMap.find_opt s symmap with
      | Some t -> Symbol (CommonSym t)
      | None -> Symbol (CommonSym s))
    | Symbol (PrimitiveSym MAP) -> alpha_trans_aux (Symbol (CommonSym "map")) symmap  (* FIXME *)
    | Symbol (PrimitiveSym _) -> ast
    | Symbol _ -> raise (Invalid_argument "AST doesn't have contsym")
    | Lambda (args, larg, body) ->
        let all = larg >:: args in
        let renames = List.map rename all in
        let symmap = add_defs symmap all renames in
        let body = alpha_trans_aux body symmap in
        (match larg with
          | Some _ ->
              let larg, arg = (List.hd renames, List.tl renames) in
              Lambda (args, Some larg, body)
          | None -> Lambda (renames, None, body))
    | Apply (f, args) -> Apply (recf f, List.map recf args)
    | Define (binds, body) ->
        let symmap = update_map binds symmap in
        let binds = trans_binds binds symmap in
        let body = alpha_trans_aux body symmap in
        Define (binds, body)
    | Let (binds, body) ->
        let symmap = update_map binds symmap in
        let binds = trans_binds binds symmap in
        let body = alpha_trans_aux body symmap in
        Let (binds, body)
    | Branch (a, b, c) -> Branch (recf a, recf b, recf c)
    | Statement lis -> Statement (List.map recf lis)
    | _ -> ast

let alpha_trans ast = alpha_trans_aux ast SMap.empty


let mutrec_slot = SlotNumber.make (fun x -> "__mutrec_" ^ (string_of_int x))
let compiler_slot = SlotNumber.make (fun x -> "__compiler_sym_" ^ (string_of_int x))

let split_defs binds =
  let is_lambda = fun x -> match x with
    | Lambda _ -> true
    | _ -> false
  in
  List.partition (fun x -> is_lambda (snd x)) binds

let rec re_bind binds idx l = match binds with
  | x :: xs ->
      let s, _ = x in
      let t = Apply (Symbol (PrimitiveSym LISTREF), [ l; Num idx ]) in
      let xs = re_bind xs (idx + 1) l in
      (s, t) :: xs
  | [] -> []

let rec flat_defs ast = 
  let f b =
    let s, t = b in
    (s, flat_defs t)
  in
  match ast with
    | Define (b1, Define (b2, body)) ->
        let b = List.append b1 b2 in
        let def = Define (b, body) in
        flat_defs def
    | Define (bl, body) -> Define (List.map f bl, flat_defs body)
    | Let (bl, body) -> Let (List.map f bl, flat_defs body)
    | Lambda (a, b, body) -> Lambda (a, b, flat_defs body)
    | Apply (a, b) -> Apply (flat_defs a, List.map flat_defs b)
    | Branch (a, b, c) -> Branch (flat_defs a, flat_defs b, flat_defs c)
    | Statement l -> Statement (List.map flat_defs l)
    | _ -> ast

let rec remove_define ast symmap =
  let recf c = remove_define c symmap in
  match ast with
    | Define (binds, body) ->
        let lambdas, values = split_defs binds in
        let lsym = SlotNumber.fresh compiler_slot in
        let lambdas, rm_rec =
          let mutrec_list = SlotNumber.fresh_list mutrec_slot (List.length lambdas) in
          let rewrite_map =
            lambdas |> List.map (fun x -> fst x)
                    |> List.fold_left2 (fun x y z -> SMap.add z y x) symmap mutrec_list 
          in
          let rewrite_lambda lambda = match lambda with
            | Lambda (args, larg, body) ->
                let all = larg >:: args in
                let rewrite_map = 
                  List.fold_left (fun x y -> SMap.remove y x) rewrite_map all in
                let body = remove_define body rewrite_map in
                body |> fun x -> Lambda (args, larg, x)
                     |> fun x -> Lambda (mutrec_list, None, x)
            | _ -> raise (Invalid_argument "lambda must be Lambda")
          in
          let rm_rec =
            lambdas |> List.split
                    |> snd
                    |> List.map rewrite_lambda
                    |> fun x -> Apply (Symbol (CommonSym y_star), x)
          in
          (re_bind lambdas 0 (Symbol (CommonSym lsym)), rm_rec)
        in
        let values = List.map (fun x -> (fst x, remove_define (snd x) symmap)) values in
        let binds = List.append lambdas values in
        let body = remove_define body symmap in
        let res = Let (binds, body) in
        let res = Lambda ([ lsym ], None, res) in
        Apply (res, [ rm_rec ])
    | Symbol (CommonSym s) -> 
        Symbol (CommonSym (if SMap.mem s symmap then SMap.find s symmap else s))
    | Symbol s -> Symbol s
    | Let (bl, body) ->
        let bl =
          let a, b = List.split bl in
          let b = List.map recf b in
          List.combine a b
        in
        let body = recf body in
        Let (bl, body)
    | Apply (f, args) -> Apply (recf f, List.map recf args)
    | Lambda (a, b, body) -> Lambda (a, b, recf body)
    | Branch (a, b, c) -> Branch (recf a, recf b, recf c)
    | Statement l -> Statement (List.map recf l)
    | _ -> ast

let make_ast src =
  src |> make_let y_star y_star_body
      |> make_let map map_body
      |> make_let open_map open_map_body
      |> Lexing.from_string
      |> Parser.root Lexer.program

let normalize ast =
  ast |> flat_defs
      |> alpha_trans
      |> fun x -> remove_define x SMap.empty
