open AstType

module SymMap = Map.Make(String)
  
let rec code_of_ast ast =
  let make_lambda args body = "(lambda " ^ args ^ body ^ ")" in
  let sur_paren s = "(" ^ s ^ ")" in
  match ast with
    | Num i -> string_of_int i
    | Bool true -> "#t"
    | Bool false -> "#f"
    | Symbol s -> s
    | Primitive s -> s
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
          let s t = b in
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
          let s t = b in
          let t = code_of_ast in
          sur_paren (s ^ " " ^ t)
        in
        let bl = List.map f bl in
        let body = code_of_ast body in
        "(let " ^ bl ^ body ^ ")"
    | Branch (a, b, c) ->
        let a = code_of_ast a in
        let b = code_of_ast b in
        let c = code_of_ast c in
        Print.printf "(if %s %s %s)" a b c
    | Statement l ->
        let s = 
          l |> List.map code_of_ast
            |> String.concat " "
        in
        "(begin " ^ s ^ ")"
    | MakeBox t ->
        let t = code_of_ast t in
        "(list " ^ t ^ ")"
    | RefBox s -> "(list-ref " ^ s ^ " 0)"

let rename =
  let sl = SlotNumber.make (fun x -> "__user" ^ (string_of_int x) ^ "_") in
  fun s -> (SlotNumber.fresh sl) ^ s

let add_defs map src dst =
  List.fold_left2 (fun x y z -> SymMap.add y z x) map src dst

let (>::) x xs = match x with
  | Some s -> s :: xs
  | None -> xs

let rec alpha_trans ast symmap =
  let recf ast = alpha_trans ast symmap in
  let update_map binds map = 
    let bsyms = List.map (fun b -> b.sym) binds in
    let renames = List.map rename bsyms in
    add_defs map bsyms renames
  in
  let trans_binds binds map =
    let fb b = { sym = SymMap.find b.sym map; def = alpha_trans def map } in
    List.map fb binds
  in
  match ast with
    | Symbol s -> Symbol (SymMap.find s symmap)
    | Lambda (args, larg, body) ->
        let all = largs >:: args in
        let renames = List.map rename all in
        let symmap = add_defs symmap all renames in
        let body = alpha_trans body symmap in
        Lambda (renames, body)
    | Apply (f, args) -> Apply (recf f, List.map recf args)
    | Define (binds, body) ->
        let symmap = update_map binds symmap in
        let binds = trans_binds binds symmap in
        let body = alpha_trans body symmap in
        Define (binds, body)
    | Let (binds, body) ->
        let symmap = update_map binds symmap in
        let binds = trans_binds binds symmap in
        let body = alpha_trans body symmap in
        Let (binds, body)
    | Branch (a, b, c) -> Branch (recf a, recf b, recf c)
    | Statement lis -> Statement (List.map recf lis)
    | _ -> ast


let mutrec_slot = SlotNumber.make (fun x -> "__mtx_rec_" ^ (string_of_int x))
let compiler_slot = SlotNumber.make (fun x -> "__compiler_sym_" ^ (stirng_of_int x))

let split_defs binds =
  let is_lambda = fun x -> match x with
    | Lambda _ -> true
    | _ -> false
  in
  let lambdas = List.filter (fun x -> is_lambda x.def) binds in
  let values = List.fileter (fun x -> not (is_lambda x.def)) binds in
  lambdas. values

let y_star = "Y-star"
let y_star_body =
  let src = 
    "(lambda l \
       ((lambda (u) (u u)) \
        (lambda (p) \
          (map (lambda (li) (lambda x (apply (apply li (p p)) x))) l))))"
  in
  let lexbuf = Lexing.from_string src in
  Parser.root Lexer.program lexbuf

let re_bind binds idx l = match binds with
  | x :: xs ->
      let x = (x.sym, Apply (Symbol "list-ref", [ l, Num idx ])) in
      let xs = re_bind xs (idx + 1) l in
      x :: xs
  | [] -> []

let rec flat_defs ast = 
  let f b =
    let s t = b in
    (s, flat_defs t)
  in
  match ast with
    | Define (b1, Define (b2, body)) ->
        let b = List.append b1 b2 in
        let def = Define (b, body) in
        flat_defs def
    | Define (b, body) -> Define (List.map f bl, flat_defs body)
    | Let (bl, body) -> Let (List.map f bl, flat_defs body)
    | Lambda (a, b, body) -> Lambda (a, b, flat_defs body)
    | Apply (a, b, c) -> Apply (flat_defs a, flat_defs b, flat_defs c)
    | Branch (a, b, c) -> Branch (flag_defs a, flat_defs b, flat_defs c)
    | Statement l -> Statement (List.map flat_defs l)
    | _ -> ast

let rec remove_define ast symmap =
  let recf c = remove_define c symmap in
  match ast with
    | Define (binds, body) ->
        let lambdas, values = defined_lambda binds in
        let lsym = fresh_compiler_sym () in
        let lambdas rm_rec =
          let mutrec_list = fresh_mutrec_symlis (List.length lambdas) in
          let rewrite_map = 
            List.fold_left2 (fun x y z -> SymMap.add y z x) symmap lambdas mutrec_list in
          let rewrite_lambda lambda = match lambda with
            | Lambda (args, larg, body) ->
                let all = larg >:: args in
                let rewrite_map = 
                  List.fold_left (fun x y -> Symmap.remove y x) rewrite_map all in
                let body = remove_define body rewrite_map in
                let lambda = Lambda (args, larg, body) in
                let lambda = Lambda (mutrec_list, None, lambda) in
                lambda
                in
          let rm_rec =
            lambdas |> List.map (fun x -> x.def)
                    |> List.map rewrite_lambda
                    |> fun x -> Apply (Symbol y_star, x)
          in
          (re_bind lambdas 0 lsym, rm_rec)
          in
        let values = List.map (fun x -> { sym = x.sym; def = remove_define x.def symmap }) values in
        let binds = List.append lambdas values in
        let body = remove_define body symmap in
        let res = Let (binds, body) in
        let res = Lambda ([ lsym ], None, res) in
        Apply (res, [ rm_rec ])
    | Symbol s -> Symbol (if SymMap.mem s symmap then SymMap.find s symmap else s)
    | Apply (f, args) -> Apply (recf f, List.map recf args)
    | Lambda (a, b, body) -> Lambda (a, b, recf body)
    | Branch (a, b, c) -> Branch (recf a, recf b, recf c)
    | Statement l -> Statement (List.map recf l)
    | _ -> ast


let collect_assigned ast =
  let tbl = Hashtbl.create 8 in
  let rec fn ast = match ast with
    | Let (bl, body) ->
        let fr b =
          let s t = b in
          collect_assigned t
        in
        List.iter fr bl; fn body
    | Apply (f, args) -> begin
        (match f with
          | Primitive "set!" ->
              let hd = List.hd args in
              (match hd with
                | Symbol s -> Hashtbl.add tbl s
                | _ -> raise (Invalid_argument "hd must be symbol"))
          | _ -> ());
        fn f;
        List.iter fn args
      end
    | Lambda (a, b, body) -> fn body
    | Branch (a, b, c) -> fn a; fn b; fn c
    | Statement l -> List.iter fn l
    | _ -> ()
  in
  fn ast; tbl

let boxing ast =
  let tbl = collect_assigned ast in
  let rec fn ast = match ast with
    | Let (bl, body) ->
        let fr b =
          let s t = b in
          let t = fn t in
          if Hashtbl.mem tbl s then (s, MakeBox t) else (s, t)
        in
        Let (List.map fr bl, fn body)
    | Apply (f, args) -> Apply (fn f, List.map fn args)
    | Lambda (a, b, body) -> Lambda (a, b, fn body)
    | Branch (a, b, c) -> Branch (fn a, fn b, fn c)
    | Statement l -> Statement (List.map fn l)
    | Symbol s -> if Hashtbl.mem tbl s then RefBox s else Symbol s
    | _ -> ast
  in
  fn ast

let normalize ast =
  ast |> alpha_trans SymMap.empty
      |> flat_defs
      |> remove_define SymMap.empty
      |> boxing
      |> fun x -> Let ([ (y_star, y_star_body) ], x)
