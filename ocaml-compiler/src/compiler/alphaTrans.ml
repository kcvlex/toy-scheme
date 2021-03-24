open AstType

let user_sym_counter = ref 0

let fresh_prefix () =
  let res = !user_sym_counter in
  let res = "__user" ^ (string_of_int res) ^ "_" in
  incr user_sym_counter; res

let rename s = (fresh_prefix ()) ^ s

module SymMap = Map.Make(String)

let get_sym b = match b with
  | Bind { sym; def = _ } -> sym

let add_defs map src dst =
  List.fold_left2 (fun x y z -> SymMap.add y z x) map src dst

let is_primitive s = match s with
  | "+" | "-" | "<" | "eq?" -> true
  | _ -> false

let rec trans_aux ast symmap =
  let recf ast = trans_aux ast symmap in
  match ast with
    | Symbol s ->
        if is_primitive s then 
          ast 
        else
          let s = SymMap.find s symmap in
          Symbol s
    | Lambda (args, body) ->
        let renames = List.map rename args in
        let symmap = add_defs symmap args renames in
        let body = trans_aux body symmap in
        Lambda (renames, body)
    | Apply (f, args) -> Apply (recf f, List.map recf args)
    | Define (binds, body) ->
        let bsyms = List.map get_sym binds in
        let renames = List.map rename bsyms in
        let symmap = add_defs symmap bsyms renames in
        let fb b = match b with
          | Bind { sym; def } -> Bind { sym = SymMap.find sym symmap; def = trans_aux def symmap }
        in
        let binds = List.map fb binds in
        let body = trans_aux body symmap in
        Define (binds, body)
    | Branch (a, b, c) -> Branch (recf a, recf b, recf c)
    | Statement lis -> Statement (List.map recf lis)
    | _ -> ast

let trans ast = trans_aux ast SymMap.empty
