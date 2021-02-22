open Cps

type t =
  | Num of int
  | Bool of bool
  | Symbol of string
  | Lambda of t * t
  | LambdaVar of t * t
  | Apply of t * t
  | Define of t * t
  | Cons of t * t

exception CPSError of string

let cont_id_counter = ref 0
let cont_param_counter = ref 0

let fresh_cont_id () =
  let num = !cont_id_counter in
  begin
    incr cont_id_counter; 
    CpsContSym num
  end

let fresh_cont_param () =
  let num = !cont_param_counter in
  begin
    incr cont_param_counter;
    CpsParamSym num
  end

let rec convert_ast ast = match ast with
  | Ast.Num n -> Num n
  | Ast.Bool b -> Bool b
  | Ast.Symbol s -> Symbol s
  | Ast.Lambda (args, body) -> 
      List.fold_right (fun x y -> Lambda ((convert_ast x), y)) args (convert_ast body)
  | Ast.LambdaVar (arg, body) -> LambdaVar ((convert_ast arg), (convert_ast body))
  | Ast.Apply (f, args) ->
      List.fold_left (fun x y -> Apply (x, (convert_ast y))) (convert_ast f) args
  | Ast.Define (sym, body) -> Define ((convert_ast sym), (convert_ast body))
  | Ast.Cons (car, cdr) -> Cons ((convert_ast car), (convert_ast cdr))

let sym2sym ast = match ast with
  | Symbol s -> UserSym s
  | _ -> raise (CPSError "sym2sym")

let rec dprog2cps ast =
  let k = fresh_cont_id () in
  let e = dexp2cps ast (Some (ContSym k)) in
  Program (ContFunc (k, e))
and dexp2cps ast cont = match (ast, cont) with
  | (Apply (e0, e1), Some c) -> 
      let v0 = fresh_cont_param () in
      let v1 = fresh_cont_param () in
      let body = PassCont (Triv (Sym v0), ContSym v1) in
      let body = PassCont (body, c) in
      let body = ContFunc (v1, body) in
      let body = dexp2cps e1 (Some body) in
      let body = ContFunc (v0, body) in
      dexp2cps e0 (Some body)
  | (Cons (_, _), _) -> dcons2cps ast cont
  | (_, Some c) -> CallCont (c, dtriv2cps ast)
  | _ -> dtriv2cps ast
and dtriv2cps ast = Triv(match ast with
  | Num n -> Int n
  | Bool b -> Bool b
  | Symbol s -> Sym (UserSym s)
  | Lambda (x, e) ->
      let x = sym2sym x in 
      let cps = dprog2cps e in
      (match cps with
        | Program (ContFunc (k, e)) -> Lambda (x, Cont (ContFunc (k, e)))
        | _ -> raise (CPSError "CPS transformation failed."))
  | _ -> raise (CPSError "Don't Implemented"))
and dcons2cps ast cont = match ast with
  | Cons (car, cdr) ->
    (* car must be `define` *)
    (match car with
      | Define (sym, def) ->
          let sym = sym2sym sym in
          let def = dprog2cps def in
          let id_fun () =
            begin
              let v = fresh_cont_param () in
              ContFunc (v, Triv (Sym v))
            end
          in
          let def = PassCont (def, id_fun ()) in
          let def = Def (sym, def) in
          Triv (Cons (Triv def, (dcons2cps cdr cont)))
      | _ -> raise (CPSError "car must be define."))
  | _ -> dexp2cps ast cont

let cps_transformation ast = dprog2cps (convert_ast ast)
