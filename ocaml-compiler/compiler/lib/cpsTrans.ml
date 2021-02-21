open Cps

exception CPSError of string

let cont_id_counter = ref 0
let cont_param_counter = ref 0

let fresh_cont_id () =
  let num = !cont_id_counter in
  begin
    incr cont_id_counter; 
    "k" ^ (string_of_int num)
  end

let fresh_cont_param () =
  let num = !cont_param_counter in
  begin
    incr cont_param_counter;
    "v" ^ (string_of_int num);
  end

let rec gen_fresh_params n =
  if n = 0 then begin
    []
  end else begin
    let x = fresh_cont_param() in
    let xs = gen_fresh_params (n - 1) in
    x :: xs
  end

let rec dprog_to_cps ast =
  let k = fresh_cont_id () in
  let e = dexp_to_cps ast (Some (ContSym k)) in
  Program(ContFunc(k, e))
and dexp_to_cps ast cont =
  match (ast, cont) with
  | (Ast.Apply(f, args), Some c) -> 
      begin
        let vs = gen_fresh_params((List.length args) + 1) in
        let vs_sym = List.map (fun x -> Triv(Sym(x))) vs in
        let body = PassCont((List.hd vs_sym), (List.tl vs_sym), c) in
        List.fold_right2 (fun e v expr -> dexp_to_cps e (Some(ContFunc(v, expr)))) (f :: args) vs body
      end
  | (_, Some c) -> CallCont(c, dtriv_to_cps ast)
  | _ -> dtriv_to_cps ast
and dtriv_to_cps ast = Triv(
  match ast with
  | Num n -> Int n
  | Bool b -> Bool b
  | Symbol s -> Sym s
  | Lambda(args, proc) -> (match (dprog_to_cps proc) with
    | Program(ContFunc(k, e)) -> Lambda(args, k, e)
    | _ -> raise (CPSError "CPS transformation failed."))
  | _ -> raise (CPSError "Don't Implemented")
)

let cps_transformation ast = dprog_to_cps ast
