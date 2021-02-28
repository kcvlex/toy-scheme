open Cps

let gen_var_counter = ref 0

let hashtbl_sz = 100

let var2var = Hashtbl.create hashtbl_sz

let fresh_gen_var () =
  let num = !gen_var_counter in
  begin
    incr gen_var_counter;
    GeneratedSym num
  end

let initialize_entry varname =
  if Hashtbl.mem var2var varname then
    ()
  else
    Hashtbl.add var2var varname (Stack.create ())

let add_new_var sym = match sym with
  | UserSym s ->
      initialize_entry s;
      Stack.push (fresh_gen_var ()) (Hashtbl.find var2var s)
  | _ -> ()

let rm_var sym = match sym with
  | UserSym s -> 
      let _ = Stack.pop (Hashtbl.find var2var s) in
      ()
  | _ -> ()

let replace_user_var varname =
  let stk = Hashtbl.find var2var varname in
  Stack.top stk

let rec rename_cps cps = match cps with
  | Value (adm, v) -> Value (adm, rename_cps_value v)
  | Cont c -> Cont (rename_cps_cont c)
  | PassCont (t, c) -> PassCont (rename_cps t, rename_cps_cont c)
  | ApplyFunc (f, c, args) -> ApplyFunc (rename_cps_sym f, rename_cps_cont c, List.map rename_cps_sym args)
  | Bind (src, t, body) -> 
      add_new_var src;
      let dst = rename_cps_sym src in
      let t = rename_cps_sym t in
      let body = rename_cps body in
      begin
        rm_var src;
        Bind (dst, t, body)
      end
and rename_cps_sym sym = match sym with
  | UserSym s -> replace_user_var s
  | _ -> sym
and rename_cps_cont c = match c with
  | AdmLambda (adm, t) -> AdmLambda (adm, rename_cps t)
  | _ -> c
and rename_cps_value v = match v with
  | Sym s -> Sym (rename_cps_sym s)
  | Lambda (adm, srcs, t) ->
      List.iter add_new_var srcs;
      let dsts = List.map rename_cps_sym srcs in
      let t = rename_cps t in
      begin
        List.iter rm_var srcs;
        Lambda (adm, dsts, t)
      end
  | _ -> v 


let rename_vars cps = rename_cps cps
