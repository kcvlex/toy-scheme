open ClosureType
open SymbolType
open Util

type fv_list = fv_ele list
and fv_ele =
  | FVar of string
  | FParentList of fv_list

module SS = Set.Make(String)

let clsr_slot = SlotNumber.make (fun x -> "__clsr_" ^ (string_of_int x))

let rec find_fv_aux fname flis var idx seed = match flis with
  | (FVar s) :: xs ->
      if s = var then
        ClosureRef (seed, idx) 
      else 
        find_fv_aux fname xs var (idx + 1) seed
  | (FParentList l) :: [] ->
      let seed = ClosureRef (seed, idx) in
      find_fv_aux fname l var 0 seed
  | _ -> raise (Invalid_argument ("Not found " ^ var))

let find_fv fname flis var = find_fv_aux fname flis var 0 (Var fname)

let (>::) s sl = match s with
  | Some t -> t :: sl
  | None -> sl

let all_fv tbl = Hashtbl.fold (fun _ v s -> SS.union v s) tbl SS.empty

let rec collect_fv anf name defined tbl =
  let recf a = collect_fv a name defined tbl in
  let recf_t a = collect_fv_term a name defined tbl in
  match anf with
    | AnfType.Term t -> collect_fv_term t name defined tbl
    | AnfType.Bind (sl, body) ->
        let aux1 x defined = match x with
          | AnfType.BindValue (s, v) -> 
              let name = match v with
                | AnfType.Lambda _ -> s
                | _ -> name
              in
              collect_fv_term v name defined tbl;
              SS.add s defined
          | AnfType.BindCall (s, f, args) ->
              List.iter (fun v -> collect_fv_term v name defined tbl) (f :: args);
              SS.add s defined
        in
        let rec aux2 lis defined = match lis with
          | x :: xs ->
              let defined = aux1 x defined in
              aux2 xs defined
          | [] -> defined
        in
        let defined = aux2 sl defined in
        collect_fv body name defined tbl
    | AnfType.Branch (p, t1, t2) ->
        recf_t p;
        recf t1;
        recf t2
    | AnfType.Call (f, args) ->
        recf_t f;
        List.iter recf_t args
and collect_fv_term term name defined tbl = match term with
  | AnfType.Lambda (sl, s, body) ->
      let defined = List.fold_left (fun x y -> SS.add y x) SS.empty (s >:: sl) in
      Hashtbl.replace tbl name SS.empty;
      collect_fv body name defined tbl
  | AnfType.Ref s ->
      if SS.mem s defined then
        ()
      else if not (Hashtbl.mem tbl name) then
        ()
      else
        let set = Hashtbl.find tbl name in
        let set = SS.add s set in
        Hashtbl.replace tbl name set
  | _ -> ()

let collect_fv anf = 
  let tbl = Hashtbl.create 8 in
  collect_fv anf "DUMMY" SS.empty tbl;
  tbl

let convert_term term = match term with
  | AnfType.Int i -> Int i
  | AnfType.Bool b -> Bool b
  | AnfType.Primitive p -> Primitive p
  | AnfType.Nil -> Nil
  | AnfType.Quote a -> Quote a
  | _ -> raise (Invalid_argument "Unexpected term")

let procs = ref []
let allfv = ref SS.empty

let modify_term t cname clis binds = match t with
  | AnfType.Ref s ->
      if SS.mem s binds then
        Var s
      else
        find_fv cname clis s
  | _ -> convert_term t

let rec closure_trans_aux (anf: AnfType.t) 
                          (cname: string) 
                          (clis: fv_list) 
                          (binds: SS.t)
                          (fvtbl: (string, SS.t) Hashtbl.t) =
  let recf a = closure_trans_aux a cname clis binds fvtbl in
  match anf with
    | AnfType.Term (AnfType.Lambda _) -> raise (Invalid_argument "Unnamed function")
    | AnfType.Term t -> Term (modify_term t cname clis binds)
    | AnfType.Bind (slis, body) ->
        let fn s t binds = match t with
          | AnfType.Lambda (al, a, body) ->
              let lbinds = List.fold_left (fun x y -> SS.add y x) SS.empty (a >:: al) in
              let lis = !allfv |> SS.filter (fun x -> SS.mem x binds) |> SS.elements in
              let pass_clis = List.fold_left (fun x y -> (FVar y) :: x) [ FParentList clis ] lis in
              let pass_cn = SlotNumber.fresh clsr_slot in
              let body = closure_trans_aux body pass_cn pass_clis lbinds fvtbl in
              let lambda = (pass_cn, al, a, body) in
              let gs = "__glob_" ^ s in
              procs := (gs, lambda) :: !procs;
              let parent_clis = if cname == "DUMMY" then [] else [ Var cname ] in
              let make_cl = 
                List.fold_left (fun x y -> 
                  (modify_term (AnfType.Ref y) cname clis binds) :: x) parent_clis lis 
              in
              Closure (gs, make_cl)
          | _ -> modify_term t cname clis binds
        in
        let aux1 x binds = match x with
          | AnfType.BindValue (s, t) ->
              let t = fn s t binds in
              let binds = SS.add s binds in
              ((s, t), binds)
          | AnfType.BindCall (s, f, args) ->
              let f = fn s f binds in
              let args = List.map (fun t -> fn s t binds) args in
              let call = Call (f, args) in
              let binds = SS.add s binds in
              ((s, call), binds)
        in
        let rec aux2 lis binds = match lis with
          | x :: xs ->
              let x, binds = aux1 x binds in
              let xs, binds = aux2 xs binds in
              (x :: xs, binds)
          | [] -> ([], binds)
        in
        let slis, binds = aux2 slis binds in
        let body = closure_trans_aux body cname clis binds fvtbl in
        Bind (slis, body)
    | AnfType.Branch (p, t1, t2) ->
        let p = modify_term p cname clis binds in
        let t1 = recf t1 in
        let t2 = recf t2 in
        Branch (p, t1, t2)
    | AnfType.Call (f, args) ->
        let f = modify_term f cname clis binds in
        let args = List.map (fun x -> modify_term x cname clis binds) args in
        let call = Call (f, args) in
        Term call

let closure_trans anf =
  procs := [];
  let fvtbl = collect_fv anf in
  allfv := all_fv fvtbl;
  let body = closure_trans_aux anf "DUMMY" [] SS.empty fvtbl in
  { procs = List.rev !procs; body = body }

let rec ast_of_clo_expr expr = match expr with
  | Term t -> ast_of_clo_term t
  | Bind (slis, body) ->
      let slis =
        let a, b = List.split slis in
        let b = List.map ast_of_clo_term b in
        List.combine a b
      in
      let body = ast_of_clo_expr body in
      AstType.Let (slis, body)
  | Branch (a, b, c) ->
      let a = ast_of_clo_term a in
      let b = ast_of_clo_expr b in
      let c = ast_of_clo_expr c in
      AstType.Branch (a, b, c)
and ast_of_clo_term term = match term with
  | Int i -> AstType.Num i
  | Bool b -> AstType.Bool b
  | Closure (s, slis) ->
      let s = AstType.Symbol (CommonSym s) in
      let slis = List.map ast_of_clo_term slis in
      let slis = AstType.Apply (AstType.Symbol (PrimitiveSym LIST), slis) in
      AstType.Apply (AstType.Symbol (PrimitiveSym CONS), [ s; slis ])
  | Call (f, args) ->
      let f = ast_of_clo_term f in
      let args = List.map ast_of_clo_term args in
      AstType.Apply (AstType.Symbol (CommonSym "__call"), f :: args);
  | Primitive APPLY -> AstType.Symbol (CommonSym "apply-clo")
  | Primitive p -> AstType.Symbol (PrimitiveSym p)
  | Var "DUMMY" -> AstType.Apply (AstType.Symbol (PrimitiveSym LIST), [ AstType.Num 0 ])
  | Var s -> AstType.Symbol (CommonSym s)
  | Nil -> AstType.Nil
  | ClosureRef (c, i) -> 
      AstType.Apply (AstType.Symbol (PrimitiveSym LISTREF), [ ast_of_clo_term c; AstType.Num i ])
  | Quote t -> AstType.Quote t
  | _ -> raise (Invalid_argument "UNIMPLED")

let ast_of_clo clo =
  let { procs; body } = clo in
  let ast_of_proc p = match p with
    | (c, al, a, body) -> AstType.Lambda (c :: al, a, ast_of_clo_expr body)
  in
  let procs =
    let pname, pbody = List.split procs in
    let pbody = List.map ast_of_proc pbody in
    List.combine pname pbody
  in
  let body = ast_of_clo_expr body in
  AstType.Let (procs, body)
