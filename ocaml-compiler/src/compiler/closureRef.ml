type t = rt option
and rt = (SymbolType.closure_sym * tbl_t) * (SymbolType.record_sym * tbl_t) * tbl_t
and tbl_t = (SymbolType.t, int) Hashtbl.t

let n = 8

let empty = None

let rec make_aux symlist idx tbl = match symlist with
  | x :: xs -> Hashtbl.add tbl x idx; make_aux xs (idx + 1) tbl
  | [] -> tbl

let make ~passed ~created ~defined =
  let psym, plis = passed in
  let csym, clis = created in
  let tbl1 = make_aux plis 0 (Hashtbl.create n) in
  let tbl1 = (psym, tbl1) in
  let tbl2 = make_aux clis 0 (Hashtbl.create n) in
  let tbl2 = (csym, tbl2) in
  let tbl3 = make_aux defined 0 (Hashtbl.create n) in
  Some (tbl1, tbl2, tbl3)

let reftype tv sym = match tv with
  | Some (stbl1, stbl2, tbl3) ->
      let s1, tbl1 = stbl1 in
      let s2, tbl2 = stbl2 in
      if Hashtbl.mem tbl3 sym then
        RefType.LocalBind (sym, s2, Hashtbl.find tbl2 sym)
      else if Hashtbl.mem tbl1 sym then
        RefType.PassedClosure (s1, Hashtbl.find tbl1 sym)
      else if Hashtbl.mem tbl2 sym then
        RefType.PassingRecord (sym, s2, Hashtbl.find tbl2 sym)
      else
        RefType.Direct sym
  | None -> RefType.Direct sym
