open Compiler
open Util
open TestUtil

let repeat c n =
  let rec aux i = if i = n then "" else c ^ (aux (i + 1)) in
  aux 0

let lfill c n s =
  let remain = 
    let len = String.length s in
    if len < n then n - len else 0
  in
  let prefix = repeat c remain in
  prefix ^ s

let make_rptbl ptbl =
  let tbl = Hashtbl.create (Hashtbl.length ptbl) in
  Hashtbl.iter (fun x y -> Hashtbl.add tbl y x) ptbl;
  tbl

let string_of_proc vec ptbl =
  let rptbl = make_rptbl ptbl in
  let str i = 
    let instr = i |> Vector.get vec |> Compiler.ThreeAddressCode.string_of_instr in
    let label =
      if Hashtbl.mem rptbl i then (Hashtbl.find rptbl i) ^ ":" else ""
    in
    let label = lfill " " 10 label in
    label ^ instr
  in
  let rec aux i = if i = Vector.length vec then [] else (str i) :: (aux (i + 1)) in
  aux 0

let () =
  let reg_num = (2, 2, 0) in
  let _, ptbl, vec = ThreeAddressCode.sample_program in
  let vec, ptbl = RegAlloc.allocate_experiment reg_num vec ptbl in
  let lis = string_of_proc vec ptbl in
  print_label "Register Allocation";
  print_endline (String.concat "\n" lis)
