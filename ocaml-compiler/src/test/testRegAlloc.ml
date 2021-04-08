open Compiler
open Util
open TestUtil

(*
 * --------------------------------
 * Example from Tiger book (p.238)
 * --------------------------------
 *
 * // f(a, b) returns a * b
 * int f(int a, int b) {
 *   int d = 0;
 *   int e = 0;
 *   do {
 *     d = d + b;
 *     e = e - 1;
 *   } while (e > 0);
 *   return d;
 * }
 *
 *
 * --------------------------------
 * Compiled (r1 and r2 are caller-saved, r3 is callee-saved)
 * --------------------------------
 *
 * enter: c <- r3
 *        a <- r1
 *        b <- r2
 *        d <- 0
 *        e <- a
 * loop:  d <- d + b
 *        e <- e - 1
 *        if e > 0 jump loop
 *        r1 <- d
 *        r3 <- c
 *        return
 *
 *
 *---------------------------------
 * Interference graph
 *---------------------------------
 *
 * r1 -> [ r2, r3 ]
 * r2 -> [ r1, r3, a, c ]
 * r3 -> [ r1, r2 ]
 * a  -> [ b, c, d ]
 * b  -> [ a, c, d, e ]
 * c  -> [ r2, a, b, c, d, e ]
 * d  -> [ a, b, c, e ]
 * e  -> [ b, c, d ]
 *
 *
 * --------------------------------
 * Move relate pairs
 * --------------------------------
 *
 * (r1, a), (r1, d), (r3, c), (a, e)
 *
 *)

let repeat c n =
  let rec aux i = if i = n then "" else c ^ (aux (i + 1)) in
  aux 0

let rfill c n s =
  let remain = 
    let len = String.length s in
    if len < n then n - len else 0
  in
  let suffix = repeat c remain in
  s ^ suffix

let make_rptbl ptbl =
  let tbl = Hashtbl.create (Hashtbl.length ptbl) in
  Hashtbl.iter (fun x y -> Hashtbl.add tbl y x) ptbl;
  tbl

let string_of_label label = 
  let label = match label with
    | Some l -> l ^ ":"
    | None -> ""
  in
  rfill " " 10 label

let string_of_proc seq =
  seq |> Vector.list_of_vector
      |> List.map (fun (x, y) -> (ThreeAddressCode.string_of_instr x, string_of_label y))
      |> List.map (fun (x, y) -> y ^ x)

let () =
  let reg_num = (1, 2, 0) in
  let sample = ThreeAddressCode.sample_program in
  let allocated = RegAlloc.allocate_experiment reg_num sample in
  let seq = allocated.seq in
  let lis = string_of_proc seq in
  print_label "Register Allocation";
  print_endline (String.concat "\n" lis)
