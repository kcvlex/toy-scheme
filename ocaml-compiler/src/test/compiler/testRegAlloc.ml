open Compiler
open Util
open Testtool

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

let print_machine machine = 
  machine |> AbstractMachine.string_of_machine 
          |> print_endline

let () =
  RegAlloc.set_logging true;
  let reg_num = (0, 1, 2) in
  let allocated = RegAlloc.allocate_experiment reg_num ThreeAddressCode.sample2 in
  let () =
    let seq = allocated.seq in
    let lis = string_of_proc seq in
    TestUtil.print_label "Register Allocation";
    print_endline (String.concat "\n" lis)
  in
  let make_int i = AbstractMachineType.Int i in
  let entry = AbstractMachine.call_and_print "entry" "f" [ make_int 42; make_int 42 ] in
  let machine =
    allocated
    |> ThreeAddressCode.to_abs_program
    |> AbstractMachine.link entry
    |> AbstractMachine.make_machine
  in
  print_machine machine;
  AbstractMachine.eval machine
