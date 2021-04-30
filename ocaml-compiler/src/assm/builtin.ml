open Rv32i
open RiscvAssm
open Compiler

exception Unimpled

(* FIXME *)
let wsize = 8
let wsize_log = 3

let actual i = Instr (Actual i)

let pseudo i = Instr (Pseudo i)
  
let func_of_prim (p: SymbolType.primitive_sym) = match p with
  | ADD     -> "__add"
  | SUB     -> "__sub"
  | EQ      -> "__eq"
  | LESS    -> "__less"
  | NULL    -> "__null"
  | CONS    -> "__cons"
  | CAR     -> "__car"
  | CDR     -> "__cdr"
  | LISTREF -> "__list_ref"
  | APPLY   -> "__apply"
  | DISPLAY -> "__display"
  | _ -> raise Unimpled

let closure_of_prim p = Printf.sprintf "__%s_closure" (func_of_prim p)

let closure_func_of_prim p = (closure_of_prim p) ^ "__f"

let iota n =
  let rec aux i = if i = n then [] else i :: (aux (i + 1)) in
  aux 0
  
let upper = 8  (* FIXME *)

let add =
  let l = func_of_prim ADD in
  let c = closure_of_prim ADD in
  let cl = closure_func_of_prim ADD in
  [ Ops (Section Rodata);
    Label c;
    Ops (Word (Symbol cl));
    Ops (Word (Num Int32.zero));
    Ops (Section Text);
    Label l;
    actual (ADD { dst = Arg 0; lhs = Arg 0; rhs = Arg 1; });
    pseudo RET;
    Label cl;
    actual (ADD { dst = Arg 0; lhs = Arg 1; rhs = Arg 2; });
    pseudo RET ]

let sub =
  let l = func_of_prim SUB in
  let c = closure_of_prim SUB in
  let cl = closure_func_of_prim SUB in
  [ Ops (Section Rodata);
    Label c;
    Ops (Word (Symbol cl));
    Ops (Word (Num Int32.zero));
    Ops (Section Text);
    Label l;
    actual (SUB { dst = Arg 0; lhs = Arg 0; rhs = Arg 1 });
    pseudo RET;
    Label cl;
    actual (SUB { dst = Arg 0; lhs = Arg 1; rhs = Arg 2 });
    pseudo RET ]

let eq =
  let l = func_of_prim EQ in
  let c = closure_of_prim EQ in
  let cl = closure_func_of_prim EQ in
  [ Ops (Section Rodata);
    Label c;
    Ops (Word (Symbol cl));
    Ops (Word (Num (Int32.zero)));
    Ops (Section Text);
    Label l;
    actual (XOR { dst = Arg 0; lhs = Arg 0; rhs = Arg 1 });
    pseudo (SEQZ { rd = Arg 0; rs = Arg 0 });
    pseudo RET;
    Label cl;
    actual (XOR { dst = Arg 0; lhs = Arg 1; rhs = Arg 2 });
    pseudo (SEQZ { rd = Arg 0; rs = Arg 0 });
    pseudo RET ]

let less =
  let l = func_of_prim LESS in
  let c = closure_of_prim LESS in
  let cl = closure_func_of_prim LESS in
  [ Ops (Section Rodata);
    Label c;
    Ops (Word (Symbol cl));
    Ops (Word (Num Int32.zero));
    Ops (Section Text);
    Label l;
    actual (SUB { dst = Arg 0; lhs = Arg 0; rhs = Arg 1 });
    pseudo (SLTZ { rd = Arg 0; rs = Arg 0 });
    pseudo RET;
    Label cl;
    actual (SUB { dst = Arg 0; lhs = Arg 1; rhs = Arg 2 });
    pseudo (SLTZ { rd = Arg 0; rs = Arg 0 });
    pseudo RET ]

let null = 
  let l = func_of_prim NULL in
  let c = closure_of_prim NULL in
  let cl = closure_func_of_prim NULL in
  [ Ops (Section Rodata);
    Label c;
    Ops (Word (Symbol cl));
    Ops (Word (Num Int32.zero));
    Ops (Section Text);
    Label l;
    pseudo (SEQZ { rd = Arg 0; rs = Arg 0 });
    pseudo RET;
    Label cl;
    pseudo (SEQZ { rd = Arg 0; rs = Arg 1 });
    pseudo RET ]

let cons =
  let l = func_of_prim CONS in
  let c = closure_of_prim CONS in
  let cl = closure_func_of_prim CONS in
  [ Ops (Section Rodata);
    Label c;
    Ops (Word (Symbol cl));
    Ops (Word (Num Int32.zero));
    Ops (Section Text);
    Label l;
    pseudo (MV { rd = Arg 2; rs = Arg 1 });
    pseudo (MV { rd = Arg 1; rs = Arg 0 });
    pseudo (LI { rd = Arg 0; imm = 2 });
    pseudo (TAIL { offset = "allocate" });  (* car, cdr *)
    Label cl;
    pseudo (LI { rd = Arg 0; imm = 2 });
    pseudo (TAIL { offset = "allocate" }) ]

let car =
  let l = func_of_prim CAR in
  let c = closure_of_prim CAR in
  let cl = closure_func_of_prim CAR in
  [ Ops (Section Rodata);
    Label c;
    Ops (Word (Symbol cl));
    Ops (Word (Num Int32.zero));
    Ops (Section Text);
    Label l;
    actual (LW { dst = Arg 0; base = Arg 0; offset = Int 0 });
    pseudo RET;
    Label cl;
    actual (LW { dst = Arg 0; base = Arg 1; offset = Int 0 });
    pseudo RET ]

let cdr =
  let l = func_of_prim CDR in
  let c = closure_of_prim CDR in
  let cl = closure_func_of_prim CDR in
  [ Ops (Section Rodata);
    Label c;
    Ops (Word (Symbol cl));
    Ops (Word (Num Int32.zero));
    Ops (Section Text);
    Label l;
    actual (LW { dst = Arg 0; base = Arg 0; offset = Int wsize });
    pseudo RET;
    Label cl;
    actual (LW { dst = Arg 0; base = Arg 1; offset = Int wsize });
    pseudo RET ]

let list_ref = 
  let l = func_of_prim LISTREF in
  let c = closure_of_prim LISTREF in
  let cl = closure_func_of_prim LISTREF in
  let label = "__list_ref_loop" in
  [ Ops (Section Rodata);
    Label c;
    Ops (Word (Symbol cl));
    Ops (Word (Num Int32.zero));
    Ops (Section Text);
    Label l;
    pseudo (BGTZ { rs = Arg 1; offset = label });
    actual (LW { dst = Arg 0; base = Arg 0; offset = Int 0 });
    pseudo RET;
    Label label;
    actual (LW { dst = Arg 0; base = Arg 0; offset = Int wsize });
    actual (ADDI { dst = Arg 1; lhs = Arg 1; rhs = Int (-1) });
    pseudo (J { offset = l });
    Label cl;
    pseudo (MV { rd = Arg 0; rs = Arg 1 });
    pseudo (MV { rd = Arg 1; rs = Arg 2 });
    pseudo (TAIL { offset = l }) ]

(*
 * FIXME : when args = nil
 *)
let apply =
  let l = func_of_prim APPLY in
  let c = closure_of_prim APPLY in
  let cl = closure_func_of_prim APPLY in
  let apply_fin = "__apply_fin" in
  let entry = [
    Ops (Section Rodata);
    Label c;
    Ops (Word (Symbol cl));
    Ops (Word (Num Int32.zero));
    Ops (Section Text);
    Label l;
    actual (LW { dst = Tmp 0; base = Arg 0; offset = Int 0 });
    actual (LW { dst = Arg 0; base = Arg 0; offset = Int wsize });
    pseudo (MV { rd = Tmp 1; rs = Arg 1; });
  ]
  in
  let assign_arg i = [
    actual (LW { dst = Arg (i + 1); base = Tmp 1; offset = Int 0 });  (* car *)
    actual (LW { dst = Tmp 1; base = Tmp 1; offset = Int wsize });
    pseudo (BEQZ { rs = Tmp 1; offset = apply_fin });
    actual (LW { dst = Tmp 1; base = Tmp 1; offset = Int 0 });
  ]
  in
  let fin = [
    Label apply_fin;
    pseudo (JR { rs = Tmp 0 })
  ]
  in
  let ulis = iota (upper - 1) in
  List.flatten [
    entry;
    List.flatten (List.map assign_arg ulis);
    fin;
    [ Label cl;
      pseudo (MV { rd = Arg 0; rs = Arg 1 });
      pseudo (MV { rd = Arg 1; rs = Arg 2 });
      pseudo (TAIL { offset = l }) ]
  ]

let display =
  let l = func_of_prim DISPLAY in
  let c = closure_of_prim DISPLAY in
  let cl = closure_func_of_prim DISPLAY in
  let format_s = "__display_format" in
  [ Ops (Section Rodata);
    Label format_s;
    Ops (String "%d\\n");
    Label c;
    Ops (Word (Symbol cl));
    Ops (Word (Num Int32.zero));
    Ops (Section Text);
    Label l;
    pseudo (MV { rd = Arg 1; rs = Arg 0 });
    pseudo (LA { rd = Arg 0; symbol = format_s });
    pseudo (TAIL { offset = "printf" });
    Label cl;
    pseudo (MV { rd = Arg 0; rs = Arg 1 });
    pseudo (TAIL { offset = l }) ]

let allocate =
  let free_list = "__free_list" in
  let br = "__branch_allocate" in
  let entry = [
    Ops (Comm (free_list, wsize));
    Ops (Section Text);
    Label "allocate";
    pseudo (BNEZ { rs = Arg 0; offset = br });
    pseudo RET;
    Label br;
    actual (SLLI { dst = Tmp 0; lhs = Arg 0; rhs = Int wsize_log });
    pseudo (LG { rd = Arg 0; symbol = free_list });
    actual (ADD { dst = Tmp 1; lhs = Arg 0; rhs = Tmp 0 });
    pseudo (SG { rd = Tmp 1; rt = Tmp 2; symbol = free_list });
  ]
  in
  (* FIXME *)
  let assign i = [
    actual (SW { src = Arg (i + 1); base = Arg 0; offset = Int (i * wsize) })
  ]
  in
  List.flatten [
    entry;
    List.flatten (List.map assign (iota (upper - 1)));
    [ pseudo RET ]
  ]


let lib = List.flatten [
  [ Ops (Align 2) ];
  add;
  sub;
  eq;
  less;
  null;
  cons;
  car;
  cdr;
  list_ref;
  apply;
  display;
  allocate
]
