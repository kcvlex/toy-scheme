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

let iota n =
  let rec aux i = if i = n then [] else i :: (aux (i + 1)) in
  aux 0
  
let upper = 8  (* FIXME *)

let add = [
  Ops (Section Text);
  Ops (Globl (Symbol "__add"));
  Label "__add";
  actual (ADD { dst = Arg 0; lhs = Arg 1; rhs = Arg 2; });
  pseudo RET
]

let sub = [
  Ops (Section Text);
  Ops (Globl (Symbol "__sub"));
  Label "__sub";
  actual (SUB { dst = Arg 0; lhs = Arg 1; rhs = Arg 2 });
  pseudo RET
]

let eq = [
  Ops (Section Text);
  Ops (Globl (Symbol "__eq"));
  Label "__eq";
  actual (XOR { dst = Arg 0; lhs = Arg 1; rhs = Arg 2 });
  pseudo (SEQZ { rd = Arg 0; rs = Arg 0 });
  pseudo RET
]

let less = [
  Ops (Section Text);
  Ops (Globl (Symbol "__less"));
  Label "__less";
  actual (SUB { dst = Arg 0; lhs = Arg 1; rhs = Arg 2 });
  pseudo (SLTZ { rd = Arg 0; rs = Arg 0 });
  pseudo RET
]

let null = [
  Ops (Section Text);
  Ops (Globl (Symbol "__null"));
  Label "__null";
  pseudo (SEQZ { rd = Arg 0; rs = Arg 1 });
  pseudo RET
]

let cons = [
  Ops (Section Text);
  Ops (Globl (Symbol "__cons"));
  Label "__cons";
  pseudo (LI { rd = Arg 0; imm = 2 });
  pseudo (TAIL { offset = "allocate" });  (* car, cdr *)
]

let car = [
  Ops (Section Text);
  Ops (Globl (Symbol "__car"));
  Label "__car";
  actual (LW { dst = Arg 0; base = Arg 1; offset = Int 0 });
  pseudo RET
]

let cdr = [
  Ops (Section Text);
  Ops (Globl (Symbol "__cdr"));
  Label "__cdr";
  actual (LW { dst = Arg 0; base = Arg 1; offset = Int wsize });
  pseudo RET
]

let list_ref = 
  let label = "__list_ref_loop" in
  [
    Ops (Section Text);
    Ops (Globl (Symbol "__list_ref"));
    Label "__list_ref";
    pseudo (BGTZ { rs = Arg 2; offset = label });
    actual (LW { dst = Arg 0; base = Arg 1; offset = Int 0 });
    pseudo RET;
    Label label;
    actual (LW { dst = Arg 1; base = Arg 1; offset = Int wsize });
    actual (ADDI { dst = Arg 2; lhs = Arg 2; rhs = Int (-1) });
    pseudo (J { offset = "__list_ref" })
  ]

(*
 * t0 : f
 * t1 : address of args
 * t2 : counter
 *)
let apply =
  let loop_entry = "__apply_loop_entry" in
  let loop_exec = "__apply_loop_exec" in
  let entry = [
    Ops (Section Text);
    Ops (Globl (Symbol "__apply"));
    Label "__apply";
    actual (LW { dst = Tmp 0; base = Arg 1; offset = Int 0 });
    actual (LW { dst = Arg 0; base = Arg 1; offset = Int wsize });
    pseudo (MV { rd = Tmp 1; rs = Arg 2; });
    pseudo (LI { rd = Tmp 2; imm = 1 });
    Label loop_entry;
    pseudo (BNEZ { rs = Tmp 1; offset = loop_exec });
    pseudo (JR { rs = Tmp 0 });
  ]
  in
  let exit = [
    Label loop_exec;
    actual (ADDI { dst = Tmp 2; lhs = Tmp 2; rhs = Int 1 });
    actual (LW { dst = Tmp 1; base = Tmp 1; offset = Int wsize });
    pseudo (J { offset = loop_entry })
  ]
  in
  let assign_arg_label i = Printf.sprintf "__apply_ignore_assign_%d" i in
  let assign_arg i = [
    pseudo (LI { rd = Tmp 3; imm = i });
    actual (BNE { lhs = Tmp 2; rhs = Tmp 3; offset =  Raw (assign_arg_label i) });
    actual (LW { dst = Arg (i + 1); base = Tmp 1; offset = Int 0 });  (* car *)
    Label (assign_arg_label i);
  ]
  in
  let ulis = iota (upper - 1) in
  List.flatten [
    entry;
    List.flatten (List.map assign_arg ulis);
    exit
  ]

let display =
  let format_s = "__display_format" in
  [
    Ops (Section Rodata);
    Label format_s;
    Ops (String "%d\\n");
    Ops (Section Text);
    Ops (Globl (Symbol "__display"));
    Label "__display";
    actual (LUI { dst = Arg 0; imm = Hi (format_s, 0) });
    actual (ADDI { dst = Arg 0; lhs = Arg 0; rhs = Lo (format_s, 0) });
    pseudo (TAIL { offset = "printf" })
  ]

let allocate =
  let free_list = "__free_list" in
  let entry = [
    Ops (Section Data);
    Ops (Globl (Symbol free_list));
    Label free_list;
    Ops (Word (Num Int32.zero));
    Ops (Section Text);
    Ops (Globl (Symbol "allocate"));
    Label "allocate";
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

let closure =
  let plis = [ 
    SymbolType.ADD; 
    SymbolType.SUB;
    SymbolType.EQ;
    SymbolType.LESS;
    SymbolType.NULL;
    SymbolType.CONS;
    SymbolType.CAR;
    SymbolType.CDR;
    SymbolType.LISTREF;
    SymbolType.APPLY;
    SymbolType.DISPLAY
  ]
  in
  let gen p =
    let func = func_of_prim p in
    let closure = closure_of_prim p in
    [ 
      Ops (Section Rodata);
      Label closure;
      Ops (Word (Symbol func));
      Ops (Word (Num Int32.zero));
    ]
  in
  plis
  |> List.map gen
  |> List.flatten

let lib = List.flatten [
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
  allocate;
  closure
]
