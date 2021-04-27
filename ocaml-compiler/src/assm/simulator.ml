open Rv32i
open RiscvAssm
open RiscvAssmGen
open ActualInstr
open Compiler
open Util

(*
type value_type = Int32.t

(* value is address *)
type symtbl_type = (string, value_type) Hashtbl.t

exception Unimpled

type t = {
  stack : value_type Array.t;
  instr_mem : ActualInstr.t Array.t;
  label2addr : (string, value_type) Hashtbl.t;
  symtbl : symtbl_type;
  mutable pc : value_type;
  regfile : (reg_type * value_type) Array.t
}

let stk_len = Int32.of_int 65536
let wsize = Int32.of_int 4
let sys_display_label = "__sys_display"
let exit_addr = Int32.mul Int32.minus_one wsize

let to_bin n =
  let iota n =
    let rec aux i = if i = n then [] else i :: (aux (i + 1)) in
    aux 0
  in
  (iota 32)
  |> List.map (Int32.shift_left Int32.one)
  |> List.rev
  |> List.map (Int32.logand n)
  |> List.map (fun x -> if x = Int32.zero then "0" else "1")
  |> String.concat ""

let read_reg regfile reg =
  let rec aux idx =
    if idx = Array.length regfile then
      raise (Invalid_argument (string_of_reg reg))
    else
      match Array.get regfile idx with
        | (r, v) when r = reg -> v
        | _ -> aux (idx + 1)
  in
  aux 0

let write_reg regfile reg v =
  let rec aux idx =
    if idx = Array.length regfile then
      raise (Invalid_argument (string_of_reg reg))
    else begin
      match Array.get regfile idx with
        | (r, _) when r = reg ->
            if r = ZERO then
              ()
            else
              Array.set regfile idx (r, v)
        | _ -> aux (idx + 1)
    end
  in
  aux 0

let read_mem sim addr = 
  let idx = Int32.div addr wsize |> Int32.to_int in
  Array.get sim.stack idx

let write_mem sim addr v =
  let idx = Int32.div addr wsize |> Int32.to_int in
  Array.set sim.stack idx v

let init_regfile reg_num =
  let rec construct f i u =
    if i = u then [] else (f i) :: (construct f (i + 1) u)
  in
  let tr, cr, ar = reg_num in
  let tregs = construct (fun x -> Tmp x) 0 tr in
  let cregs = construct (fun x -> CalleeSaved x) 1 (cr + 1) in
  let aregs = construct (fun x -> Arg x) 0 ar in
  let regs = List.flatten [ tregs; cregs; aregs ] in
  let regs = ZERO :: RA :: SP :: FP :: regs in
  regs
  |> List.map (fun x -> (x, Int32.zero))
  |> Array.of_list

let make_stack symtbl data_lis =
  let stack_len = wsize |> Int32.div stk_len |> Int32.to_int in
  let stack = Array.make stack_len Int32.zero in
  let rec pack lis idx = match lis with
    | x :: xs ->
        Array.set stack idx x;
        pack xs (idx - 1)
    | [] -> idx |> Int32.of_int |> Int32.mul wsize
  in
  let sbtm = pack (List.rev data_lis) (stack_len - 1) in
  let new_symtbl = Hashtbl.create (Hashtbl.length symtbl) in
  let data_len =
    data_lis
    |> List.length
    |> Int32.of_int
    |> Int32.mul wsize
  in
  let fix_offset offset =
    stk_len
    |> Int32.sub data_len
    |> Int32.neg
    |> Int32.add offset
  in
  Hashtbl.iter (fun x y -> Hashtbl.add new_symtbl x (fix_offset y)) symtbl;
  (stack, new_symtbl, sbtm)

let load_assm assm =
  let label2addr = Hashtbl.create 8 in
  let symtbl = Hashtbl.create 8 in
  let rec loading assm instr_len data_len section = match assm with
    | (Ops o) :: tl -> (match o with
        | File _ | String _ -> raise Unimpled
        | Globl _ | Local _ -> loading tl instr_len data_len section
        | Section section -> loading tl instr_len data_len section
        | Word w ->
            let data_len = Int32.add data_len wsize in
            let instr, data = loading tl instr_len data_len section in
            (instr, w :: data))
    | (Label s) :: tl ->
        let () = match section with
          | Text -> Hashtbl.add label2addr s instr_len
          | _ -> Hashtbl.add symtbl s data_len
        in
        loading tl instr_len data_len section
    | (Instr (Pseudo _)) :: _ -> raise (Invalid_argument "Pseudo instruction")
    | (Instr (Actual i)) :: tl ->
        let instr_len = Int32.add instr_len wsize in
        let instr, data = loading tl instr_len data_len section in
        (i :: instr, data)
    | [] -> 
        Hashtbl.add label2addr sys_display_label instr_len;
        let sys_display_impl = 
          [ ADD { dst = ZERO; rhs = ZERO; lhs = ZERO };  (* NOP *)
            ADD { dst = Arg 0; lhs = ZERO; rhs = ZERO };
            JALR { base = RA; dst = ZERO; offset = Int 0 } ]
        in
        let rec aux plis data_len = match plis with
          | x :: xs ->
              let func = func_of_prim x in
              let closure = closure_of_prim x in
              let addr = Hashtbl.find label2addr func in
              Hashtbl.add symtbl closure data_len;
              let data_len = data_len |> Int32.add wsize |> Int32.add wsize in
              let xs = aux xs data_len in
              addr :: Int32.zero :: xs
          | [] -> []
        in
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
        (sys_display_impl, aux plis data_len)
  in
  let instr_lis, data_lis = loading assm Int32.zero Int32.zero Text in
  let instr_arr = Array.of_list instr_lis in
  for i = 0 to (Array.length instr_arr)-1 do
    Hashtbl.iter (fun x y -> 
      if y = Int32.of_int (i * 4) then
        Printf.printf "%s:\n" x
      else
        ()
    ) label2addr; 
    Printf.printf "%s\n" (string_of_actual (Array.get instr_arr i))
  done;
  let stack, symtbl, sbtm = make_stack symtbl data_lis in
  (instr_arr, label2addr, stack, symtbl, sbtm)

let init reg_num assm =
  let regfile = init_regfile reg_num in
  let instr_mem, label2addr, stack, symtbl, sbtm = load_assm assm in
  let pc = Hashtbl.find label2addr "entry" in
  write_reg regfile RA exit_addr;
  write_reg regfile SP sbtm;
  {
    stack;
    instr_mem;
    label2addr;
    symtbl;
    pc;
    regfile;
  }

let eval_sym sim s =
  if Hashtbl.mem sim.label2addr s then begin
    (*
    print_endline s;
    print_endline (Int32.to_string (Hashtbl.find sim.label2addr s));
    *)
    Hashtbl.find sim.label2addr s
  end else if Hashtbl.mem sim.symtbl s then
    Hashtbl.find sim.symtbl s
  else
    raise (Invalid_argument s)

let int_range u32 a b =
  let mask =
    (a + 1)
    |> Int32.shift_left Int32.one
    |> Int32.pred
  in
  (u32 |> Int32.logand mask |> Int32.shift_right_logical) b

let int_hi u32 = int_range u32 31 12

let int_lo u32 = int_range u32 11 0

(* xlen bit -> 32 bit *)
let sign_extend xlen n =
  let rem = 32 - xlen in
  let n = Int32.shift_left n rem in
  let n = Int32.shift_right n rem in
  n

let comp_as_unsigned =
  let mask = 
    32 
    |> Int64.shift_left Int64.one 
    |> Int64.pred
  in
  let norm a = a |> Int64.of_int32 |> Int64.logand mask in
  fun a b -> Int64.compare (norm a) (norm b)

let eval_imm sim i = match i with
  | Int i -> Int32.of_int i
  | Hi (s, i) -> s |> eval_sym sim |> Int32.add (Int32.of_int i) |> int_hi
  | Lo (s, i) -> s |> eval_sym sim |> Int32.add (Int32.of_int i) |> int_lo
  | PcrelHi (s, i) -> s |> eval_sym sim |> Int32.add (Int32.of_int i) |> Int32.sub sim.pc |> Int32.neg |> int_hi
  | PcrelLo s -> s |> eval_sym sim |> Int32.sub sim.pc |> Int32.neg |> int_lo

(* Deal with `offset` as absolute address *)
let step_aux sim =
  let idx = Int32.div sim.pc wsize in
  let instr = Array.get sim.instr_mem (Int32.to_int idx) in
  Printf.printf "%s:\t%s\n" (Int32.to_string sim.pc) (string_of_actual instr);
  let jump dst addr =
    write_reg sim.regfile dst (Int32.add sim.pc wsize);
    sim.pc <- addr;
    true
  in
  let branch r1 r2 comp offset =
    let r1 = read_reg sim.regfile r1 in
    let r2 = read_reg sim.regfile r2 in
    let p = comp r1 r2 in
    if p then begin
      let addr = Int32.add offset sim.pc in
      sim.pc <- addr;
      true
    end else
      false
  in 
  let binop_ri dst r1 r2 reduce =
    let r1 = read_reg sim.regfile r1 in
    let r2 = eval_imm sim r2 in
    write_reg sim.regfile dst (reduce r1 r2);
    false
  in
  let binop_rr dst r1 r2 reduce =
    let r1 = read_reg sim.regfile r1 in
    let r2 = read_reg sim.regfile r2 in
    write_reg sim.regfile dst (reduce r1 r2);
    false
  in
  let slt a b =
    if Int32.compare a b < 0 then Int32.one else Int32.zero
  in
  let sll a b = Int32.shift_left a (Int32.to_int b) in
  let srl a b = Int32.shift_right a (Int32.to_int b) in
  let sra a b = Int32.shift_right a (Int32.to_int b) in
  let slt a b = if comp_as_unsigned a b < 0 then Int32.one else Int32.zero in
  let jumped = match instr with
    | LUI { dst; imm } -> 
        begin
          imm 
          |> eval_imm sim
          |> fun x -> Int32.shift_left x 12
          |> write_reg sim.regfile dst;
          false
        end
    | AUIPC { dst; imm } -> 
        begin
          imm
          |> eval_imm sim
          |> fun x -> Int32.shift_left x 12
          |> Int32.add sim.pc
          |> write_reg sim.regfile dst;
          false
        end
    | JAL { dst; offset } -> 
        offset |> eval_imm sim |> sign_extend 12 |> Int32.add sim.pc |> jump dst
    | JALR { base; dst; offset } ->
        let base = base |> read_reg sim.regfile in
        offset |> eval_imm sim |> Int32.add base |> jump dst
    | BEQ { lhs; rhs; offset } -> offset |> eval_imm sim |> branch lhs rhs (=)
    | BNE { lhs; rhs; offset } -> offset |> eval_imm sim |> branch lhs rhs (!=)
    | BLT { lhs; rhs; offset } -> offset |> eval_imm sim |> branch lhs rhs (fun x y -> Int32.compare x y < 0)
    | BGE { lhs; rhs; offset } -> offset |> eval_imm sim |> branch lhs rhs (fun x y -> Int32.compare x y >= 0)
    | BLTU _ | BGEU _ -> raise Unimpled
    | LB _ | LH _ | LBU _ | LHU _ -> raise Unimpled
    | LW { base; dst; offset } ->
        let base = read_reg sim.regfile base in
        offset |> eval_imm sim |> Int32.add base |> read_mem sim |> write_reg sim.regfile dst;
        false
    | SB _ | SH _ -> raise Unimpled
    | SW { base; src; offset } ->
        let base = read_reg sim.regfile base in
        let addr = offset |> eval_imm sim |> Int32.add base in
        src |> read_reg sim.regfile |> write_mem sim addr;
        false
    | ADDI { lhs; rhs; dst } -> binop_ri dst lhs rhs Int32.add
    | SLTI { lhs; rhs; dst } -> binop_ri dst lhs rhs slt
    | XORI { lhs; rhs; dst } -> binop_ri dst lhs rhs Int32.logxor
    | ORI { lhs; rhs; dst } -> binop_ri dst lhs rhs Int32.logor
    | ANDI { lhs; rhs; dst } -> binop_ri dst lhs rhs Int32.logand
    | SLLI { lhs; rhs; dst } -> binop_ri dst lhs rhs sll
    | SRLI { lhs; rhs; dst } -> binop_ri dst lhs rhs srl
    | SRAI { lhs; rhs; dst } -> binop_ri dst lhs rhs sra
    | SLTIU { lhs; rhs; dst } ->
        let lhs = read_reg sim.regfile lhs in
        let rhs = rhs |> eval_imm sim |> sign_extend 12 in
        write_reg sim.regfile dst (slt lhs rhs);
        false
    | ADD { lhs; rhs; dst } -> binop_rr dst lhs rhs Int32.add
    | SUB { lhs; rhs; dst } -> binop_rr dst lhs rhs Int32.sub
    | SLL { lhs; rhs; dst } -> binop_rr dst lhs rhs sll
    | SLT { lhs; rhs; dst } -> binop_rr dst lhs rhs slt
    | XOR { lhs; rhs; dst } -> binop_rr dst lhs rhs Int32.logxor
    | SRL { lhs; rhs; dst } -> binop_rr dst lhs rhs srl
    | SRA { lhs; rhs; dst } -> binop_rr dst lhs rhs sra
    | OR { lhs; rhs; dst } -> binop_rr dst lhs rhs Int32.logor
    | AND { lhs; rhs; dst } -> binop_rr dst lhs rhs Int32.logand
    | SLTU _ -> raise Unimpled
  in
  if not jumped then
    sim.pc <- Int32.add sim.pc wsize
  else
    ()

let dump_regfile sim =
  let to_str (r, v) =
    let r = string_of_reg r in
    let v = Int32.to_int v in
    Printf.sprintf "%s  --->  %d" r v
  in
  sim.regfile
  |> Array.to_list
  |> List.map to_str
  |> String.concat "\n"

let counter = ref 0
let upper = 10000
let bound = true
let step sim =
  try
    if upper <= !counter then
      not bound
    else if sim.pc = Hashtbl.find sim.label2addr sys_display_label then begin
      print_endline (Int32.to_string (read_reg sim.regfile (Arg 1)));
      sim.pc <- Int32.add sim.pc wsize;
      true
    end else if sim.pc = exit_addr then
      false
    else begin
      step_aux sim;
      incr counter;
      true
    end
  with _  as e -> print_endline (dump_regfile sim); raise e


let eval sim =
  while step sim do
    ()
  done
*)
