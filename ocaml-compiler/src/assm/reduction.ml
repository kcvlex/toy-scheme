open RiscvAssm
open ActualInstr
open PseudoInstr
open RiscvAssmGen

exception Unimpled
  
let bind i = Instr i

let modify lis = 
  let rec validate lis = match lis with
    | ((Instr (Actual (LW { base = FP; dst = CalleeSaved _; offset = _; }))) as hd) :: tl -> 
        (match validate tl with
          | Some (a, b) -> Some (hd :: a, b)
          | None -> None)
    | (Instr (Pseudo RET)) :: tl -> Some ([ Instr (Pseudo RET) ], tl)
    | _ -> None
  in
  match lis with
    | ((Instr (Pseudo (CALL { offset }))) as hd) :: tl -> (match validate tl with
      | Some (a, b) ->
          let a = List.append a [ Instr (Pseudo (TAIL { offset })) ] in
          (a, b)
      | None -> ([ hd ], tl))
  | hd :: tl -> ([ hd ], tl)
  | [] -> raise (Invalid_argument "[]")

let rec tail_call_opt lis = match lis with
  | [] -> []
  | _ ->
    let hd, tl = modify lis in
    let tl = tail_call_opt tl in
    List.append hd tl

let reduction assm = assm |> tail_call_opt

let rec convert assm = match assm with
  | [] -> []
  | ((Label _) as hd) :: tl -> hd :: (convert tl)
  | ((Ops _) as hd) :: tl -> hd :: (convert tl)
  | ((Instr (Actual _)) as hd) :: tl -> hd :: (convert tl)
  | (Instr (Pseudo p)) :: tl ->
      let tl = convert tl in
      let hd = match p with
        | LA { rd; symbol }      -> [ Actual (LUI { dst = rd; imm = Hi (symbol, 0) });    (* FIXME : LUI *)
                                      Actual (ADDI { dst = rd; lhs = rd; rhs = Lo (symbol, 0) }) ]
        | LG { rd; symbol }      -> [ Actual (LUI { dst = rd; imm = Hi (symbol, 0) });    (* FIXME : LUI *)
                                      Actual (LW { dst = rd; base = rd; offset = Lo (symbol, 0) }) ]
        | SG { rd; rt; symbol }  -> [ Actual (LUI { dst = rt; imm = Hi (symbol, 0) });    (* FIXME : LUI *)
                                      Actual (SW { src = rd; base = rt; offset = Lo (symbol, 0) }) ]
        | NOP                    -> [ Actual (ADDI { dst = ZERO; lhs = ZERO; rhs = Int 0 }) ]
        | LI { rd; imm }         -> [ Actual (ADDI { dst = rd; lhs = ZERO; rhs = Int imm }) ]
        | MV { rd; rs }          -> [ Actual (ADDI { dst = rd; lhs = rs; rhs = Int 0  }) ]
        | NOT { rd; rs }         -> [ Actual (XORI { dst = rd; lhs = rs; rhs = Int (-1) }) ]
        | NEG { rd; rs }         -> [ Actual (SUB { dst = rd; lhs = ZERO; rhs = rs }) ]
        | SEQZ { rd; rs }        -> [ Actual (SLTIU { dst = rd; lhs = rs; rhs = Int 1 }) ]
        | SNEZ { rd; rs }        -> [ Actual (SLTU { dst = rd; lhs = ZERO; rhs = rs }) ]
        | SLTZ { rd; rs }        -> [ Actual (SLT { dst = rd; lhs = rs; rhs = ZERO }) ]
        | SGTZ { rd; rs }        -> [ Actual (SLT { dst = rd; lhs = ZERO; rhs = rs }) ]
        | BEQZ { rs; offset }    -> [ Actual (BEQ { lhs = rs; rhs = ZERO; offset = Raw offset }) ]
        | BNEZ { rs; offset }    -> [ Actual (BNE { lhs = rs; rhs = ZERO; offset = Raw offset }) ]
        | BLEZ { rs; offset }    -> [ Actual (BGE { lhs = ZERO; rhs = rs; offset = Raw offset }) ]
        | BGEZ { rs; offset }    -> [ Actual (BGE { lhs = rs; rhs = ZERO; offset = Raw offset }) ]
        | BLTZ { rs; offset }    -> [ Actual (BLT { lhs = rs; rhs = ZERO; offset = Raw offset }) ]
        | BGTZ { rs; offset }    -> [ Actual (BLT { lhs = ZERO; rhs = rs; offset = Raw offset }) ]
        | BGT { rs; rt; offset } -> [ Actual (BLT { lhs = rt; rhs = rs; offset = Raw offset }) ]
        | BLE { rs; rt; offset } -> [ Actual (BGE { lhs = rt; rhs = rs; offset = Raw offset }) ]
        | J { offset }           -> [ Actual (JAL { dst = ZERO; offset = Raw offset }) ]
        | JAL { offset }         -> [ Actual (JAL { dst = RA; offset = Raw offset }) ]
        | JR { rs }              -> [ Actual (JALR { dst = ZERO; base = rs; offset = Int 0 }) ]
        | JALR { rs }            -> [ Actual (JALR { dst = RA; base = rs; offset = Int 0 }) ]
        | RET                    -> [ Actual (JALR { dst = ZERO; base = RA; offset = Int 0 }) ]
        | CALL { offset }        -> [ Actual (LUI { dst = Tmp 1; imm = Hi (offset, 0) });   (* FIXME : LUI *)
                                      Actual (JALR { base = Tmp 1; dst = RA; offset = Lo (offset, 0) }) ]
        | TAIL { offset }        -> [ Actual (LUI { dst = Tmp 1; imm = Hi (offset, 0) });   (* FIXME : LUI *)
                                      Actual (JALR { base = Tmp 1; dst = ZERO; offset = Lo (offset, 0) }) ]
      in
      let hd = List.map bind hd in
      List.append hd tl
