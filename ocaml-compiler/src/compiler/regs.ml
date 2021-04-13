open RegsType

let make_reg_set reg_num =
  let caller_saved, callee_saved, args = reg_num in
  let rec aux i n cnstr =
    if i = n then [] else (cnstr i) :: (aux (i + 1) n cnstr)
  in
  let caller_saved_regs = aux 0 caller_saved (fun x -> CallerSaved x) in
  let callee_saved_regs = aux 0 callee_saved (fun x -> CalleeSaved x) in
  let argument_regs = aux 0 args (fun x -> Argument x) in
  let all_regs = List.flatten [ caller_saved_regs; callee_saved_regs; argument_regs ] in
  let reg_sum = List.length all_regs in
  { caller_saved_regs; callee_saved_regs; argument_regs; all_regs; reg_sum; }

let string_of_reg r = match r with
  | CallerSaved i -> Printf.sprintf "CER_%d" i
  | CalleeSaved i -> Printf.sprintf "CEE_%d" i
  | Argument i -> Printf.sprintf "ARG_%d" i
  | Virtual i -> Printf.sprintf "VTR_%d" i

let string_of_regtbl rs =
  (Hashtbl.fold (fun x _ l -> x :: l) rs [])
  |> List.map string_of_reg
  |> String.concat " "
  |> Printf.sprintf "[ %s ]"
