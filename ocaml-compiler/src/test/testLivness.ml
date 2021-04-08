open Util
open TestUtil
open Compiler

let vreg2str vr = match vr with
  | ThreeAddressCodeType.Virtual i -> string_of_int i
  | _ -> raise (Invalid_argument "Unkonwn register")

let validate_regset rset lis =
  let string_of_rlis l =
    String.concat " " (List.map ThreeAddressCode.string_of_reg l)
  in
  let s1 = Hashtbl.fold (fun x _ l -> x :: l) rset [] in
  let s1 = string_of_rlis s1 in
  let s2 = string_of_rlis lis in
  Printf.printf "solved : [ %s ], expect : [ %s ]\n"  s1 s2;
  if List.length lis != Hashtbl.length rset then
    false
  else
    List.fold_left (fun x y -> x && (Hashtbl.mem rset y)) true lis

let () =
  let sample = ThreeAddressCode.sample_program in
  let label_tbl = sample.label_tbl in
  let seq = sample.seq in
  let regs = ThreeAddressCode.make_reg_set (1, 1, 0) in
  let liveness = Liveness.analyze regs seq label_tbl in
  let vr i = ThreeAddressCodeType.Virtual i in
  let cer i = ThreeAddressCodeType.CallerSaved i in
  let cee i = ThreeAddressCodeType.CalleeSaved i in
  let rv = ThreeAddressCodeType.RV in
  let correct = [
    ([ vr 2; cee 0 ],       [ vr  0; vr 2; cee 0 ]);
    ([ vr 0; vr 2; cee 0 ], [ vr  1; vr 2; cee 0 ]);
    ([ vr 1; vr 2; cee 0 ], [ vr  1; vr 2; cee 0 ]);
    ([ vr 1; vr 2; cee 0 ], [ vr  0; vr 2; cee 0 ]);
    ([ vr 0; vr 2; cee 0 ], [ vr  0; vr 2; cee 0 ]);
    ([ vr 0; vr 2; cee 0 ], [ vr  0; vr 2; cee 0 ]);
    ([ vr 2; cee 0 ],       [ cee 0; rv ]         );
    ([ cee 0; rv ],         [ cee 0; rv ]         )
  ]
  in
  let correct = Vector.vector_of_list correct in
  for i = 0 to (Liveness.length liveness) - 1 do
    let l_in = Hashtbl.copy (Liveness.live_in liveness i) in
    let l_out = Hashtbl.copy (Liveness.live_out liveness i) in
    let c_in, c_out = Vector.get correct i in
    if not (validate_regset l_in c_in) then
      failwith (Printf.sprintf "live-in %d" i)
    else if not (validate_regset l_out c_out) then
      failwith (Printf.sprintf "live-out %d" i)
    else
      ()
  done;
  print_label "test passed : Liveness analysis"
