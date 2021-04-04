open Compiler

let () =
  let sample = ThreeAddressCode.sample_program in
  let vreg2str vr = match vr with
    | ThreeAddressCodeType.Virtual i -> string_of_int i
    | _ -> rais (Invalid_argument "Unkonwn register")
  in
  let tbl2list tbl =
    Hashtbl.fold (fun x _ l -> x :: l) tbl []
  in
  let regset2str tbl =
    tbl |> tbl2list |> List.map vreg2str |> String.concat " "
  in
  let liveness = LivnessAnalysis.analyze sample in
  for i = 0 to (LivnessAnalysis.length liveness) - 1 do
    let l_in = LivenessAnalysis.live_in liveness i in
    let l_out = LivenessAnalysis.live_out liveness i in
    Printf.printf("\n\n%d : \n  in : %s\n  out : %s\n") i (regset2str l_in) (regset2str l_out)
  done
