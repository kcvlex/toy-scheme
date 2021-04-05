open Util
open Compiler

let vreg2str vr = match vr with
  | ThreeAddressCodeType.Virtual i -> string_of_int i
  | _ -> raise (Invalid_argument "Unkonwn register")

let validate_regset rset lis =
  if List.length lis != Hashtbl.length rset then
    false
  else
    List.fold_left (fun x y -> x && (Hashtbl.mem rset y)) true lis

let make_reg i = ThreeAddressCodeType.Virtual i

let make_vec lis =
  let v = Vector.empty () in
  let rec make l = match l with
    | x :: xs -> Vector.push_back v x; make xs
    | [] -> ()
  in
  make lis; v

let () =
  let sample = ThreeAddressCode.sample_program in
  let liveness = Liveness.analyze sample in
  let correct = [
    ([ 2 ],    [ 0; 2 ]);
    ([ 0; 2 ], [ 1; 2 ]); 
    ([ 1; 2 ], [ 1; 2 ]);
    ([ 1; 2 ], [ 0; 2 ]);
    ([ 0; 2 ], [ 0; 2 ]);
    ([ 0; 2 ], [ 0; 2 ]);
    ([ 2 ],    []      );
    ([],       []      )
  ]
  in
  let correct = List.map (fun (x, y) -> (List.map make_reg x, List.map make_reg y)) correct in
  let correct = make_vec correct in
  for i = 0 to (Liveness.length liveness) - 1 do
    let l_in = Hashtbl.copy (Liveness.live_in liveness i) in
    let l_out = Hashtbl.copy (Liveness.live_out liveness i) in
    Hashtbl.remove l_in ThreeAddressCodeType.RV;
    Hashtbl.remove l_out ThreeAddressCodeType.RV;
    let c_in, c_out = Vector.get correct i in
    if not (validate_regset l_in c_in) then
      failwith (Printf.sprintf "live-in %d" i)
    else if not (validate_regset l_out c_out) then
      failwith (Printf.sprintf "live-out %d" i)
    else
      ()
  done
 
 (*
  let tbl2list tbl =
    Hashtbl.fold (fun x _ l -> x :: l) tbl []
  in
  let regset2str tbl =
    tbl |> tbl2list |> List.map vreg2str |> String.concat " "
  in
  *)
