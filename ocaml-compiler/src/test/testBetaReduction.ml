open Compiler
open TestUtil

let decorate cps_code =
  cps_code |> fun x -> "(" ^ x ^ " display)"
           |> fun x -> 
               [ add_cps; 
                 sub_cps; 
                 mul_cps;
                 div_cps;
                 less_cps;
                 cons_cps;
                 car_cps;
                 cdr_cps;
                 null_cps;
                 eq_cps;
                 apply_cps;
                 list_ref_cps;
                 x ]
           |> String.concat " "
 
let po = "((lambda (x) (let ((y 10) (z 20) (w (lambda (x) (* x x x)))) (* (w x) y z))) 42)"
let () =
  let code = 
    source2 |> Ast.make_ast
            |> Ast.normalize
            |> Cps.cps_trans
            |> AdmBetaReduction.normalize
            |> Cps.ast_of_cps
            |> Ast.code_of_ast
            |> decorate
  in
  print_label "Reduced CPS"; print_endline code
