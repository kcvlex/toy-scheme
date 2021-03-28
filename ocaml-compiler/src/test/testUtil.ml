open Compiler

(*
let gen_primitive_cps name op =
  Printf.sprintf "(define %s (lambda (_ k . args) (__call k (apply %s args))))" name op
*)

let gen_primitive_cps name op =
  Printf.sprintf "(define %s (lambda (k . args) (k (apply %s args))))" name op

let decoration =
  let rec repeat n s =
    if n = 0 then "" else (s ^ (repeat (n - 1) s))
  in
  repeat 10 "="

let source1 = 
  "((lambda () \
     (define a 1) \
     (define b ((lambda () (+ a a)))) \
     ((lambda (x y) (+ x y a b)) ((lambda (x) x) 42) 43)))"

let source2 = 
  "((lambda (n) \
     (define fib \
        (lambda (k) \
          (cond ((< k 2) 1) \
                (else (+ (fib (- k 1)) (fib (- k 2))))))) \
      (fib n)) 15)"

let source3 = 
  "((lambda () \
     (define odd \
        (lambda (n) (cond ((eq? n 0) #f) (else (even (- n 1)))))) \
      (define even \
        (lambda (n) (cond ((eq? n 0) #t) (else (odd (- n 1)))))) \
      (odd 10)))"

let add_cps = gen_primitive_cps "cps-add" "+"
let sub_cps = gen_primitive_cps "cps-sub" "-"
let mul_cps = gen_primitive_cps "cps-mul" "*"
let div_cps = gen_primitive_cps "cps-div" "/"
let less_cps = gen_primitive_cps "cps-less" "<"
let eq_cps = gen_primitive_cps "cps-eq?" "eq?"

let apply_cps = "(define cps-apply (lambda (k f args) (apply f (cons k args))))"
let cons_cps = "(define cps-cons (lambda (k a b) (k (cons a b))))"
let car_cps = "(define cps-car (lambda (k l) (k (car l))))"
let cdr_cps = "(define cps-cdr (lambda (k l) (k (cdr l))))"
let null_cps = "(define cps-null? (lambda (k a) (k (null? a))))"
let list_ref_cps = "(define cps-list-ref (lambda (k l i) (k (list-ref l i))))"
let display_func = "(define display-f (lambda (_ a) (display a)))"

let call_func =
  "(define __call \
     (lambda (f . args) \
       (if (pair? f) \
           (apply (car f) (cons (cdr f) args)) \
           (apply f (cons (make-vector 0) args)))))"
 
let append_primitives src = add_cps ^ sub_cps ^ less_cps ^ eq_cps ^ src

let print_label s = print_endline ("\n\n" ^ decoration ^ " " ^ s ^ " " ^ decoration ^ "\n")
