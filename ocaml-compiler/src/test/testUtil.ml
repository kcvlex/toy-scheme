open Compiler

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

let source4 =
  "((lambda () \
     (let ((x 10) \
           (y 20) \
           (z 30)) \
       (* x y z))))"

let print_label s = print_endline ("\n\n" ^ decoration ^ " " ^ s ^ " " ^ decoration ^ "\n")
