let decoration =
  let rec repeat n s =
    if n = 0 then "" else (s ^ (repeat (n - 1) s))
  in
  repeat 10 "="

let source1 = 
  "(display ((lambda () \
     (define a 1) \
     (define b ((lambda () (+ a a)))) \
     ((lambda (x y) (+ x y a b)) ((lambda (x) x) 42) 43))))"

let source2 = 
  "(display ((lambda (n) \
     (define fib \
        (lambda (k) \
          (cond ((< k 2) 1) \
                (else (+ (fib (- k 1)) (fib (- k 2))))))) \
      (fib n)) 6))"

let source3 = 
  "(display ((lambda () \
     (define odd \
        (lambda (n) (cond ((eq? n 0) #f) (else (even (- n 1)))))) \
      (define even \
        (lambda (n) (cond ((eq? n 0) #t) (else (odd (- n 1)))))) \
      (odd 50))))"

let source4 =
  "(display ((lambda () \
     (let ((x 10) \
           (y 20) \
           (z 30)) \
       (- x y z)))))"

let source5 =
  "(display ((lambda () \
     (define fib-aux \
       (lambda (n i a0 a1) \
         (if (eq? n i) a1 (fib-aux n (+ i 1) a1 (+ a0 a1))))) \
     ((lambda () \
       (define fib \
         (lambda (n) \ (if (< n 2) 1 (fib-aux n 1 1 1)))) \
       (fib 20))))))"

let source6 =
  "(display ((lambda () \
     (define fib-aux \
       (lambda (n i a0 a1) \
         (if (eq? n i) a1 (fib-aux n (+ i 1) a1 (+ a0 a1))))) \
     (fib-aux 15 1 1 1))))"

let print_label s = print_endline ("\n\n" ^ decoration ^ " " ^ s ^ " " ^ decoration ^ "\n")
