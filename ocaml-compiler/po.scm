(define add (lambda (k . args) (k (apply + args))))
((lambda (k9) ((lambda (k8) (k8 (lambda (k0) ((lambda (k7) ((lambda (k6) (k6 1)) (lambda (t3) (define __v0 t3) ((lambda (k5) ((lambda (k4) (k4 2)) (lambda (t2) (define __v1 t2) ((lambda (k1) ((lambda (k2) (k2 a)) (lambda (t1) ((lambda (k3) (k3 b)) (lambda (t0) (add k1 t1 t0)))))) k5)))) k7)))) k0)))) (lambda (t4) (t4 k9 t4)))) (lambda (k) (display k)))

