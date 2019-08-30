;;; function solutions
;;; Seungjoo Kim
;;; sjkr012

;;; fib(n) function
(define (fib n)
  (cond (( = n 0) 0)
        (( = n 1) 1)
        (( > n 1) (+ (fib (- n 1)) (fib (- n 2))))
        (else 'error)))

;;; F(n) and M(n) functions
(define (F n)
  (cond (( = n 0) 1)
        (( > n 0) (- n (M (F (- n 1)))))
        (else 'error)))

(define (M n)
  (cond (( = n 0) 0)
        (( > n 0) (- n (F (M (- n 1)))))
        (else 'error)))

;;; gcd(x,y) function
(define (mygcd x y)
  (cond (( = y 0) x)
        (( > y 0) (mygcd y (remainder x y)))
        (else 'error)))

;;; add-one(x) and ncall(n,f,x) functions
(define (add-one x) (+ x 1))

(define (ncall n f x)
  (cond (( = n 0) x)
        (( > n 0) (ncall (- n 1) f (f x)))
        (else 'error)))


