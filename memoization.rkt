;;; MEMOIZATION IN SCHEME
;;; SEUNGJOO KIM
;;; sjkr012



;;;; fac function
(define (fac n)
    (cond ((not(integer? n)) (display "ERROR: please type integer"))
          ((= n 0) 1)
          (else (* n (fac (- n 1))))))

;;; bind function
(define (bind k v al)
    (cons (list k v) al))

;;; lookup function
(define (lookup k al)
    (cond ((null? al) #f)
          ((equal? k (caar al)) (cadr(car al)))
          (else (lookup k (cdr al)))))

;;; global variable al
(define al '())


;;; fac_mem function
(define (fac_mem n)
    (cond ((null? n) 'error)
          ((not(equal? (lookup n al) #f))
                (begin (display "memoization hit \n")(display (lookup n al))))
          (else (set! al (bind n (fac n) al))
                (fac n))))


;;; build_mem function
(define (build_mem f)
   (let ((l '()))
       (lambda (n)
           (cond ((null? n) 'error)
                 ((not(equal? (lookup n l) #f))
                      (begin (display "memoization hit \n")(f n)))
                 (else (set! l (bind n (f n) l))
                       (f n))))))


;;; test functions
(define test1 fac_mem) ;;;fac_mem test

(define test2 (build_mem fac)) ;;;build_mem test

(define mult_test (build_mem (lambda(x) (* x x)))) ;;;build_mem multiplication test