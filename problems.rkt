;;; Various problems solution
;;; Seungjoo Kim
;;; sjkr012

;;;;;;;;;;;;;;;;;;;;;;;;; Question 1 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; remove-if function
(define (remove-if f l)
  (cond ((null? l) '())
        ((equal? (f (car l)) #f ) (cons (car l)(remove-if f (cdr l))))
        (else (remove-if f (cdr l)))))

;;;;;;;;;;;;;;;;;;;;;;;;; Question 2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; removeLast function
(define (removeLast l)
  (cond ((null? l) '())
        ((not(null? (cdr l))) (cons (car l)(removeLast (cdr l))))
        (else '())))

;;;;;;;;;;;;;;;;;;;;;;;;; Question 3 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; rev function
(define (rev l1 l2)
  (define (revOnly l)
  (cond ((null? l) '())
        (else (append(revOnly (cdr l))(list (car l))))))
  (cond ((null? l1) l2)
        ((null? l2) (revOnly l1))
        (else (append(revOnly l1)l2))))

;;; reverseList function
(define (reverseList l)
  (cond ((null? l) '())
        (else (rev l '()))))


;;;;;;;;;;;;;;;;;;;;;;;;; Question 4 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; helper function: tenth
;;; this basically assign tenth int value into words
(define (tenth x)
   (cond ((= x 2) '(twenty))
         ((= x 3) '(thirty))
         ((= x 4) '(forty))
         ((= x 5) '(fifty))
         ((= x 6) '(sixty))
         ((= x 7) '(seventy))
         ((= x 8) '(eighty))
         ((= x 9) '(ninety))
         (else '())))

;;; helper function: oneth
;;; this assign oneth int value into words
(define (oneth x)
  (cond ((= x 0) '(zero))
        ((= x 1) '(one))
        ((= x 2) '(two))
        ((= x 3) '(three))
        ((= x 4) '(four))
        ((= x 5) '(five))
        ((= x 6) '(six))
        ((= x 7) '(seven))
        ((= x 8) '(eight))
        ((= x 9) '(nine))
        ((= x 10) '(ten))
        ((= x 11) '(eleven))
        ((= x 12) '(twelve))
        ((= x 13) '(thirteen))
        ((= x 14) '(fourteen))
        ((= x 15) '(fifteen))
        ((= x 16) '(sixteen))
        ((= x 17) '(seventeen))
        ((= x 18) '(eighteen))
        ((= x 19) '(nineteen))
        (else '())))

;;; int-to-words function using uppper two helper functions
(define (int-to-words x)
   (cond ((> (quotient x 10) 1)
             (if (= (remainder x 10) 0) (tenth(quotient x 10))
                 (append (tenth(quotient x 10))(oneth(remainder x 10)))))
         (else (oneth x))))

;;; list = {twenty, thrity, fourty, fifty, sixty, seventy, eighty, ninety)
;;; list2 = {one, two, three, four ...}

;;;;;;;;;;;;;;;;;;;;;;;;; Question 5 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; nzero function
(define (nzero n)
  (if (= n 0) '()
      (cons 0 (nzero(- n 1)))))

;;; polyAdd function
(define (polyAdd l1 l2)
  (cond ((and (null? l1) (null? l2)) '())
        ((null? l1)
             (if (null? l2) '()
                 (append '() (append (list(car l2)) (cdr l2)))))
        ((null? l2)
             (if (null? l1) '()
                 (append '() (append (list(car l1)) (cdr l1)))))
        (else (cons (+ (car l1) (car l2))(polyAdd (cdr l1) (cdr l2))))))

;;; polyAddList function
(define (polyAddList l)
  (cond ((null? l) '())
        (else (polyAdd (car l) (polyAddList (cdr l))))))

;;; helper function: listMult
;;; this function multiplies the first element in l1 to l2
(define (listMult l1 l2)
  (cond ((null? l1) '())
        ((null? l2) '())
        (else (cons (* (car l1) (car l2)) (listMult l1 (cdr l2))))))

;;; helper function: numberofElements
;;; this functions prints number of elements in the list
(define (numberofElements l)
  (cond ((null? l) 0)
        (else (+ 1 (numberofElements (cdr l))))))


;;; helper function: polyMultHelper
(define (polyMultHelper l1 l2 n)
  (cond ((null? l1) '())
        ((null? l2) '())
        ((equal? (numberofElements l1) 0) '())
        (else (cons (append (nzero n) (listMult l1 l2)) (polyMultHelper (cdr l1) l2 (+ n 1))))))

;;; polyMult function
(define (polyMult l1 l2)
  (polyAddList (polyMultHelper l1 l2 0)))


