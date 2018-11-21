#lang racket

(require rackunit
         (for-syntax syntax/parse))

(begin-for-syntax
  (define-syntax-class pat
    #:attributes ((vars 1) (test 0))
    #:literals (quote cons)
    [pattern (bin (id n) ...)
             #:with test #'(λ (mch)
                             (and (= (length '(id ...))
                                     (length (cdr mch)))
                                  (map car (cdr mch)))) ;; bit equal?
             #:with (vars ...) #'(id ...)]
    ))

(define-syntax (bit-match stx)
  (syntax-parse stx
    [(_ e [p:pat rhs] ...)
     #'(let ([mch e])
         (cond
           [(p.test mch) => (λ (result)
                              (let-values
                                  ([(p.vars ...)
                                    (apply values result)])
                                rhs))]
           ...
           [else (error 'bit-match "no matching pattern for ~v" e)]))]))


;(define-values (x y z) (10 10 10))
;
;(bit-match (bin x 4 y 2 z 20)
;           [(bin e1 n1 e2 n2 e3 n3) (+ e1 e2 e3)])

(bit-match (bytes 40 10)
  ((bin x 8 y 8) (+ x y))) ;; ==> 50

(bit-match (bytes 16)
 ((bin x 4 y 4) (+ x y))) ; ==> 1



(bit-match '(bin (40 10) (200 60))
  [(bin (y n)) y]
  [(bin (y n) (z m)) (+ y z)])

(check-equal? (bit-match '(bin (40 10))
                         [(bin (y n)) y])
              40)



;
;(bit-match '(bin 40 10)
;  [(bin x n) n])


;(define z 10)
;
;(bit-match `(bin ,z 10)
;  [(bin x n) (add1 n)])
;
;(bit-match `(bin ,z 10)
;  [(bin x n) x])
;
;(bit-match `(bin ,z 10)
;  [(bin x n) (begi x n)])
