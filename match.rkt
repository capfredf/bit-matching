#lang racket

(require (for-syntax syntax/parse))

(define z 10)


(begin-for-syntax
  (define-syntax-class pat
    #:attributes ((vars 1) (test 0))
    #:literals (quote cons)
    [pattern (bin id n ...)
             #:with test #'(λ (mch)
                             (equal? id mch))
             #:with (vars ...) #'(id)]
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

(bit-match '(bin z 10)
  [(bin x n) (add1 n)])

(bit-match (bin z 10)
  [(bin x n) x])

(bit-match (bin z 10)
  [(bin x n) (begi x n)])



