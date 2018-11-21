#lang racket

#;
(424-match (cons 1 (cons 2 '()))
           [(cons x (cons y '())) (+ x y)]
           [x (car x)])

(let ([z (cons 1 (cons 2 '()))])
  (if #t
      (let ([x z]) (car x))
      (error 'match)))

(let ([z (cons 1 (cons 2 '()))])
  (cond [(and (pair? z)
              (let ([a (car z)]
                    [d (cdr z)])
                (if (pair? d)
                    (let ([aa (car d)]
                          [dd (cdr d)])
                      (and (null? dd)
                           (list a aa)))
                    #f)))
         => (λ (result) (let ([x (first result)] [y (second result)]) (+ x y)))]
        [(list z)
         => (λ (result) (let ([x (first result)]) (car x)))]
        [else (error 'match)]))
(require (for-syntax syntax/parse))

(begin-for-syntax
  (define-syntax-class pat
    #:attributes ((vars 1) (test 0))
    #:literals (quote cons)
    [pattern (m args ...)
             #:when (procedure? (syntax-local-value #'m (λ () #f)))
             #:with (test (vars ...)) ((syntax-local-value #'m) #'(m args ...))]
    [pattern x:id
             #:with test #'(λ (z) (list z))
             #:with (vars ...) #'(x)]
    [pattern (quote v)
             #:with test #'(λ (z) (and (equal? 'v z)
                                       (list)))
             #:with (vars ...) #'()]
    [pattern (cons p1:pat p2:pat)
             #:with test #'(λ (z)
                             (and (pair? z)
                                  (let ([p1? (p1.test (car z))])
                                    (and p1?
                                         (let ([p2? (p2.test (cdr z))])
                                           (and p2? (append p1? p2?)))))))
             #:with (vars ...) #'(p1.vars ... p2.vars ...)]))


(define-syntax (424-match stx)
  (syntax-parse stx
    [(_ e [p:pat rhs] ...)
     #'(let ([z e])
         (cond [(p.test z) => (λ (result)
                                (let-values ([(p.vars ...)
                                              (apply values result)])
                                  rhs))]
               ...
               [else (error 'match)]))]))

(424-match 17
           ['18 18]
           ['17 17]
           [x 16])

(define-syntax (VOID stx)
  (syntax-parse stx
    [(_ x:id)
     #'((λ (z) (if (void? z) (list z) #f))  (x))]))


(424-match (cons 1 (cons 2 '()))
           [(cons x (cons y '())) (+ x y)]
           [(cons hello (cons y z))   (+ hello y)]
           [x (car x)])

(424-match (void)
           [(VOID x) (list x)]
           [_ 'no])


