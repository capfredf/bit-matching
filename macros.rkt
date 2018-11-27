#lang racket

(require (for-syntax syntax/parse))

(define-syntax (identifier stx)
    (syntax-parse stx
      #;[(_ x) #'x]
      [(_ x) #`#,(datum->syntax stx (string->symbol (syntax->datum #'x)))]))
  (define-syntax (num stx)
    (syntax-parse stx
      [(_ n:number) #'n]))

  (define-syntax (expr stx)
    (syntax-parse stx #:literals (num identifier)
                  [(_ (identifier x)) #'(identifier x)]
                  [(_ (num n)) #'(num n)]
                  [(_ "(" (op +) e1 e2 ")") #'(+ e1 e2)]
                  [(_ "(" (op 'define) (identifier x:id) e2 ")") #'(define x e2)]))


  (define-syntax (arg-expr stx)
    (syntax-parse stx
      [(_ e1 ":" e2) #'(cons e1 e2)]))

  (define-syntax (to-bin stx)
    (syntax-parse stx
      [(_ acc e1)
       #'(->byte (reverse (cons e1 acc)))]

      [(_ acc e1 "," er ...)
       #'(let ([ret (cons e1 acc)])
           (to-bin ret er ...))]))

  (define-syntax (bin stx)
    (syntax-parse stx
      [(bin "<<" e ... ">>")
       #'(to-bin '() e ...)]))

  (define-syntax (program stx)
    (syntax-parse stx
      [(program p ...) #'(begin p ...)]))
