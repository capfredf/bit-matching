#lang racket
(provide (all-from-out racket))

(module reader racket
  (require brag/support)
  (require (for-syntax syntax/parse))

  (provide (rename-out (my-read read)))
  (provide (rename-out (my-read-syntax read-syntax)))
  (require "bm-grammar.rkt")
  (provide expr bin ->byte arg-expr to-bin program num identifier)

  (define lex
    (lexer
     [(:or "\n" " ") (lex input-port)]
     ["<<"         (token 'OPEN lexeme)]
     [">>"         (token 'CLOSE lexeme)]
     [","         (token 'SEP lexeme)]
     #;[(:: (:+ any-char) ":" (:+ any-char)) (token 'Expr lexeme)]
     ;; [(from/to " " " ") (token 'EXPR lexeme)]
     [":"          (token 'COLON lexeme)]
     ["+" (token 'OP #'+)]
     ["define" (token 'OP 'define)]
     ["(" (token 'LPAREN lexeme)]
     [")" (token 'RPAREN lexeme)]
     [(:+ numeric) (token 'NUM (string->number lexeme))]
     [(:+ (char-range #\A #\~)) (token 'ID lexeme)]
     ))

  #;(parse (apply-lexer lex (open-input-string "<<(+ 20 20):20 y:20>>")))
  #;(apply-lexer lex (open-input-string "<<(+ 20 10):20 30:40>>"))
  #;(apply-lexer lex (open-input-string "<<(+ 20 10):20 30:40>>"))
  ;; (parse (apply-lexer lex (open-input-string "<<(+ 20 10):30, 40:30>>")))
  ;; (apply-lexer lex (open-input-string "(+ x 20)"))
  ;; (parse (apply-lexer lex (open-input-string "(define x 20)")))
  (define (tokenize ip)
    (port-count-lines! ip)
    (apply-lexer lex ip))

  (define (my-read port)
    `(module mod bit-matching ,(parse (tokenize port))))

  (define (my-read-syntax source-name port)
    (let ((t (tokenize port)))
      (printf "tokens : ~a\n" t)
      (let ((p (parse t)))
        (printf "parsed : ~a\n" p)
        `(module mod bit-matching ,p))))

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

  (define ->byte (λ (xs)
                   (bytes (for/fold ([s 0])
                                    ([i xs])
                            (bitwise-and 255 (bitwise-ior (arithmetic-shift (car i) (cdr i)) s)))))))

(require 'reader)
(provide expr bin ->byte arg-expr to-bin read read-syntax program num identifier define)
;; (define x 30)
;; ;; (define y 1)
;; ;; (provide x y)

;; (expr (num 20))
;; (expr "(" (op +) (expr (num 20)) (expr (num 10)) ")")
#; (program (bin "<<" (arg-expr (expr "(" (op +) (expr (num 20)) (expr (num 10)) ")") ":" (expr (num 4))) ">>"))

;; (bin "<<" (arg-expr (expr "(" (op +) (expr (num 20)) (expr (num 10)) ")") ":" (expr (num 4))) "," (arg-expr (expr (num 5)) ":" (expr (num 4))) ">>")

;; (bin "<<" (arg-expr (expr (identifier x)) ":" (expr (num 1))) ">>")

;; (char "a")(char "b")
