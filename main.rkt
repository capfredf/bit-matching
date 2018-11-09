#lang racket
(provide (all-from-out racket))

(module reader racket
  (require brag/support)
  (require (for-syntax syntax/parse))

  (provide (rename-out (my-read read)))
  (provide (rename-out (my-read-syntax read-syntax)))
  (require "bm-grammar.rkt")
  (provide bin id size to-bin ->byte)

  (define lex
    (lexer
     [(:or "\n" " ") (lex input-port)]
     ["<<"         (token 'OPEN lexeme)]
     [">>"         (token 'CLOSE lexeme)]
     #;[(:: (:+ any-char) ":" (:+ any-char)) (token 'Expr lexeme)]
     [":"          (token 'COLON lexeme)]
     ["("          "("]
     [")"          ")"]
     ["+"          "+"]
     [(:+ numeric) (token 'NUMBER lexeme)]
     [any-char lexeme]
     ))

  #;(parse (apply-lexer lex (open-input-string "<<(+ 20 20):20 y:20>>")))
  #;(apply-lexer lex (open-input-string "<<(+ 20 10):20 30:40>>"))

  (parse (apply-lexer lex (open-input-string "<<(+ 20 10):20 30:40>>")))
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

  (define-syntax (id stx)
    (syntax-parse stx
      [(_ id) (datum->syntax stx (string->symbol (syntax->datum #'id)))]))

  (define-syntax (size stx)
    (syntax-parse stx
      [(_ size) (datum->syntax stx (string->number (syntax->datum #'size)))]))

  (define-syntax (to-bin stx)
    (syntax-parse stx
      [(_ acc)
       #'(->byte acc)]

      [(_ acc id ":" size r ...)
       #'(let ([ret (cons (cons id size) acc)])
           (to-bin ret r ...))]))
  (define-syntax (bin stx)
    (syntax-parse stx
      [(bin "<<" e ... ">>")
       #'(to-bin '()  e ...)]))

  (define (->byte xs)
    xs
    #;
    (apply bytes (for/fold ([r '()]
                            [s 0])
                           ([i xs])
                   (make-byte (bitwise-ior (arithmetic-shift (car i) (cdr i)) r))))))

(require 'reader)
(provide bin id size to-bin ->byte)
(define x 0)
(define y 1)
(provide x y)
