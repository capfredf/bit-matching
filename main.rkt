#lang racket
(provide (all-from-out racket))

(module reader racket
  (require brag/support)
  (require (for-syntax syntax/parse))

  (provide (rename-out (my-read read)))
  (provide (rename-out (my-read-syntax read-syntax)))
  (require "bm-grammar.rkt")
  (provide bin id size)

  (define lex
    (lexer
     ["\n" (lex input-port)]
     ["<<"         (token 'OPEN lexeme)]
     [">>"         (token 'CLOSE lexeme)]
     [":"          (token 'COLON lexeme)]
     [(:+ numeric) (token 'SIZE lexeme)]
     [any-char (token 'ID lexeme)]))

  #;(parse (apply-lexer lex (open-input-string "<<x:20>>")))
  (define (tokenize ip)
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

  (define-syntax (bin stx)
    (syntax-parse stx
      [(bin "<<" id ":" size ">>")
       #'(bytes id)])))

(require 'reader)
(provide bin id size)
(define x 42)
(provide x)
