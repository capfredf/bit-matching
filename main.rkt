#lang racket
(provide (all-from-out racket))


(module reader racket
  (require brag/support)

  (provide (rename-out (my-read read)))
  (provide (rename-out (my-read-syntax read-syntax)))
  (require "bm-grammar.rkt")
  (provide bin)

  (define lex
    (lexer
     ["\n" (lex input-port)]
     ["<<"         (token 'OPEN lexeme)]
     [">>"         (token 'CLOSE lexeme)]
     [":"          (token 'COLON lexeme)]
     [(:+ numeric) (token 'SIZE lexeme)]
     [any-char (token 'ID lexeme)]))

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

  (define-syntax (bin stx)
    #'42))

(require 'reader)
(provide bin)
