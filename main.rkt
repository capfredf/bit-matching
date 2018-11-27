#lang racket
(provide (all-from-out racket))

(module reader racket
  (require brag/support)
  (require (for-syntax syntax/parse))
  (provide (rename-out (my-read read)))
  (provide (rename-out (my-read-syntax read-syntax)))
  (require "bm-grammar.rkt")
  (require "match.rkt")

  (provide ->bytes bit-match)

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
    `(module mod bit-matching ,@(my-read-syntax port)))

  (define (my-read-syntax source-name port)
    (let ([x (let loop ()
               (let ([res (read-syntax source-name port)])
                 (if (eof-object? res) '()
                     (cons res
                           (loop)))))])
      `(module mod bit-matching ,@x))
    #;
    (let ((t (tokenize port)))
      (printf "tokens : ~a\n" t)
      (let ((p (parse t)))
        (printf "parsed : ~a\n" p)
        `(module mod bit-matching ,p)))))

(require 'reader)
(provide ->bytes bit-match)
;(provide expr bin ->byte arg-expr to-bin read read-syntax program num identifier define)
;; (define x 30)
;; ;; (define y 1)
;; ;; (provide x y)

;; (expr (num 20))
;; (expr "(" (op +) (expr (num 20)) (expr (num 10)) ")")
#; (program (bin "<<" (arg-expr (expr "(" (op +) (expr (num 20)) (expr (num 10)) ")") ":" (expr (num 4))) ">>"))

;; (bin "<<" (arg-expr (expr "(" (op +) (expr (num 20)) (expr (num 10)) ")") ":" (expr (num 4))) "," (arg-expr (expr (num 5)) ":" (expr (num 4))) ">>")

;; (bin "<<" (arg-expr (expr (identifier x)) ":" (expr (num 1))) ">>")

;; (char "a")(char "b")
