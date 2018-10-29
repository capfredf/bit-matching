#lang racket
(require (for-syntax "bm-grammar.rkt"
                     brag/support
                     syntax/parse))










;; struct MatchedBit [(val type length)]
(define x 10)
;; <<x:16>>
;; ==> (MatchedBit [(10 Nat 16)]) ;;

;; (define (str-empty? s) (string=? str ""))

(require ;br-parser-tools/lex
         brag/support)

;; A MBType is one of : Nat | String

;; A MatchedBit is a (MatchedBit Number MBType Number)
(define-struct MatchedBit [val len])

(define lex
  (lexer
   ["<<"         (token 'OPEN lexeme)]
   [">>"         (token 'CLOSE lexeme)]
   [":"          (token 'COLON lexeme)]
   [(:+ numeric) (token 'SIZE lexeme)]
   [any-char     (token 'ID lexeme)]))


(require syntax/parse)

(begin-for-syntax
  (define (to-struct stx)
  (define (rewrite x op)
    (datum->syntax stx (op (syntax->datum x))))
  (syntax-parse stx #:datum-literals (expr)
    ((expr "<<" id ":" size ">>") #`(MatchedBit #,(rewrite #'id string->symbol)
                                                #,(rewrite #'size string->number)))))
)

(module+ test
  (require rackunit)
  (check-equal? (apply-lexer lex "<<x:16>>") (list (token 'OPEN "<<")
                                                   (token 'ID "x")
                                                   (token 'COLON ":")
                                                   (token 'SIZE "16")
                                                   (token 'CLOSE ">>")))
  (check-equal? (to-struct
                 (parse (list (token 'OPEN "<<")
                       (token 'ID "x")
                       (token 'COLON ":")
                       (token 'SIZE "16")
                       (token 'CLOSE ">>"))))

                (MatchedBit 10 16)))