#lang racket
(require brag/support)
#;(require (submod "main.rkt" reader))

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

(require "bm-grammar.rkt")
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

  (define ->byte (λ (xs)
                   (bytes (for/fold ([s 0])
                                    ([i xs])
                            (bitwise-and 255 (bitwise-ior (arithmetic-shift (car i) (cdr i)) s))))))
#;
(define rt (make-readtable (current-readtable)
                           #\> 'terminating-macro
                           (λ (ch p . args)
                             (error "no opening <"))
                           #\< 'terminating-macro
                           (λ (ch p . args)
                             (define pk (peek-char p))
                             ;; (when (equal? pk #\>)
                               ;; (error "no content"))
                             (unless (equal? pk #\<)
                               (error "wrong grammar" pk))
                             (define v
                               (read p))
                             ;; (define n (read-char p))
                             ;; (unless (equal? #\> n)
                             ;;   (error "wrong char:" n))
                             (define n1 (read-char p))
                             (unless (equal? #\> n1)
                               (error "wrong char:" n1))
                             v)))

(define rt (make-readtable (current-readtable)
                           #\> 'terminating-macro
                           (λ args
                             (error "no opening <"))
                           ;; #\: 'terminating-macro
                           ;; (λ (ch p . args)
                           ;;   (pretty-print 'xxxx)
                           ;;   (define pk (peek-char p))
                           ;;   (when (equal? pk #\>)
                           ;;     (error "no content"))
                           ;;   (define v
                           ;;     (read p))
                           ;;   (define n (read-char p))
                           ;;   (unless (equal? #\> n)
                           ;;     (error "wrong char:" n))
                           ;;   v)

                           #\< 'terminating-macro
                           (λ (ch p . args)
                             (define pk (peek-char p))
                             (when (equal? pk #\>)
                               (error "no content"))
                             (define v (let loop ((seq '(#\< #\<)))
                                         (define n (read-char p))
                                         (if (equal? n #\>) (apply string (reverse (append (list #\> #\>) seq)))
                                             (loop (cons n seq)))))
                             (parse (apply-lexer lex (open-input-string v)))
                             )))

(define (my-read a)
  (parameterize ([current-readtable rt])
    `(module anything racket ,(read a))))
(require syntax/strip-context)
(define (my-read-syntax a b)
  (define v
    (parameterize ([current-readtable rt])
      (let loop ()
        (define x (read-syntax a b))
        (if (eof-object? x)
            empty
            (cons x (loop))))))
  (strip-context #`(module whatever racket #,@v)))

(provide (rename-out [my-read read] [my-read-syntax read-syntax])
         expr bin ->byte arg-expr to-bin num identifier
         )
