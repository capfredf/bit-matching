#lang racket
(provide (all-from-out racket))

(module reader racket
  (require (for-syntax syntax/parse))
  (provide (rename-out (my-read read)))
  (provide (rename-out (my-read-syntax read-syntax)))
  (require "match.rkt" syntax/readerr)
  (provide bit-match ->bytes bin)



  (define rt (make-readtable (current-readtable)
                             #\> 'terminating-macro
                             (λ args
                               (error "no opening <"))
                             #\< 'terminating-macro
                             (λ (ch p source lineno colno pos)
                               (define pk (peek-char p))
                               (when (equal? pk #\>)
                                 (error "no content"))
                               (define res (let loop ((acc '())
                                                      (i 0))
                                             (if (equal? (peek-char p) #\>)
                                                 (begin
                                                   (read-char p)
                                                   `(bin ,@(reverse acc)))
                                                 (let ([v (read p)])
                                                   (match v
                                                     [`(,a ,d) (loop (cons `(,a ,d) acc) (add1 i))]
                                                     [else (raise-read-error (format "expected a pair (a d), got: ~v" v)
                                                                             source lineno colno pos #f)])))))
                               res)))

  (define (my-read a)
    (parameterize ([current-readtable rt])
      `(module anything bit-matching ,(read a))))
  (require syntax/strip-context)
  (define (my-read-syntax a b c d e f)
    (define v
      (parameterize ([current-readtable rt])
        (let loop ()
          (define x (read-syntax a b))
          (if (eof-object? x)
              empty
              (cons x (loop))))))
    (strip-context #`(module whatever bit-matching #,@v))))

(require 'reader)
(provide ->bytes bit-match bin)
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
