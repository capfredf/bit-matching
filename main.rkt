#lang racket
(provide (all-from-out racket)
         ->bytes bit-match bin)

(module reader racket

  (require (for-syntax syntax/parse)
           "match.rkt" syntax/readerr
           syntax/strip-context)

  (provide (rename-out (my-read read)
                       (my-read-syntax read-syntax))
           bit-match ->bytes bin)

  (define rt (make-readtable (current-readtable)
                             #\> 'terminating-macro
                             (λ args
                               (error "no opening <"))
                             #\< 'terminating-macro
                             (λ (ch p source lineno colno pos)
                               (port-count-lines! p)
                               (define pk (peek-char p))
                               (when (equal? pk #\>)
                                 (error "no content"))
                               (define res
                                 (let loop [(acc '())
                                            (i 0)]
                                   (if (equal? (peek-char p) #\>)
                                       (begin
                                         (read-char p)
                                         `(bin ,@(reverse acc)))
                                       (let*-values ([(v) (read p)]
                                                     [(position) (file-position p)]
                                                     [(line column _) (port-next-location p)])
                                         (match v
                                           [`(,a ,d) (loop (cons `(,a ,d) acc) (add1 i))]
                                           [else (raise-read-error (format "expected a pair of two things, got: ~v" v)
                                                                   source line column position 1)])))))
                               res)))

  (define (my-read a)
    (parameterize ([current-readtable rt])
      `(module anything bit-matching ,(read a))))
  
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
