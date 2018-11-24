#lang racket

(require rackunit
         (for-syntax syntax/parse))

(begin-for-syntax
  (define-syntax-class pat
    #:attributes ((vars 1) (test 0))
    #:literals (quote cons)
    [pattern (bin (id n) ...)
             #:with test #'(λ (mch)
                             (and (= (length '(id ...))
                                     (length (cdr mch)))
                                  (map car (cdr mch)))) ;; bit equal?
             #:with (vars ...) #'(id ...)]
    ))

(define-syntax (bit-match stx)
  (syntax-parse stx
    [(_ e [p:pat rhs] ...)
     #'(let ([mch e])
         (cond
           [(p.test mch) => (λ (result)
                              (let-values
                                  ([(p.vars ...)
                                    (apply values result)])
                                rhs))]
           ...
           [else (error 'bit-match "no matching pattern for ~v" e)]))]))


(define (extract bv lens)
  ;; TODO check if the sum of lens are equal to 8 * size of bv
  (let loop ([not-consumed 8]
             [curr-val #f]
             [idx 0]
             [curr-res 0]
             [acc '()]
             [lens lens])
    (if (empty? lens) (reverse acc)
        (let ([curr-len (car lens)]
              [curr-val (or curr-val (bytes-ref bv idx))])
          (let* ([consumed (if (< not-consumed curr-len) not-consumed curr-len)]
                 [move (- not-consumed consumed)]
                 [mask (arithmetic-shift (sub1 (expt 2 consumed)) move)]
                 [res (bitwise-ior (arithmetic-shift curr-res curr-len)
                                   (arithmetic-shift (bitwise-and curr-val mask) (- move)))]
                 [not-consumed^ (- not-consumed consumed)])
            (let-values ([(res^ acc^ lens^) (if (equal? consumed curr-len)
                                                (values 0 (cons res acc)
                                                        (cdr lens))
                                                (values res
                                                        acc
                                                        (cons (- curr-len consumed) (cdr lens))))])
              (if (zero? not-consumed^)
                  (loop 8 #f (add1 idx) res^ acc^
                        lens^)
                  (loop not-consumed^ (bitwise-and (bitwise-not mask) curr-val)
                        idx res^ acc^
                        lens^))))))))

(check-equal? (extract (bytes 16) '(3 5)) '(0 16))
(check-equal? (extract (bytes 16) '(4 4)) '(1 0))
(check-equal? (extract (bytes 17) '(4 4)) '(1 1))
(check-equal? (extract (bytes 1 16) '(4 4 8)) '(0 1 16))

;; (bytes 4 16) ==> 0000 0001 0001 0000
;; 4 9 3 ==> 0000 0001000010 000
;; 4 4 5 3 ==> 0000 0001 000010 000
(check-equal? (extract (bytes 1 16) '(4 9 3)) '(0 34 0))
(check-equal? (extract (bytes 1 16) '(4 4 5 3)) '(0 1 2 0))
;; 9
;; len 1
;; len 8
#;
(extract (bytes 1 16) '(4 4 8)) ;; 0000 0001 00010000 => 0 1 16
;; 00010000 00000001
;; 0001 000 000000001
;; 0001 => not-consumed 4 ; acc '(1) ; idx 0
;; 000 => not-consumed 1 ; acc '(0 1) ; idx 0
;; 0  => not-consumed 0 ; acc '(0 0 1) ; idx 1
;; 0000000 => not-consumed 0 ; acc '(1 0 1) ; idx 2

#;
(let-values ([(x y z) (extract (bytes 16 1) 4 3 9)])
  (begin
    (check-equal? x 1)
    (check-equal? y 0)))

;(define-values (x y z) (10 10 10))
;
;(bit-match (bin x 4 y 2 z 20)
;           [(bin e1 n1 e2 n2 e3 n3) (+ e1 e2 e3)])
#;
(bit-match (bytes 16)
           ((bin (x 4) (y 4)) (+ x y))) ; ==> 1


;; (bit-match (bytes 40 10)
;;            ((bin (x 8) (y 8)) (+ x y))) ;; ==> 50



;; (bit-match '(bin (40 10) (200 60))
;;   [(bin (y n)) y]
;;   [(bin (y n) (z m)) (+ y z)])

;; (check-equal? (bit-match '(bin (40 10))
;;                          [(bin (y n)) y])
;;               40)



;
;(bit-match '(bin 40 10)
;  [(bin x n) n])


;(define z 10)
;
;(bit-match `(bin ,z 10)
;  [(bin x n) (add1 n)])
;
;(bit-match `(bin ,z 10)
;  [(bin x n) x])
;
;(bit-match `(bin ,z 10)
;  [(bin x n) (begi x n)])
