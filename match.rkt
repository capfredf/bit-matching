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

(define (extract n li)
  (for/fold ([not-consumed 8]
             [acc '()]
             (idx 0)
             #:result (reverse acc))
            ([len li])
    (let* ([not-consumed^ (max (- not-consumed len) 0)]
           [val (bytes-ref n idx)]
           [mask (arithmetic-shift (- (expt 2 len) 1) not-consumed^)])

      (values (if (zero? not-consumed^) 8
                  not-consumed^)
              (cons (arithmetic-shift (bitwise-and val mask) (- not-consumed^))
                    acc)
              (if (equal? not-consumed^ 0) (add1 idx)
                  idx)))))


(define (extract2 bv lens)
  (let loop ([not-consumed 8]
             [idx 0]
             [val (bytes-ref bv 0)]
             [acc '()]
             [tmp 0]
             [lens lens])
    (if (empty? lens) (reverse acc)
        (let* ([move (if (< not-consumed (car lens)) not-consumed
                         (- not-consumed (car lens)))]
               [mask (arithmetic-shift (sub1 (expt 2 move)) move)]
               [next (arithmetic-shift (bitwise-and val mask) (- move))])
          (pretty-print (list idx next val mask (car lens) move (- not-consumed move) tmp))
          (if (equal? (car lens) (- not-consumed move))
              (loop (if (equal? not-consumed move) 8 move)
                    (if (equal? not-consumed move) (add1 idx) idx)
                    (if (and (equal? not-consumed move)
                             (< (add1 idx ) (bytes-length bv)))
                        (bytes-ref bv (add1 idx))
                        (bitwise-and val (bitwise-not mask)))
                    (append (list (bitwise-ior next (arithmetic-shift tmp (car lens)))) acc)
                    0
                    (cdr lens))
              (loop (if (equal? not-consumed move) 8 move)
                    (if (equal? not-consumed move) (add1 idx) idx)
                    (if (and (equal? not-consumed move)
                             (< (add1 idx) (bytes-length bv)))
                        (bytes-ref bv (add1 idx))
                        (bitwise-and val (bitwise-not mask)))
                    acc
                    next
                    (cons (- (car lens) move) (cdr lens))))))))

;; not-consume 4
;; 1
;; 0
;; 8
;;
#;
(extract2 (bytes 16) '(4 4))
(extract2 (bytes 16 129) '(4 3 9)) ;; 1 0 1
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
