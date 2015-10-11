#lang typed/racket
;; (: count-change (-> Integer Integer))
;; (define (count-change amount)
;;   (cc amount 5))
;; (: cc (-> Integer Integer Integer))
;; (define (cc amount kinds-of-coins)
;;   (cond ((= amount 0) 1)
;;         ((or (< amount 0) (= kinds-of-coins 0)) 0)
;;         (else (+ (cc amount
;;                      (- kinds-of-coins 1))))))
;; (count-change 3)

(: fast-expt (-> Real Natural Real))
(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (sqr (fast-expt b (quotient n 2))))
        (else (* b (fast-expt b (- n 1))))))

;; ex 1.16
(: fexpt (-> Real Natural Real))
(define (fexpt b n)
  (fexpt-iter b n 1))
(: fexpt-iter (-> Real Natural Real Real))
(define (fexpt-iter b n x)
  (if (= n 0)
      x
      (fexpt-iter (sqr b) (quotient n 2) (* x (* b (remainder n 2))))))

;; ex 1.17
(: double (-> Integer Integer))
(define (double n)
  (+ n n))
(: halve (-> Integer Integer))
(define (halve n)
  (quotient n 2))
(: fast-mul (-> Integer Integer Integer))
(define (fast-mul a b)
  (cond ((= b 0) a)
        ((even? b) (double (fast-mul a (halve b))))
        (else (+ a (fast-mul a (- 1 b))))))

;; ex 1.18
(: fmul (-> Integer Integer Integer))
(define (fmul a b)
  (fmul-iter a b 1))

(: fmul-iter (-> Integer Integer Integer Integer))
(define (fmul-iter a b x)
  (if (= b 0)
      x
      (fmul-iter (double a) (halve b) (+ x (* a (remainder b 2))))))

;; ex 1.19
(: fib (-> Natural Natural))
(define (fib n)
  (fib-iter 1 0 0 1 n))

(: fib-iter (-> Natural Natural Natural Natural Natural Natural))
(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (* p p) (* q q))
                   (+ (* q q) (* 2 p q))
                   (quotient count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))


;; ex 1.20


;; ex 1.21
(: smallest-divisor (-> Positive-Integer Positive-Integer))
(define (smallest-divisor n)
  (find-divisor n 2))

(: find-divisor (-> Positive-Integer Positive-Integer Positive-Integer))
(define (find-divisor n test-divisor)
  (cond ((> (sqr test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(: divides? (-> Positive-Integer Positive-Integer Boolean))
(define (divides? a b)
  (= (remainder b a) 0))

;; ex 1.22
;; (: timed-prime-test (-> Positive-Integer))
;; (define (timed-prime-test n)
;;   (newline)
;;   (display n)
;;   (start-prime-test n (current-process-milliseconds)))
