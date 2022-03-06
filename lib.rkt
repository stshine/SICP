#lang typed/racket

(: count-change (-> Integer Integer))
(define (count-change amount)
  (cc amount 5))

(: cc (-> Integer Integer Integer))
(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount
                     (- kinds-of-coins 1))))))

;; (count-change 3)

(provide count-change)
