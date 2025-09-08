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


(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let* ([m (frame-coord-map frame)]
           [new-origin (m origin)])
      (painter
       (make-frame new-origin
                   (sub-vect (m corner1) new-origin)
                   (sub-vect (m corner2) new-origin))))))


(provide
 count-change
 transform-painter)
