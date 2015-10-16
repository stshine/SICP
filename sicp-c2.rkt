#lang typed/racket

;; ex 2.1
(: number (-> (Pairof Real Real) Real))
(define (number x) (car x))

(: denom (-> (Pairof Real Real) Real))
(define (denom x) (cdr x))

(: print-rat (-> (Pairof Real Real) Void))
(define (print-rat x)
  (printf "~a / ~a ~n" (number x) (denom x)))

(: make-rat (-> Real Real (Pairof Real Real)))
(define (make-rat n d)
  (let ((g ((if (< d 0) - +) (gcd n d))))
    (cons (/ n g) (/ d g))))
