#lang typed/racket

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
        ((divides? n test-divisor) test-divisor)
        (else (find-divisor n (next-divisor test-divisor)))))
(: next-divisor (-> Positive-Integer Positive-Integer))
;; ex 1.23
(define (next-divisor n)
  (if (= n 2) 3
      (+ 2 n)))
(: divides? (-> Positive-Integer Positive-Integer Boolean))
(define (divides? a b) (= (remainder a b) 0))
(: prime? (-> Positive-Integer Boolean))
(define (prime? n)
  (= n (smallest-divisor n)))

;; ex 1.22
(: timed-prime-test (-> Positive-Integer Void))
(define (timed-prime-test n)
  (start-prime-test n (current-process-milliseconds)))

(: start-prime-test (-> Positive-Integer Fixnum Void))
(define (start-prime-test n start-time)
  (when (fast-prime? n 12)
    (newline)
    (display n)
    (report-prime (- (current-process-milliseconds) start-time))))
(: report-prime (-> Integer Void))
(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(: search-for-primes (-> Positive-Integer Positive-Integer Void))
(define (search-for-primes start end)
  (search-iter start end))
(: search-iter (-> Positive-Integer Positive-Integer Void))
(define (search-iter cur last)
  (when (<= cur last) (timed-prime-test cur))
  (when (<= cur last) (search-iter (+ cur 2) last)))

;; ex 1.24
(: expmod (-> Positive-Integer Natural Positive-Integer Natural))
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder
          (sqr (expmod base (quotient exp 2) m))
          m))
        (else
         (remainder
          (* base (expmod base (- exp 1) m))
          m))))

(: fermat-test (-> Positive-Integer Boolean))
(define (fermat-test n)
  (: try-it (-> Positive-Integer Boolean))
  (define (try-it a)
;;; NOTE: notice the solving of difficulty of error handling here.
    (= (expmod a (- n 1) n) 1))
  (try-it (+ 1 (random (- n 1)))))

(: fast-prime? (-> Positive-Integer Natural Boolean))
(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

;; ex 1.28
(: mmod (-> Positive-Integer Natural Positive-Integer Natural))
(define (mmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (let ((a (remainder (sqr (expmod base (quotient exp 2) m)) m)))
           (if (= 1 a) 0 a)))
        (else
         (remainder
          (* base (expmod base (- exp 1) m))
          m))))


;; ex 1.29
(: sum (-> (-> Integer Real) Integer (-> Integer Integer) Integer Real))
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(: sintegral (-> (-> Real Real) Integer Integer Natural Real))
(define (sintegral f a b n)
  (: next (-> Integer Integer))
  (define (next n) (+ n 1))
  (: term (-> Integer Real))
  (define (term x)
    (*  (f (+ a (/ (* x (- b a)) n)))
        (if (or (= x 0) (= x n))
            1
            (* 2 (+ 1 (remainder x 2))))))
  (* (/ (- b a) (* 3 n)) (sum term 0 next n)))


;; ex 1.30
(: nsum (-> (-> Real Real) Integer (-> Integer Integer) Integer Real))
(define (nsum term a next b)
  (: iter (-> Integer Real Real))
  (define (iter a result)
    (if (> a b) result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

;; ex 1.31
(: product (-> (-> Real Real) Integer (-> Integer Integer) Integer Real))
(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

(: factorial (-> Natural Real))
(define (factorial n)
  (product (lambda (a) a) 1 (lambda (a) (+ 1 a)) n))

(: pro-pi (-> Positive-Integer Real))
(define (pro-pi n)
  (/ (product (lambda (a) (/ (* (- a 1) (+ a 1)) (* a a))) 3 (lambda (a) (+ 2 a)) n) 4))

(: npro (-> (-> Real Real) Integer (-> Integer Integer) Integer Real))
(define (npro term a next b)
  (: iter (-> Integer Real Real))
  (define (iter a result)
    (if (> a b) result
        (* result (term a))))
  (iter a 1))

;; ex 1.32
(: accu (-> (-> Real Real Real) Real (-> Real Real) Integer (-> Integer Integer) Integer Real))
(define (accu combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
       (accu combiner null-value term (next a) next b))))

(: asum (-> (-> Real Real) Integer (-> Integer Integer) Integer Real))
(define (asum term a next b)
  (accu + 0 term a next b))

(: apro (-> (-> Real Real) Integer (-> Integer Integer) Integer Real))
(define (apro term a next b)
  (accu * 1 term a next b))

(: iaccu (-> (-> Real Real Real) Real (-> Real Real) Integer (-> Integer Integer) Integer Real))
(define (iaccu combiner null-value term a next b)
  (: iter (-> Integer Real Real))
  (define (iter x result)
    (if (> x b) result
        (iter (next x) (combiner result (term x)))))
  (iter a null-value))

;; (: accu-filter (-> (-> Positive-Integer Boolean) (-> Real Real Real) Real (-> Real Real) Integer (-> Integer Integer) Integer Real))
;; (define (accu-filter predicate combiner null-value term a next b)
;;   (cond ((> a b) null-value)
;;         ((predicate (term a)) (combiner (term a)
;;                                         (accu-filter predicate combiner null-value term (next a) next b)))
;;         (else (accu-filter predicate combiner null-value term (next a) next b))))

;; ;; (: prime-sum (-> Natural Real))
;; (define (prime-sum n)
;;   (accu-filter prime? + 0 + 2 (lambda (a) (+ 1 a)) n))

;; ex 1.35

(define tolerance 0.00001)

(: fixed-point (-> (-> Real Real) Real Real))
(define (fixed-point f first-guess)
  (: close-enough? (-> Real Real Boolean))
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (: try (-> Real Real))
  (define (try guess)
    (let ((next (f guess)))
      (displayln guess)
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))


;; (fixed-point (lambda ([x : Real]) (+ 1 (/ 1 x))) 1.0)

;; ex 1.36
(: uln (-> Positive-Real Positive-Real))
(define (uln x) (assert (/ (log 1000) (log x)) positive?))
(: ull (-> Positive-Real Positive-Real))
(define (ull x) (assert (/ (+ x (/ (log 1000) (log x))) 2) positive?))
;; ;; (fixed-point ull 2.0)

;; ex 1.37
(: cont-frac (-> (-> Integer Real) (-> Integer Real) Index Real))
(define (cont-frac n d k)
  (: iter (-> Natural Real))
  (define (iter x)
    (if (= x k)
        (/ (n x) (d x))
        (/ (n x) (+ (d x) (iter (+ x 1))))))
  (iter 1))

(: icont-frac (-> (-> Integer Real) (-> Integer Real) Index Real))
(define (icont-frac n d k)
  (: iter (-> Natural Real Real))
  (define (iter x result)
    (if (= x 0) result
        (iter (- x 1) (/ (d x) (+ (n x) result)))))
  (iter k 0))

;; ex 1.38
(: expc (-> Index Real))
(define (expc k)
  (: do (-> Integer Integer))
  (define (do n)
    (if (= (remainder n 3) 2) (* 2 (+ 1 (quotient n 3))) 1))
  (cont-frac (lambda (x) 1.0) do k))

;; ex 1.39
(: tan-cf (-> Real Index Real))
(define (tan-cf x k)
  (cont-frac (lambda (n) (- (+ 1 (* 2 n)))) (lambda (n) (expt x n)) k))


;; ex 1.40

(: deriv (-> (-> Real Real) (-> Real Real)))
(define (deriv g)
  (define dx 0.00001)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(: newton-transform (-> (-> Real Real) (-> Real Real)))
(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(: newton-method (-> (-> Real Real) Real Real))
(define (newton-method g guess)
  (fixed-point (newton-transform g) guess))


(: cubic (-> Integer Integer Integer (-> Real Real)))
(define (cubic a b c)
  (lambda (x) (+ (*  x x x) (* a x x) (* b x) c)))
;; (newton-method (cubic 3 2 1) 1)

;; ex 1.41
;; (define-type NumFunc (Rec NF (U (-> NF NF) (-> Number Number))))

(: doub (All (a) (-> (-> a a) (-> a a))))
(define (doub f)
  (lambda (x) (f (f x))))

(: inc (-> Number Number))
(define (inc x) (+ 1 x))
;;; Hack due to the limit of Typed Racket.
;; (((doub (doub (inst doub Number))) inc) 5)

;; ex 1.42

(: comps (All (a b c) (-> (-> b c) (-> a b) (-> a c))))
(define (comps f g)
  (lambda (x)  (f (g x))))

;; ex 1.43
(: repeated (All (a) (-> (-> a a) Integer (-> a a))))
(define (repeated f n)
  (if (= n 1)
      f
      (compose f (repeated f (- n 1)))))

;; ex 1.44

(: smooth (-> (-> Number Number) Number (-> Number Number)))
(define (smooth f dx)
  (lambda (x) (+ (f (- x dx)) (f x) (f (+ x dx)))))
(: nsmooth (-> (-> Number Number) Number Integer (-> Number Number)))
(define (nsmooth f dx n)
  (repeated (smooth f dx) n))

;; ex 1.45
(: average-damp (-> (-> Real Real) (-> Real Real)))
(define (average-damp f)
  (lambda (x) (/ (+ x (f x)) 2)))

(: nth-root (-> Integer Real Real))
(define (nth-root n x)
  (fixed-point (repeated (average-damp (lambda (y) (/ x (expt y (- n 1))))) n) 1.0))

;; ex 1.46
(: iterative-improve (-> (-> Real Boolean) (-> Real Real) (-> Real Real)))
(define (iterative-improve good-enough? improve)
  (: iter (-> Real Real))
  (define (iter y)
    (if (good-enough? y) y
        (iter (improve y))))
  (lambda (x)  (iter x)))

(: ifixed (-> (-> Real Real) Real Real))
(define (ifixed f x)
  ((iterative-improve (lambda (y) (< (abs (- y (f y))) 0.00001)) f) x))
(: fsqrt (-> Real Real))
(define (fsqrt x)
  ((iterative-improve (lambda (y) (< (abs (- (sqr y) x)) 0.000001))
                      (lambda (y) (/ (+ y (/ 1 y)) 2)))
   x))
