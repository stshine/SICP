#lang typed/racket

;; ex 2.1

(: make-rat (-> Real Real (Pairof Real Real)))
(define (make-rat n d)
  (let ((g ((if (< d 0) - +) (gcd n d))))
    (cons (/ n g) (/ d g))))

(: number (-> (Pairof Real Real) Real))
(define (number x) (car x))

(: denom (-> (Pairof Real Real) Real))
(define (denom x) (cdr x))

(: print-rat (-> (Pairof Real Real) Void))
(define (print-rat x)
  (printf "~a / ~a ~n" (number x) (denom x)))


;; ex 2.2
(: make-point (-> Real Real (Pairof Real Real)))
(define (make-point x y)
  (cons x y))

(: x-point (-> (Pairof Real Real) Real))
(define (x-point p)
  (car p))

(: y-point (-> (Pairof Real Real) Real))
(define (y-point p)
  (cdr p))

(: print-point (-> (Pairof Real Real) Void))
(define (print-point p)
  (printf "(~a , ~a ~n)" (x-point p) (y-point p)))

(: make-segment (-> (Pairof Real Real) (Pairof Real Real) (Pairof (Pairof Real Real) (Pairof Real Real))))
(define (make-segment start end)
  (cons start end))

(: start-segment (-> (Pairof (Pairof Real Real) (Pairof Real Real)) (Pairof Real Real)))
(define (start-segment seg)
  (car seg))

(: end-segment (-> (Pairof (Pairof Real Real) (Pairof Real Real)) (Pairof Real Real)))
(define (end-segment seg)
  (cdr seg))


;; ex 2.3

(define-type Rectangle (Pairof (Pairof Real Real) (Pairof Real Real)))

(: make-rect (-> (Pairof Real Real) (Pairof Real Real) Rectangle))
(define (make-rect p1 p2)
  (cons p1 p2))

(: rect-p1 (-> Rectangle (Pairof Real Real)))
(define (rect-p1 r)
  (car r))

(: rect-p2 (-> Rectangle (Pairof Real Real)))
(define (rect-p2 r)
  (cdr r))

(: rect-length (-> Rectangle Real))
(define (rect-length r)
  (abs (- (y-point (rect-p1 r)) (y-point (rect-p2 r)))))

(: rect-width (-> Rectangle Real))
(define (rect-width r)
  (abs (- (x-point (rect-p1 r)) (x-point (rect-p2 r)))))

(: rect-peri (-> Rectangle Real))
(define (rect-peri r)
  (* 2 (+ (rect-width r) (rect-length r))))

(: rect-area (-> Rectangle Real))
(define (rect-area r)
  (* (rect-width r) (rect-length r)))


;; ex 2.4

;; (define-type (Cons A B) (-> (-> A B (U A B)) (U A B)))

(: consp (All (A B) (-> A B (-> (-> A B (U A B)) (U A B)))))
(define (consp x y)
  (lambda (m) (m x y)))

(: carp (All (A B) (-> (-> (-> A B (U A B)) A) A)))
(define (carp z)
  (z (lambda (p q) p)))

(: cdrp (All (A B) (-> (-> (-> A B (U A B)) B) B)))
(define (cdrp z)
  (z (lambda (p q) q)))

;; ex 2.5
(: conse (-> Integer Integer Positive-Exact-Rational))
(define (conse x y)
  (* (expt 2 x) (expt 3 y)))

(: care-p (-> Integer Integer Integer Integer))
(define (care-p z b n)
  (if (= z 0)
      n
      (care-p (quotient z b) b (+ 1 n))))

(: care (-> Integer Integer))
(define (care z)
  (care-p z 2 0))

(: cdre (-> Integer Integer))
(define (cdre z)
  (care-p z 3 0))


;; ex 2.6

(define-type fnumber (All (a) (-> (-> a a) (-> a a))))

(: zero fnumber)
(define zero (lambda (f) (lambda (x) x)))
;; (define (zero f) (lambda (x) x))

(: add-1 (-> fnumber fnumber))
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(: one fnumber)
;; (: one (All (a) (-> (-> a a) (-> a a))))
(define one
  (lambda (f) (lambda (x) (f x))))

(: two fnumber)
(define two
  (lambda (f) (lambda (x) (f (f x)))))

(: zero? (-> fnumber Boolean))
(define (zero? n)
  (eq? 1 ((n (lambda (x) x)) 1)))

(: plus (-> fnumber fnumber fnumber))
(define (plus a b)
  (lambda (f)
    (lambda (x) ((a f) ((b f) x)))))


;; ex 2.7
(: make-interval (-> Real Real (Pairof Real Real)))
(define (make-interval a b) (cons a b))

(: upper-bound (-> (Pairof Real Real) Real))
(define (upper-bound p)
  (let ([a (car p)]
        [b (cdr p)])
    (if (< a b) b a)))

(: lower-bound (-> (Pairof Real Real) Real))
(define (lower-bound p)
  (let ([a (car p)]
        [b (cdr p)])
    (if (< a b) a b)))

(: add-interval (-> (Pairof Real Real) (Pairof Real Real) (Pairof Real Real)))
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(: mul-interval (-> (Pairof Real Real) (Pairof Real Real) (Pairof Real Real)))
(define (mul-interval x y)
  (let ([p1 (* (lower-bound x) (lower-bound y))]
        [p2 (* (lower-bound x) (upper-bound y))]
        [p3 (* (upper-bound x) (lower-bound y))]
        [p4 (* (upper-bound x) (upper-bound y))])
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(: div-interval (-> (Pairof Real Real) (Pairof Real Real) (Pairof Real Real)))
(define (div-interval x y)
  (if (< (* (lower-bound y) (upper-bound y) 0))
      (error "Interval crossing 0")
      (mul-interval x
                    (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y))))))


;; ex 2.8

(: sub-interval (-> (Pairof Real Real) (Pairof Real Real) (Pairof Real Real)))
(define (sub-interval p1 p2)
  (make-interval
   (min (- (lower-bound p1) (lower-bound p2))
        (- (upper-bound p2) (upper-bound p1)))
   (max (- (upper-bound p1) (lower-bound p2))
        (- (upper-bound p2) (lower-bound p1)))))


;; ex 2.9

(: interval-width (-> (Pairof Real Real) Real))
(define (interval-width x)
  (/ (- (upper-bound x) (lower-bound x)) 2))


;; ex 2.10

;; ex 2.11
(: mul-interval1 (-> (Pairof Real Real) (Pairof Real Real) (Pairof Real Real)))
(define (mul-interval1 x y)
  (let ([xl (lower-bound x)]
        [xu (upper-bound x)]
        [yl (lower-bound x)]
        [yu (upper-bound x)])
    (cond
      [(and (> xl 0) (> xu 0) (> yl 0) (> yu 0)) (make-interval (* xl yl) (* xu yu))]
      [(and (< xl 0) (< xu 0) (< yl 0) (< yu 0)) (make-interval (* xu yu) (* xl yl))]
      [(and (< xl 0) (> xu 0) (> yl 0) (> yu 0)) (make-interval (* xl yu) (* xu yu))]
      [(and (< xl 0) (< xu 0) (> yl 0) (> yu 0)) (make-interval (* xl yu) (* xu yl))]
      [(and (> xl 0) (> xu 0) (< yl 0) (> yu 0)) (make-interval (* xu yl) (* xu yu))]
      [(and (> xl 0) (> xu 0) (< yl 0) (< yu 0)) (make-interval (* xu yl) (* xl yu))]
      [(and (< xl 0) (> xu 0) (< yl 0) (> yu 0)) (make-interval (min (* xl yu) (* xu yl)) (max (* xl yl) (* xu yu)))]
      [(and (< xl 0) (> xu 0) (< yl 0) (< yu 0)) (make-interval (* xu yl) (* xl yl))]
      [(and (< xl 0) (< xu 0) (< yl 0) (> yu 0)) (make-interval (* xl yu) (* xu yu))]
      ;; Not need, to make typed racket happy
      [else (make-interval (* xl yl) (* xu yu))])))


;; ex 2.12

(: make-center-percent (-> Real Real (Pairof Real Real)))
(define (make-center-percent c p)
  (make-interval (- c (* c (/ p 100)))
                 (+ c (* c (/ p 100)))))

(: center (-> (Pairof Real Real) Real))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(: percent (-> (Pairof Real Real) Real))
(define (percent i)
  (/ (interval-width i) (center i)))


;; ex 2.13
;; (define (percent-mul x y)
  ;; (make-interval (* (- center (* percent )))))

