#lang typed/racket

(require "lib.rkt")


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

(: make-segment (-> (Pairof Real Real) (Pairof Real Real) (Pairof (Pairof Real Real) (Pairof Real Real))))
(define (make-segment start end)
  (cons start end))

(: start-segment (-> (Pairof (Pairof Real Real) (Pairof Real Real)) (Pairof Real Real)))
(define (start-segment seg)
  (car seg))

(: end-segment (-> (Pairof (Pairof Real Real) (Pairof Real Real)) (Pairof Real Real)))
(define (end-segment seg)
  (cdr seg))

;; ex 2.4
(: consp (-> Real Real (-> (-> Real Real Real) Real)))
(define (consp x y)
  (lambda (m) (m x y)))

(: carp (-> (-> (-> Real Real Real) Real) Real))
(define (carp z)
  (z (lambda (p q) p)))

(: cdrp (-> (-> (-> Real Real Real) Real) Real))
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
;; Have to find a way to escape typed racket

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
  (lambda f (lambda (x) (f (f x)))))

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
(define (upper-bound p) (car p))

(: lower-bound (-> (Pairof Real Real) Real))
(define (lower-bound p) (cdr p))

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
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

(: sub-interval (-> (Pairof Real Real) (Pairof Real Real) (Pairof Real Real)))
(define (sub-interval p1 p2)
  (make-interval
   (min (- (lower-bound p1) (lower-bound p2))
        (- (upper-bound p2) (upper-bound p1)))
   (max (- (upper-bound p1) (lower-bound p2))
        (- (upper-bound p2) (lower-bound p1)))))

(: interval-width (-> (Pairof Real Real) Real))
(define (interval-width x)
  (- (upper-bound x) (lower-bound x)))







;; ex 2.17

(: lastpair (All (a) (->  (Listof a) (Listof a))))
(define (lastpair l)
  (cond
    [(null? l) (error "Can't take empty list")]
    [(eq? (cdr l) '()) l]
    [else (lastpair (cdr l))]))

;; ex 2.18
(: reverse-iter (All (a) (->  (Listof a) (Listof a) (Listof a))))
(define (reverse-iter l ld)
  (cond
    [(null? l) ld]
    [else (reverse-iter (cdr l) (cons (car l) ld))]))

(: reversel (All (a) (->  (Listof a) (Listof a))))
(define (reversel l)
  (reverse-iter l '()))


;; ex 2.19

(: no-more? (-> (Listof Integer) Boolean))
(define (no-more? coin-values)
  (null? coin-values))

(: except-first-denomination (-> (Listof Integer) (Listof Integer)))
(define (except-first-denomination coin-values)
  (cdr coin-values))

(: first-denomination (-> (Listof Integer) Integer))
(define (first-denomination coin-values)
  (car coin-values))

(: cc (-> Integer (Listof Integer) Integer))
(define (cc amount coin-values)
  (cond
    [(= amount 0) 1]
    [(or (< amount 0) (no-more? coin-values)) 0]
    [else
     (+ (cc amount (except-first-denomination coin-values))
        (cc (- amount (first-denomination coin-values)) coin-values))]))

(define us-coins (list 50 25 10 5 1))
(cc 100 us-coins)

;; ex 2.20
(: parity-iter (-> Integer (Listof Integer) (Listof Integer) (Listof Integer)))
(define (parity-iter n l s)
  (cond
    [(null? l) s]
    [(= 0 (remainder (- (car l) n) 2)) (parity-iter n (cdr l) (cons (car l) s))]
    [else (parity-iter n (cdr l) s)]))

(: same-parity (-> Integer Integer (Listof Integer)))
(define (same-parity n . l)
  (parity-iter n l '()))


;; ex 2.21
(: square-list (-> (Listof Number) (Listof Number)))
(define (square-list items)
  (if (null? items)
      '()
      (cons (expt (car items) 2) (cdr items))))

(: square-list1 (-> (Listof Number) (Listof Number)))
(define (square-list1 items)
  (map (lambda ([x : Number]) (expt x 2)) items))


(: for-each (All (a) (-> (-> a Any) (Listof a) Boolean)))
(define (for-each proc items)
  (if (null? items)
      true
      (begin
        (proc (car items))
        (for-each proc (cdr items)))))


;;; ex 2.22 -- 2.26 does not need coding.


;; ex 2.27
(define-type t (Listof t))

(: deep-reverse-iter (->  (Listof t) (Listof t) (Listof t)))
(define (deep-reverse-iter l ld)
  (cond
    [(null? l) ld]
    [(not (pair? (car l))) (reverse-iter (cdr l) (cons (car l) ld))]
    [else (reverse-iter (cdr l) (cons (reverse-iter (car l) '()) ld))]))

(: deep-reverse (->  (Listof t) (Listof t)))
(define (deep-reverse l)
  (reverse-iter l '()))


;; ex 2.28
(: fringe (-> (Listof t) (Listof t)))
(define (fringe l)
  (cond
    [(null? l) '()]
    [(not (pair? (car l))) (cons l (fringe (cdr l)))]
    [else (append (car l) (fringe (cdr l)))]))


;; ex 2.29
(define-type structure (U Real mobile))
(define-type branch (Pairof Real structure))
(define-type mobile (Pairof branch branch))

(: make-mobile (-> branch branch mobile))
(define (make-mobile left right)
  (cons left right))

(: make-branch (-> Real structure branch))
(define (make-branch len structure)
  (cons len structure))

(: left-branch (-> mobile branch))
(define (left-branch mobile)
  (car mobile))

(: right-branch (-> mobile branch))
(define (right-branch mobile)
  (cdr mobile))

(: branch-length (-> branch Real))
(define (branch-length branch)
  (car branch))

(: branch-structure (-> branch structure))
(define (branch-structure branch)
  (cdr branch))

(: weight (-> structure Real))
(define (weight structure)
  (cond
    [(pair? structure) (total-weight structure)]
    [else structure]))

(: total-weight (-> mobile Real))
(define (total-weight mobile)
  (+ (weight (branch-structure (left-branch mobile)))
     (weight (branch-structure (right-branch mobile)))))

(: torque (-> branch Real))
(define (torque branch)
  (* (branch-length branch) (weight (branch-structure branch))))

(: mobile-balanced? (-> mobile Boolean))
(define (mobile-balanced? mobile)
  (= (torque (left-branch mobile)) (torque (right-branch mobile))))


;; ex 2.30
(define-type tr (U Real (Listof tr)))
(define-type tre (Listof tr))
(: square-tree (-> (Listof tr) (Listof tr)))
(define (square-tree tree)
  (map (lambda ([subtree : tr])
         (if (list? subtree)
             (square-tree subtree)
             (expt subtree 2)))
       tree))


;; ex 2.31
(: tree-map (-> (-> Real Real) tre tre))
(define (tree-map proc tree)
  (cond
    [(null? tree) '()]
    [(list? (car tree)) (cons (tree-map proc (car tree)) (tree-map proc (cdr tree)))]
    [else (cons (proc (car tree)) (tree-map proc (cdr tree)))]))


;; ex 2.32
(: subsets (-> (Listof t) (Listof t)))
(define (subsets s)
  (if (null? s)
      (list '())
      (let ([rest (subsets (cdr s))])
        (append rest (map (lambda (l) (cons (car s) l)) rest)))))


;; ex 2.33
(: accumulate (All (a b) (-> (-> a b b) b (Listof a) b)))
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))


(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) '() sequence))

(define (append1 l1 l2)
  (accumulate cons l2 l1))

(: length1 (All (a) (-> (Listof a) Integer)))
(define (length1 seq)
  (accumulate (lambda (x [n : Number]) (+ 1 n)) 0 seq))


(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) (+ this-coeff (* x higher-terms)))
              0
              coefficient-sequence))



;; ex 2.34
(: count-leaves (-> (Listof t) Integer))
(define (count-leaves t)
  (accumulate
   +
   0
   (map (lambda (subtree)
          (cond
            [(null? subtree) 0]
            [(not (list? subtree)) 1]
            [else (count-leaves subtree)])) t)))


;; ex 2.36
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))


;; ex 2.37
(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (w) (dot-product w v)) m))

(define (transpose mat)
  (accumulate-n cons '()  mat))

(define (matrix-*-matrix m n)
  (let ([cols (transpose n)])
    (map (lambda (v) (matrix-*-vector cols v)) m)))


;; ex 2.39
(define (reverse-r sequence)
  (foldr (lambda (x y) (append y (list x))) '() sequence))

(define (reverse-l sequence)
  (foldr (lambda (x y) (cons x y)) '() sequence))


;; ex 2.40
(define (flatmap proc lst)
  (foldr append '() (map proc lst)))

(define (unique-pairs n)
  (flatmap (lambda (i)
             (map
              (lambda (j) (list i j))
              (range 1 i)))
           (range 1 n)))

(define (unique-triple n)
  (flatmap (lambda (i)
             (flatmap
              (lambda (j)
                (map
                 (lambda (k)
                   (list i j k))
                 (range j n)))
              (range i n)))
           (range n)))


(: triple-sum (-> Integer Integer (Listof (Listof Integer))))
(define (triple-sum s n)
  (filter (lambda ([lst : (Listof Integer)])
            (= s (apply + lst)))
          (unique-triple n)))


;; (define (safe? k positions)
;;   (foldr
;;    (lambda (x y) (and x y))
;;    #t
;;    (flatmap
;;     (lambda
;;       (pos)
;;       (map (lambda
;;              (k-pos)
;;              (not (and (= (car k-pos ) (car pos))
;;                        (= (cdr k-pos) (cdr pos))
;;                        (= (abs (- (car k-pos) (car pos))) (abs (- (cdr k-pos) (cdr pos)))))))
;;            (map (lambda (row) (cons k row) (range 1 k)))))
;;     (positions))))


;; ex 2.42
(define (safe? k positions)
  ;; (printf "~a~n" positions)
  (if (<= k 1)
      #t
      (let ([new-queen (car positions)]
            [rest-of-queens (cdr positions)])
        (foldr
         (lambda (x y) (and x y))
         #t
         (map (lambda (queen)
                (not (or (= (car new-queen) (car queen))
                         (= (cdr new-queen) (cdr queen))
                         (= (abs (- (car new-queen) (car queen))) (abs (- (cdr new-queen) (cdr queen)))))))
              rest-of-queens)))))

(define (adjoin-position new-row k rest-of-queens)
  (cons (cons new-row k) rest-of-queens))

(define empty-board '())

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (range 1 (+ 1 board-size))))
          (queen-cols (- k 1))))))
  (queen-cols board-size))


(require sicp-pict)
;; ex 2.44

(define painter (paint einstein))

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ([smaller (up-split painter (- n 1))])
        (below painter (beside smaller smaller)))))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ([up (up-split painter (- n 1))]
            [right (right-split painter (- n 1))])
        (let ([top-left (beside up up)]
              [bottom-right (below right right)]
              [corner (corner-split painter (- n 1))])
          (beside (below painter top-left)
                  (below bottom-right corner))))))


;; ex 2.45
(define (split fir sec)
  (define (split-inner painter n)
    (if (= n 0)
        painter
        (let ([smaller (split-inner painter n)])
          (fir painter (sec smaller smaller)))))
  split-inner)

(define right-split (split beside below))


;; ex 2.46
(define-type Vector (Pairof Real Real))
(: make-vect (-> Real Real Vector))
(define (make-rect x y)
  (cons x y))

(: xcor-vect (-> Vector Real))
(define (xcor-vect v)
  (car v))

(: ycor-vect (-> Vector Real))
(define (ycor-vect v)
  (cdr v))

(: add-vect (-> Vector Vector Vector))
(define (add-vect v1 v2)
  (make-rect (+ (xcor-vect v1) (xcor-vect v2))
             (+ (ycor-vect v1) (ycor-vect v2))))

(: sub-vect (-> Vector Vector Vector))
(define (sub-vect v1 v2)
  (make-rect (+ (xcor-vect v1) (xcor-vect v2))
             (+ (ycor-vect v1) (ycor-vect v2))))

(: scale-vect (-> Real Vector Vector))
(define (scale-vect scale v)
  (make-vect (* scale (xcor-vect v))
             (* scale (ycor-vect v))))


;; ex 2.47
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin-frame frame)
  (car frame))

(define (edge1-frame frame)
  (cadr frame))

(define (edge2-frame frame)
  (caddr frame))

;; (define (make-frame origin edge1 edge2)
;;   (cons origin (cons edge1 edge2)))

;; (define (origin-frame frame)
;;   (car frame))

;; (define (edge1-frame frame)
;;   (cadr frame))

;; (define (edge2-frame frame)
;;   (cddr frame))


;; ex 2.48
; Done in 2.3.
;; (define (make-segment start end)
;;   (cons start end))

;; (define (start-segment seg)
;;   (car seg))

;; (define (end-segment seg)
;;   (cdr seg))


;; ex 2.49
(define (frame-painter)
  (let ([tl (make-vect 0 1)]
        [tr (make-vect 1 1)]
        [bl (make-vect 0 0)]
        [br (make-vect 1 0)])
    (segments->painter (list (make-segment bl br)
                             (make-segment br tr)
                             (make-segment tr tl)
                             (make-segment tl bl)))))

(define (cross-painter)
  (let ([tl (make-vect 0 1)]
        [tr (make-vect 1 1)]
        [bl (make-vect 0 0)]
        [br (make-vect 1 0)])
    (segments->painter (list (make-segment bl tr)
                             (make-segment br tl)))))

(define (diamond-painter)
  (let ([left (make-vect 0 0.5)]
        [top (make-vect 0.5 1)]
        [right (make-vect 1 0.5)]
        [bottom (make-vect 0.5 0)])
    (segments->painter (list (make-segment left top)
                             (make-segment top right)
                             (make-segment right bottom)
                             (make-segment bottom left)))))

;; ex 2.50
(define (flip-horiz painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

(define (rotate180 painter)
  (transform-painter painter
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 0.0)))

(define (rotate270 painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

