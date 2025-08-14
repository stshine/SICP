#lang typed/racket
;; (require typed/racket)



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

;; (define us-coins (list 50 25 10 5 1))
;; (cc 100 us-coins)


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


;; ex 2.23
(: for-each (All (a) (-> (-> a Any) (Listof a) Boolean)))
(define (for-each proc items)
  (if (null? items)
      true
      (begin
        (proc (car items))
        (for-each proc (cdr items)))))


;; ex 2.25

;; (car (cdr (car (cdr (cdr '(1 3 (5 7) 9))))))

;; (car (car '((7))))

;; (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr '(1 (2 (3 (4 (5 (6 7))))))))))))))))))


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
(define-type (tre a) (U a (tr a)))
(define-type (tr a) (Listof (tre a)))
(: square-tree (-> (tr Real) (tr Real)))
(define (square-tree tree)
  (map (lambda ([subtree : (tre Real)])
         (if (list? subtree)
             (square-tree subtree)
             (expt subtree 2)))
       tree))


;; ex 2.31
(: tree-map (-> (-> Any Any) (tr Any) (tr Any)))
(define (tree-map proc tree)
  (let ([head (car tree)])
    (cond
      [(null? tree) '()]
      [(list? head) (cons (tree-map proc head) (tree-map proc (cdr tree)))]
      [else (cons (proc head) (tree-map proc (cdr tree)))])))


;; ex 2.32
(: subsets (All (a) (-> (Listof a) (Listof (Listof a)))))
(define (subsets s)
  (if (null? s)
      (list '())
      (let ([rest (subsets (cdr s))])
        (append rest (map (lambda ([l : (Listof a)]) (cons (car s) l)) rest)))))


;; ex 2.33
(: accumulate (All (a b) (-> (-> a b b) b (Listof a) b)))
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(: map1 (All (a b) (-> (-> a b) (Listof a) (Listof b))))
(define (map1 p sequence)
  (accumulate (lambda ([x : a] [y : (Listof b)]) (cons (p x) y)) '() sequence))


(: cons1 (All (a) (-> a (Listof a) (Listof a))))
(define (cons1 x y)
  (cons x y))

(: append1 (All (a) (-> (Listof a) (Listof a) (Listof a))))
(define (append1 l1 l2)
  ;; https://docs.racket-lang.org/ts-reference/special-forms.html#%28form._%28%28lib._typed-racket%2Fbase-env%2Fprims..rkt%29._inst%29%29
  (accumulate (inst cons a (Listof a)) l2 l1))

(: length1 (All (a) (-> (Listof a) Natural)))
(define (length1 seq)
  (accumulate (lambda (x [n : Natural]) (+ 1 n)) 0 seq))

(: range (-> Integer Integer (Listof Integer)))
(define (range low high)
  (if (> low high)
      '()
      (cons low (range (+ low 1) high))))


;; ex 2.34

(: horner-eval (-> Real (Listof Real) Real))
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda ([this-coeff : Real] [higher-terms : Real])
                (+ this-coeff (* x higher-terms)))
              0
              coefficient-sequence))


;; ex 2.35
;; (: count-leaves (All (a) (-> (tr a) Natural)))
;; (define (count-leaves t)
  ;; (accumulate (lambda (a b) (+ 1 b)) 0 (map fringe t)))


;; ex 2.36

(: accumulate-n (All (a b) (-> (-> a b b) b (Listof (Listof a)) (Listof b))))
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op init (map (inst car a (Listof a)) seqs))
            (accumulate-n op init (map (inst cdr a (Listof a)) seqs)))))


;; ex 2.37
(: dot-product (-> (Listof Real) (Listof Real) Real))
(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(: matrix-*-vector (-> (Listof (Listof Real)) (Listof Real) (Listof Real)))
(define (matrix-*-vector m v)
  (map (lambda ([w : (Listof Real)]) (dot-product w v)) m))

(: transpose (-> (Listof (Listof Real)) (Listof (Listof Real))))
(define (transpose mat)
  (accumulate-n (inst cons Real (Listof Real)) '()  mat))

(: matrix-*-matrix (-> (Listof (Listof Real)) (Listof (Listof Real)) (Listof (Listof Real))))
(define (matrix-*-matrix m n)
  (let ([cols (transpose n)])
    (map (lambda ([v : (Listof Real)]) (matrix-*-vector cols v)) m)))


;; ex 2.39
(: reverse-r (All (a) (-> (Listof a) (Listof a))))
(define (reverse-r sequence)
  (foldr (lambda ([x : a] [y : (Listof a)]) (append y (list x))) '() sequence))


(: reverse-l (All (a) (-> (Listof a) (Listof a))))
(define (reverse-l sequence)
  (foldl (lambda ([x : a] [y : (Listof a)]) (cons x y)) '() sequence))


;; ;; ex 2.40
(: flatmap (All (a b) (-> (-> a (Listof b)) (Listof a) (Listof b))))
(define (flatmap proc lst)
  (foldr (inst append b) '() (map proc lst)))

(: unique-pairs (-> Integer (Listof (Listof Integer))))
(define (unique-pairs n)
  (flatmap (lambda ([i : Integer])
             (map
              (lambda ([j : Integer]) (list i j))
              (range 1 i)))
           (range 1 n)))

(: unique-triple (-> Integer (Listof (Listof Integer))))
(define (unique-triple n)
  (flatmap (lambda ([i : Integer])
             (flatmap
              (lambda ([j : Integer])
                (map
                 (lambda ([k : Integer])
                   (list i j k))
                 (range j n)))
              (range i n)))
           (range 1 n)))


(: triple-sum (-> Integer Integer (Listof (Listof Integer))))
(define (triple-sum s n)
  (filter (lambda ([lst : (Listof Integer)])
            (= s (apply + lst)))
          (unique-triple n)))


;; ex 2.42
(define-type Positions (Listof (Pairof Integer Integer)))

(: safe? (-> Integer Positions Boolean))
(define (safe? k positions)
  ;; (printf "~a~n" positions)
  (if (<= k 1)
      #t
      (let ([new-queen (car positions)]
            [rest-of-queens (cdr positions)])
        (foldr
         (lambda ([x : Boolean] [y : Boolean]) (and x y))
         #t
         (map (lambda ([queen : (Pairof Integer Integer)])
                (not (or (= (car new-queen) (car queen))
                         (= (cdr new-queen) (cdr queen))
                         (= (abs (- (car new-queen) (car queen))) (abs (- (cdr new-queen) (cdr queen)))))))
              rest-of-queens)))))

(: adjoin-position (-> Integer Integer Positions Positions))
(define (adjoin-position new-row k rest-of-queens)
  (cons (cons new-row k) rest-of-queens))

(define empty-board : Positions '())

(: queens (-> Integer (Listof Positions)))
(define (queens board-size)
  (: queen-cols (-> Integer (Listof Positions)))
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda ([positions : Positions]) (safe? k positions))
         (flatmap
          (lambda ([rest-of-queens : Positions])
            (map (lambda ([new-row : Integer])
                   (adjoin-position new-row k rest-of-queens))
                 (range 1 (+ 1 board-size))))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

