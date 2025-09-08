#lang typed/racket
;; #lang racket

;;; ex 2.53 does not require coding.

;; ex 2.54
(: eql? (-> (Listof Any) (Listof Any) Boolean))
(define (eql? l1 l2)
  (cond
    [(and (null? l1) (null? l2)) #t]
    [(or (null? l1) (null? l2)) #f]
    [else (and (eq? (car l1) (car l2)) (equal? (cdr l1) (cdr l2)))]))


;; ;;; ex 2.55 does not require coding.

;; ;; ex 2.56
(define-type Val (U Number Symbol Expr))
;; (define-type Expr (List Symbol Val Val))
(define-type Expr (List* Symbol Val Val (Listof Val)))
;; (define-type Expr (List* Symbol Val ...))

(: variable? (-> Val Boolean : Symbol))
(define (variable? x)
  (symbol? x))

(: same-variable? (-> Val Val Boolean))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(: =number? (-> Val Number Boolean))
(define (=number? exp num)
  (and (number? exp) (= exp num)))

(: make-sum (-> Val Val Val))
(define (make-sum a1 a2)
  (cond
    [(=number? a1 0) a2]
    [(=number? a2 0) a1]
    [(and (number? a1) (number? a2) (+ a1 a2))]
    [(and (number? a1) (list? a2) (eq? '+ (car a2))) (list* '+ a1 (cdr a2))]
    [else (list '+ a1 a2)]))

(: sum? (-> Val Boolean))
(define (sum? x)
  ;; (and (eq? '+ (car x)) (pair? x) )
  (if (pair? x)
      (eq? '+ (car x))
      #f))

(: addend (-> Expr Val))
(define (addend s)
  (cadr s))

(: augend (-> Expr Val))
(define (augend s)
  (if (null? (cdddr s))
      (caddr s)
      (cons '+ (cddr s))))

(: make-product (-> Val Val Val))
(define (make-product m1 m2)
  (cond
    [(or (=number? m1 0) (=number? m1 0)) 0]
    [(=number? m1 1) m2]
    [(=number? m2 1) m1]
    [(and (number? m1) (number? m2) (* m1 m2))]
    [(and (number? m1) (list? m2) (eq? '* (car m2))) (list* '* m1 (cdr m2))]
    [else (list '* m1 m2)]))

(: product? (-> Val Boolean))
(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(: multiplier (-> Expr Val))
(define (multiplier p)
  (cadr p))

(: multiplicand (-> Expr Val))
(define (multiplicand p)
  (if (null? (cdddr p)) (caddr p) (cons '* (cddr p))))

(: make-exponentiation (-> Val Val Val))
(define (make-exponentiation base exponent)
  (cond
    [(=number? exponent 0) 1]
    [(=number? exponent 1) base]
    [else (list '** base exponent)]))

(: exponentiation? (-> Val Boolean))
(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))

(: multiplicand (-> Expr Val))
(: base (-> Expr Val))
(define (base p)
  (cadr p))

(: multiplicand (-> Expr Val))
(: exponent (-> Expr Val))
(define (exponent p)
  (caddr p))

(: deriv (-> Val Symbol Val))
(define (deriv exp var)
  (cond
    [(number? exp) 0]
    [(variable? exp)
     (if (same-variable? exp var) 1 0)]
    [(exponentiation? exp)
     (make-product (exponent exp) (make-exponentiation (base exp) (make-sum -1 (exponent exp))))]
    [(sum? exp)
     (make-sum (deriv (addend exp) var)
               (deriv (augend exp) var))]
    [(product? exp)
     (make-sum (make-product (multiplier exp) (deriv (multiplicand exp) var))
              (make-product (deriv (multiplier exp) var) (multiplicand exp)))]
    [else
     (error "unknown expression")]))


;; (deriv '(* x y (+ x 3)) 'x)

;; ex 2.58
;; (define-type val (U Number Symbol Expr))
;; ;; (define-type expr (List* val (Listof (List Symbol val))))
;; (define-type InfixExpr (U Null (List* Symbol val InfixExpr)))
;; (define-type Expr (List* val Symbol val InfixExpr))
;; ;; (define-type expr (U val (List* val Symbol expr)))
;; (define-predicate expr? Expr)

;; (: low-op (-> Expr Symbol))
;; (define (low-op expr)
;;   ;; (cond
;;   ;;   [(null? (cdddr expr)) (cadr expr)]
;;   ;;   [(eq? (cadr expr) '*) '*]
;;   ;;   [else (low-op (cddr expr))])
;;   (if (memq '* expr) '* '+))

;; (: make-sum (-> val val val))
;; (define (make-sum a1 a2)
;;   (cond
;;     [(=number? a1 0) a2]
;;     [(=number? a2 0) a1]
;;     [(and (number? a1) (number? a2) (+ a1 a2))]
;;     [else (list a1 '+ a2)]))

;; (define (sum? x)
;;   ;; (and (expr? x) (eq? (cadr x) '+))
;;   (and (expr? x) (eq? (low-op x) '+)))

;; ;; (: addend (-> Expr val))
;; ;; (define (addend s)
;; ;;   (car s))

;; ;; (: augend (-> Expr val))
;; ;; (define (augend s)
;; ;;   (caddr s))

;; (: prefix (-> Symbol Expr val))
;; (define (prefix op expr)
;;   (if (eq? (cadr  expr) op) (car expr)
;;       (list* (car expr) (cadr expr) (prefix (cddr expr)))))

;; (: addend (-> Expr val))
;; (define (addend s)
;;   (prefix '+ s))

;; (: augend (-> Expr val))
;; (define (augend s)
;;   (let ([sl (memq '+ s)])
;;     (if (list? sl)
;;         sl
;;         (car sl))))

;; (: make-product (-> val val val))
;; (define (make-product m1 m2)
;;   (cond
;;     [(or (=number? m1 0) (=number? m1 0)) 0]
;;     [(=number? m1 1) m2]
;;     [(=number? m2 1) m1]
;;     [else (list m1 '*  m2)]))

;; (define (product? x)
;;   ;; (and (expr? x) (eq? (cadr x) '*))
;;   (and (expr? x) (eq? (low-op x) '*)))

;; (: multiplier (-> Expr val))
;; (define (multiplier p)
;;   (car p))

;; (: multiplicand (-> Expr val))
;; (define (multiplicand p)
;;   (caddr p))

;; (: deriv (-> val Symbol val))
;; (define (deriv exp var)
;;   (cond
;;     [(number? exp) 0]
;;     [(variable? exp)
;;      (if (same-variable? exp var) 1 0)]
;;     ;; [(exponentiation? exp)
;;     ;;  (make-product (exponent exp) (make-exponentiation (base exp) (make-sum -1 (exponent exp))))]
;;     [(sum? exp)
;;      (make-sum (deriv (addend exp) var)
;;                (deriv (augend exp) var))]
;;     [(product? exp)
;;      (make-sum (make-product (multiplier exp) (deriv (multiplicand exp) var))
;;               (make-product (deriv (multiplier exp) var) (multiplicand exp)))]
;;     [else
;;      (error "unknown expression")]))

;; (deriv '((2 * x) + 1) 'x)


(: element-of-set? (-> Any (Listof Any) Boolean))
(define (element-of-set? x st)
  (cond
    [(null? st) #f]
    [(equal? x (car st)) #t]
    [else (element-of-set? x (cdr st))]))

(: adjoin-set (-> Any (Listof Any) (Listof Any)))
(define (adjoin-set x st)
  (if (element-of-set? x st)
      st
      (cons x st)))

(: intersection-set (-> (Listof Any) (Listof Any) (Listof Any)))
(define (intersection-set set1 set2)
  (cond
    [(or (null? set2) (null? set2)) '()]
    [(element-of-set? (car set1) set2) (cons (car set1) (intersection-set (cdr set1) set2))]
    [else (intersection-set (cdr set1) set2)]))

(: union-set (-> (Listof Any) (Listof Any) (Listof Any)))
(define (union-set set1 set2)
  (cond
    [(null? set1) set2]
    [(null? set2) set1]
    [(not (element-of-set? (car set1) set2)) (cons (car set1) (intersection-set (cdr set1) set2))]
    [else (intersection-set (cdr set1) set2)]))


;; ex 2.60

(: element-of-set?1 (-> Any (Listof Any) Boolean))
(define (element-of-set?1 x st)
  (cond
    [(null? st) #f]
    [(equal? x (car st)) #t]
    [else (element-of-set?1 x (cdr st))]))

(: adjoin-set1 (-> Any (Listof Any) (Listof Any)))
(define (adjoin-set1 x st)
  (if (element-of-set?1 x st)
      st
      (cons x st)))

(: intersection-set1 (-> (Listof Any) (Listof Any) (Listof Any)))
(define (intersection-set1 set1 set2)
  (cond
    [(or (null? set2) (null? set2)) '()]
    [(element-of-set? (car set1) set2) (cons (car set1) (intersection-set (cdr set1) set2))]
    [else (intersection-set (cdr set1) set2)]))


;; ex 2.61
(: adjoin-set-sorted (-> Real (Listof Real) (Listof Real)))
(define (adjoin-set-sorted x st)
  (if (< x (car st))
      st
      (cons (car st) (adjoin-set-sorted x (cdr st)))))


;; ex 2.62
(: union-set-sorted (-> (Listof Real) (Listof Real) (Listof Real)))
(define (union-set-sorted set1 set2)
  (cond
    [(null? set1) set2]
    [(null? set2) set1]
    [else
     (let ([x1 (car set1)]
           [x2 (car set2)])
       (cond
         [(< x1 x2) (cons x1 (cons x2 (union-set-sorted (cdr set1) (cdr set2))))]
         [(> x1 x2) (cons x2 (cons x1 (union-set-sorted (cdr set1) (cdr set2))))]
         [else (cons x1 (union-set-sorted (cdr set1) (cdr set2)))]))]))


;; ex 2.63
;; (define-type Tree (U Null (List Real Tree Tree)))
(define-type Tree (List Real Tree Tree))

(: make-tree (-> Real Tree Tree Tree))
(define (make-tree entry left right)
  (list entry left right))

(: entry (-> Tree Real))
(define (entry tree)
  (car tree))

(: left-branch (-> Tree Tree))
(define (left-branch tree)
  (cadr tree))

(: right-branch (-> Tree Tree))
(define (right-branch tree)
  (caddr tree))

(: element-of-set-tree (-> Real Tree Boolean))
(define (element-of-set-tree x st)
  (cond
    [(null? st) #f]
    [(= x (entry st)) #t]
    [(< x (entry st)) (element-of-set-tree x (left-branch st))]
    [else (element-of-set-tree x (right-branch st))]))

(: adjoin-set-tree (-> Real Tree Tree))
(define (adjoin-set-tree x st)
  (cond
    [(null? st) (make-tree x '() '())]
    [(= x (entry st)) st]
    [(< x (entry st))
     (make-tree (entry st) (adjoin-set-tree x (left-branch st)) (right-branch st))]
    [else (make-tree (entry st) (right-branch st) (adjoin-set-tree x (right-branch st)))]))

(: tree-list-1 (-> Tree (Listof Real)))
(define (tree-list-1 tree)
  (if (null? tree)
      '()
      (append (tree-list-1 (left-branch tree))
              (cons (entry tree)
                    (tree-list-1 (right-branch tree))))))


;; ex 2.65
;; (define (union-set-tree set1 set2)
;;   (let ([lst1 (tree-list-1 set1)]
;;         [lst2 (tree-list-1 set1)])
;;     (list-tree (union-set-sorted lst1 lst2))))

;; (define (intersection-set-tree set1 set2)
;;   (let ([lst1 (tree-list-1 set1)]
;;         [lst2 (tree-list-1 set1)])
;;     (list-tree (intersection-set-sorted lst1 lst2))))


;; ex 2.66

(: lookup (-> Real Tree Boolean))
(define (lookup given-key set-of-records)
  (cond
    [(null? set-of-records) #f]
    [(equal? given-key (entry set-of-records))]
    [else (or (lookup given-key (left-branch set-of-records))
              (lookup given-key set-of-records))]))


;; ex 2.67

(define-type Leaf (List 'leaf Symbol Positive-Integer))
(define-predicate leaf? Leaf)

(define-type CodeTree (U Leaf (List CodeTree CodeTree (Listof Symbol) Positive-Integer)))

(: make-leaf (-> Symbol Positive-Integer Leaf))
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(: symbol-leaf (-> Leaf Symbol))
(define (symbol-leaf leaf)
  (cadr leaf))

(: weight-leaf (-> Leaf Positive-Integer))
(define (weight-leaf leaf)
  (caddr leaf))

(: make-code-tree (-> CodeTree CodeTree CodeTree))
(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(: left-branch-code (-> CodeTree CodeTree))
(define (left-branch-code tree)
  (if (leaf? tree)
      (error "Node is a leaf")
      (car tree)))

(: right-branch-code (-> CodeTree CodeTree))
(define (right-branch-code tree)
  (if (leaf? tree)
      (error "Node is a leaf")
      (cadr tree)))

(: symbols (-> CodeTree (Listof Symbol)))
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(: weight (-> CodeTree Positive-Integer))
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(: decode (-> (Listof Integer) CodeTree (Listof Symbol)))
(define (decode bits tree)
  (: decode-1 (-> (Listof Integer) CodeTree (Listof Symbol)))
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ([next-branch (choose-branch (car bits) current-branch)])
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(: choose-branch (-> Integer CodeTree CodeTree))
(define (choose-branch bit branch)
  (cond [(= bit 0) (left-branch-code branch)]
        [(= bit 1) (right-branch-code branch)]
        [else (error "Bad bit -- CHOOSE-BRANCH" bit)]))

(: adjoin-set-code (-> CodeTree (Listof CodeTree) (Listof CodeTree)))
(define (adjoin-set-code x st)
  (cond [(null? st) (list x)]
        [(< (weight x) (weight (car st))) (cons x st)]
        [else (cons (car st) (adjoin-set-code x (cdr st)))]))

(: make-leaf-set (-> (Listof (List Symbol Positive-Integer)) (Listof CodeTree)))
(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ([pair (car pairs)])
        (adjoin-set-code (make-leaf (car pair) (cadr pair))
                         (make-leaf-set (cdr pairs))))))


(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))


;; ex 2.68
(: encode-symbol (-> Symbol CodeTree (Listof Integer)))
(define (encode-symbol sym tree)
  (cond
    [(not (memq sym (symbols tree)))
     (error "Symbol not in encoding" sym)]
    [(leaf? tree) '()]
    [else
     (let ([bit (if (memq sym (symbols (left-branch-code tree))) 1 0)])
            (if bit
                (cons bit (encode-symbol sym (left-branch-code tree)))
                (cons bit (encode-symbol sym (right-branch-code tree)))))]))

(: encode (-> (Listof Symbol) CodeTree (Listof Integer)))
(define (encode message tree)
  (if (null? message)
      '()
      (append
       (encode-symbol (car message) tree)
       (encode (cdr message) tree))))

;; '(A D A B B C A)


;; ex 2.69

;; (define (thin-leafs leafs w)
;;   (cond
;;     [(null? leafs) '()]
;;     [(< (weight (car leafs)) w) (cons (car leafs) (thin-leafs (cdr leafs)))]
;;     [else '()]))

;; (define (fat-leafs leafs w)
;;   (cond
;;     [(null? leafs) '()]
;;     [(< (weight (car leafs)) w) (fat-leafs (cdr leafs))]
;;     [else leafs]))

;;; tip: use adjoin-set
(: successive-merge (-> (Listof CodeTree) CodeTree))
(define (successive-merge leafs)
  (cond
    [(null? leafs) (error "Null leaf list")]
    [(null? (cdr leafs)) (car leafs)]
    ;; [(null? (cddr leafs)) (make-code-tree (car leafs) (cadr leafs))]
    [else
     (successive-merge (adjoin-set-code (make-code-tree (car leafs) (cadr leafs)) (cddr leafs)))]))


(: generate-huffman-tree (-> (Listof (List Symbol Positive-Integer)) CodeTree))
(define (generate-huffman-tree pairs)
  (if (null? pairs)
      (error "Null symbol list")
      (successive-merge (make-leaf-set pairs))))

(define rock-weights '((BOOM 1) (WAH 1) (GET 2) (JOB 2) (A 2) (SHA 3) (YIP 9) (NA 16)))

(define rock-code-tree (generate-huffman-tree rock-weights))

;; (encode )
