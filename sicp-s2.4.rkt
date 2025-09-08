#lang racket
(require racket/generic)


(define-generics complez
  [real complez]
  [imag complez]
  [magni complez]
  [angl complez])

(struct Rect (real imag)
  #:methods gen:complez
  [(define (real z)
     (Rect-real z))
   (define (imag z)
     (Rect-imag z))
   (define (magni z)
     (sqrt (+ (sqr (Rect-real z))
              (sqr (Rect-imag z)))))
   (define (angl z)
     (atan (Rect-real z)
           (Rect-imag z)))]
  )


;; (: rectangular-magni (-> rectangular Real))
;; (define (rectangular-magni z)
;;   (sqrt (+ (sqr (rectangular-real z))
;;            (sqr (rectangular-imag z)))))

;; (: rectangular-angl (-> rectangular Real))
;; (define (rectangular-angl z)
;;   (atan (rectangular-real z)
;;         (rectangular-imag z)))


(struct Polar (magni angl)
  #:methods gen:complez
  [(define (real z)
     (* (Polar-magni z)
        (sin (Polar-angl z))))
   (define (imag z)
     (* (Polar-magni z)
        (cos (Polar-angl z))))
   (define (magni z)
     (Polar-magni z))
   (define (angl z)
     (Polar-angl z))]
  )

;; (: polar-real (-> polar Real))
;; (define (polar-real z)
;;   (* (polar-magni z)
;;      (sin (polar-angl z))))

;; (: polar-imag (-> polar Real))
;; (define (polar-imag z)
;;   (* (polar-magni z)
;;      (cos (polar-angl z))))

;; (define (apply-generic op ))

;; (: real (-> (U retangular polar) Real))
;; (define (real z))
;; (: imag (-> (U retangular polar) Real))
;; (define (imag z))
;; (: magni (-> (U retangular polar) Real))
;; (define (magni z))
;; (: angl (-> (U retangular polar) Real))
;; (define (angl z))

;; (define (variable? x)
;;   (symbol? x))

;; (define (same-variable? v1 v2)
;;   (and (variable? v1) (variable? v2) (eq? v1 v2)))

;; (: =number? (-> Any Number Boolean))
;; (define (=number? exp num)
;;   (and (number? exp) (= exp num)))

;; (define (deriv exp var)
;;   (cond
;;     [(number? exp) 0]
;;     [(variable? exp) (if (same-variable? exp var) 1 0)]
;;     [else ((get 'deriv (operator exp)) (operands exp) var)]))
