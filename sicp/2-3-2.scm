#lang sicp

; Exercises for SICP chapter 2.3.2

; Helpers pulled from earlier assignments or text
(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (sum? x) (and (pair? x) (eq? (car x) '+)))
(define (product? x) (and (pair? x) (eq? (car x) '*)))

(define (=number? exp num) (and (number? exp) (= exp num)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2))
         (+ a1 a2))
        (else (list '+ a1 a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

(define (expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (expt b (/ n 2))))
        (else (* b (expt b (- n 1))))))

(define (square x) (* x x))

(define (even? n)
  (= (remainder n 2) 0))

(define (accumulate op initial sequence) 
  (if (null? sequence) 
      initial 
      (op (car sequence) 
          (accumulate op initial (cdr sequence))))) 

; Exercise 2.56
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum? exp) (make-sum (deriv (addend exp) var)
                              (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        ((exponentiation? exp)
         (make-product
          (make-product (exponent exp)
                        (make-exponentiation (base exp)
                                             (make-sum (exponent exp) -1)))
          (deriv (base exp) var)))
        (else
         (error "unknown expression type: DERIV" exp))))

(define (exponentiation? x) (eq? (car x) '**))
(define (base e) (cadr e))
(define (exponent e) (caddr e))

(define (make-exponentiation b n)
  (cond ((=number? n 0) 1)
        ((=number? n 1) b)
        ((and (number? b) (number? n)) (expt b n))
        (else (list '** b n))))

; Exercise 2.57
(define (addend s) (cadr s))
(define (augend s) (accumulate make-sum 0 (cddr s)))

(define (multiplier p) (cadr p))
(define (multiplicand p) (accumulate make-product 1 (cddr p)))

(make-sum 4 (list 3 5))
(deriv '(* x y (+ x 3)) 'x)

; Exercise 2.58a
(define (deriv2 exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum?2 exp) (make-sum (deriv2 (addend2 exp) var)
                               (deriv2 (augend2 exp) var)))
        ((product?2 exp)
         (make-sum
          (make-product (multiplier2 exp)
                        (deriv2 (multiplicand2 exp) var))
          (make-product (deriv2 (multiplier2 exp) var)
                        (multiplicand2 exp))))   
        (else
         (error "unknown expression type: DERIV" exp))))

(define (sum?2 x) (and (pair? x) (eq? (cadr x) '+)))
(define (product?2 x) (and (pair? x) (eq? (cadr x) '*)))

(define (addend2 s) (car s))
(define (augend2 s) (caddr s))

(define (multiplier2 p) (car p))
(define (multiplicand2 p) (caddr p))

(deriv2 '(x + (3 * (x + (y + 2)))) 'x)

; Exerceise 2.58b
(define (deriv3 exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum?3 exp) (make-sum (deriv3 (addend2 exp) var)
                               (deriv3 (augend2 exp) var)))
        ((product?3 exp)
         (make-sum
          (make-product (multiplier2 exp)
                        (deriv3 (multiplicand2 exp) var))
          (make-product (deriv3 (multiplier2 exp) var)
                        (multiplicand2 exp))))   
        (else
         (error "unknown expression type: DERIV" exp))))

(define (sum?3 x) (and (pair? x)
                       (eq? (cadr x) '+)
                       (if (pair? (cdddr x))
                           (sum?3 cddr x)
                           #t)))

(define (product?3 x) (and (pair? x)
                           (accumulate or #f (map (lambda (e) (eq? '*)) x))))

(define (addend3 s) (car s))
(define (augend3 s) (caddr s))

(define (multiplier3 p) (car p))
(define (multiplicand3 p) (caddr p))

(cdddr (list 1 '+ 2 '+ 3))