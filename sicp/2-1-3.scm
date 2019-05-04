#lang sicp

; Exercises for SICP chapter 2.1.3

; Helpers pulled from earlier assignments or text
(define (fast-expt b n)
(cond ((= n 0) 1)
((even? n) (square (fast-expt b (/ n 2))))
(else (* b (fast-expt b (- n 1))))))

(define (even? n)
(= (remainder n 2) 0))

(define (square x) (* x x))


; Exercise 2.4
(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))

(car (cons 1 2))

(define (cdr z)
  (z (lambda (p q) q)))

(cdr (cons 1 2))

; Exercise 2.5
(define (cons2 a b) (* (fast-expt 2 a) (fast-expt 3 b)))

(define (car2 x)
  (define (helper x a)
    (if (= (remainder x 2) 0)
        (helper (/ x 2) (+ a 1))
        a))
  (helper x 0))

(define (cdr2 x)
  (define (helper x a)
    (if (= (remainder x 3) 0)
        (helper (/ x 3) (+ a 1))
        a))
  (helper x 0))

(car2 (cons2 4 6))
(cdr2 (cons2 5 2))

; Exercise 2.6
(define zero (lambda (f) (lambda (x) x)))
(define (add-1 n) (lambda (f) (lambda (x) (f ((n f) x)))))

(define one (lambda (f) (lambda (x) (f x))))

(define two (lambda (f) (lambda (x) (f (f x)))))






