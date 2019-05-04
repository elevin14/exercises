#lang sicp

; Exercises for SICP chapter 1-1

; Exercise 1.2
(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))
   (* 3 (- 6 2) (- 2 7)))

; Exercise 1.3
(define (sum-two-larger-squares a b c)
  (cond ((and (> a c) (> b c)) (+ (* a a) (* b b)))
        ((and (> a b) (> c b)) (+ (* a a) (* c c)))
        (else (+ (* b b) (* c c)))))

(sum-two-larger-squares 3 5 5)

; Exercise 1.4
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

(a-plus-abs-b 10 2)
(a-plus-abs-b 10 (- 2))

; Exercise 1.5
(define (p) (p))

(define (test x y)
  (if (= x 0) 0 y))

; Exercise 1.7
(define (sqrt x)
  (sqrt-iter 1.0 2.0 x))

(define (sqrt-iter guess previous-guess x)
  (if (good-enough? guess previous-guess x)
      guess
      (sqrt-iter (improve guess x) guess x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess previous-guess x)
  (< (if (> previous-guess guess)
         (/ previous-guess guess)
         (/ guess previous-guess))
     1.001))

(define (square x) (* x x))

(sqrt 10)

; Exercise 1.8
(define (crt x)
  (crt-iter 1.0 2.0 x))

(define (crt-iter guess previous-guess x)
  (if (good-enough? guess previous-guess x)
      guess
      (crt-iter (cimprove guess x) guess x)))

(define (cimprove guess x)
  (/ (+ (/ x (* guess guess)) (* 2 guess))
     3))

(crt 10)