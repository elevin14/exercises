#lang sicp

; Exercises for SICP chapter 1.3.3

; Helpers pulled from earlier assignments or text
(define (smallest-divisor n) (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b) (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (square n) (* n n))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (cube x) (* x x x))

(define (inc n) (+ n 1))

(define (identity n) n)

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (average a b)
  (/ (+ a b) 2))

; Exercise 1.35
(define (golden-ratio)
  (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0))

(golden-ratio)

; Exerceise 1.36
(define (fixed-point-print f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (display next)
      (newline)
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (golden-ratio-print)
  (fixed-point-print (lambda (x) (+ 1 (/ 1 x))) 1.0))

(golden-ratio-print)

(define (x-to-x n)
  (fixed-point-print (lambda (x) (/ (log n) (log x))) 2.0))

(define (x-to-x-damp n)
  (fixed-point-print (lambda (x) (average x (/ (log n) (log x)))) 2.0))

(x-to-x 1000)
(x-to-x-damp 1000)

; Exerceise 1.37
(define (cont-frac-rec n d k)
  (define (cont-frac-helper n d k i)
    (if (= i k)
        (/ (n i) (d i))
        (/ (n i) (+ (d i) (cont-frac-helper n d k (+ i 1))))))
  (cont-frac-helper n d k 1))

(define (cont-frac-iter n d k)
  (define (cont-frac-helper n d k a)
    (if (= k 0)
        a
        (cont-frac-helper n d (- k 1) (/ (n k) (+ (d k) a)))))
  (cont-frac-helper n d k 0))
      

(cont-frac-rec (lambda (i) 1.0) (lambda (i) 1.0) 10)
(cont-frac-iter (lambda (i) 1.0) (lambda (i) 1.0) 10)

; Exercise 1.38
(+ 2 (cont-frac-rec (lambda (i) 1.)
               (lambda (i)
                 (if (= (remainder i 3) 2)
                     i
                     1.0))
               10))

; Exercise 1.39
(define (tan-cf x k)
  (cont-frac-rec (lambda (i)
                   (if (= i 1)
                       x
                       (- (* x x))))
                 (lambda (i)
                   (- (* 2 i) 1)) k))

(tan-cf 1.5 10)
                 