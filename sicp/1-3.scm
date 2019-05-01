#lang sicp

; Exercises for SICP chapter 1-3

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

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (integral f a b dx)
  (define (add-dx x)
    (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

(define (cube x) (* x x x))

(define (inc n) (+ n 1))

(define (identity n) n)

; Exercise 1.29
(define (simpson-integral f a b n)
  (define (simpson-helper k)
    (cond ((= k 0) (+ (yk k) (simpson-helper (+ k 1))))
          ((= k n) (yk k))
          ((= (remainder k 2) 1) (+ (* 4 (yk k)) (simpson-helper(+ k 1))))
          (else (+ (* 2 (yk k)) (simpson-helper (+ k 1))))))
  (define (yk k)
    (f (+ a (* k (/ (- b a) n)))))
  (* (/ (- b a) n) (simpson-helper 0)))

(integral cube 0 1 0.01)
(integral cube 0 1 0.001)

(simpson-integral cube 0 1 100)
(simpson-integral cube 0 1 1000)

; Exercise 1.30
(define (sum-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

(sum-iter cube 1 inc 4)

; Exercise 1.31
(define (prod-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))

(define (prod-rec term a next b)
  (if (> a b)
      1
      (* (term a)
         (prod-rec term (next a) next b))))

(define (factorial f n)
  (f identity 1 inc n))

(factorial prod-iter 5)
(factorial prod-rec 5)

(define (pi-approx f n)
  (define (term a)
    (/ (+ a 2 (-(remainder a 2)))
       (+ a 1 (remainder a 2))))
  (* 4.0 (f term 1 inc n)))

(pi-approx prod-iter 100)
(pi-approx prod-rec 1000)

; Exerceise 1.32
(define (accumulate-iter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner (term a) result))))
  (iter a null-value))

(define (accumulate-rec combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a) (accumulate-rec combiner null-value term (next a) next b))))

(define (sum-acc term a next b)
  (accumulate-iter + 0 term a next b))

(define (prod-acc term a next b)
  (accumulate-rec * 1 term a next b))

(define (cube-sums-accumulate f n)
  (f + 0 cube 0 inc n))

(cube-sums-accumulate accumulate-iter 4)
(cube-sums-accumulate accumulate-rec 4)

; Exercise 1.33
(define (filtered-accumulate filter combiner null-value term a next b)
  (cond ((> a b) null-value)
        ((filter a) (combiner (term a) (filtered-accumulate filter combiner null-value term (next a) next b)))
        (else (filtered-accumulate filter combiner null-value term (next a) next b))))

(define (sum-square-primes a b)
  (filtered-accumulate prime? + 0 square a inc b))

(sum-square-primes 2 10)

(define (prod-rel-prime n)
  (define (rel-prime? a)
    (= 1 (gcd a n)))
  (filtered-accumulate rel-prime? * 1 identity 1 inc (- n 1)))

(prod-rel-prime 5)
(prod-rel-prime 8)

                          
  

