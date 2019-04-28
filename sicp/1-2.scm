#lang sicp

; Exercises for SICP chapter 1-2

; Exercise 1.10
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1) (A x (- y 1))))))

(A 1 10)
(A 2 4)
(A 3 3)

(define (f n) (A 0 n))
; f is equivalent to 2n

(define (g n) (A 1 n))
; g is equivalent to 2^n

(define (h n) (A 2 n))
; h is equivalent to 2^2^2... n times

; Exercise 1.11
(define (rec11 n)
  (if (< n 3)
      n
      (+ (rec11 (- n 1))
         (* 2 (rec11 (- n 2)))
         (* 3 (rec11 (- n 3))))))

(define (iter11 n)
  (define (iter11-helper n n1 n2 n3 count)
    (if (= n count)
        (+ n1 (* 2 n2) (* 3 n3))
        (iter11-helper n
                       (+ n1 (* 2 n2) (* 3 n3))
                       n1 
                       n2
                       (+ count 1))))
  (if (< n 3)
      n
      (iter11-helper n 2 1 0 3)))

(rec11 5)
(iter11 5)

; Exercise 1.12
; Calculate element of Pascal triangle by row and column, index starting with 1
(define (pascal row column)
  (cond ((< column 1) 0)
        ((> column row) 0)
        ((= column 1) 1)
        (else (+ (pascal (- row 1) column)
                 (pascal (- row 1) (- column 1))))))

(pascal 6 4)

; Exercise 1.16
(define (fast-expt b n)
  (define (fast-expt-iter b n a)
    (cond ((= n 0) a)
          ((even? n) (fast-expt-iter (square b) (/ n 2) a))
          (else (fast-expt-iter b (- n 1) (* a b)))))
  (fast-expt-iter b n 1))

(define (even? n)
  (= (remainder n 2) 0))

(define (square n) (* n n))

(fast-expt 2 9)

; Exercise 1.17
(define (double n) (* 2 n))
(define (half n) (/ n 2))

(define (fast-mult a b)
  (cond ((= b 1) a)
        ((even? b) (double (fast-mult a (half b))))
        (else (+ a (fast-mult a (- b 1))))))

(fast-mult 9 12)

; Exercise 1.18
(define (fast-mult-iter a b)
  (define (fast-mult-helper a b n)
    (cond ((= b 1) (+ a n))
          ((even? b) (fast-mult-helper (double a) (half b) n))
          (else (fast-mult-helper a (- b 1) (+ n a)))))
  (fast-mult-helper a b 0))

(fast-mult-iter 9 12)

; Exercise 1.19
(define (fib n)
  (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (* p p) (* q q)) ; compute p
                   (+ (* 2 p q) (* q q)) ; compute q
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))

(fib 7)

; Exercise 1.21
(define (smallest-divisor n) (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b) (= (remainder b a) 0))

(smallest-divisor 199)
(smallest-divisor 1999)
(smallest-divisor 19999)

; Exercise 1.22
(define (prime? n)
  (= n (smallest-divisor n)))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes lower-limit upper-limit)
  (timed-prime-test lower-limit)
  (cond ((> lower-limit (- upper-limit 2)) (display "Done"))
        ((= (remainder lower-limit 2) 0) (search-for-primes (+ lower-limit 1) upper-limit))
        (else (search-for-primes (+ lower-limit 2) upper-limit))))

(search-for-primes 2 30)