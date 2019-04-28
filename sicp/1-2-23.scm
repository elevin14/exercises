#lang sicp

; Exercises for SICP chapter 1-2

; Exerceise 1.23
(define (smallest-divisor n) (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

; New for 1.23
(define (next n)
  (cond ((= n 2) 3)
        (else (+ n 2))))

(define (divides? a b) (= (remainder b a) 0))

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
(define (square n) (* n n))

(search-for-primes 10000 10050)