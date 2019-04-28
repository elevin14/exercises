#lang sicp

; Exercises for SICP chapter 1-2

; Exerceise 1.24
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder
          (square (expmod base (/ exp 2) m))
          m))
        (else
         (remainder
          (* base (expmod base (- exp 1) m))
          m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (divides? a b) (= (remainder b a) 0))

(define (prime? n)
  (fast-prime? n 5))

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

(search-for-primes 1000 1050)
(newline) 

; Exercise 1.27
(define (carmichael n)
  (define (carmichael-helper n a)
    (if (= a 1)
        #t
        (and (= (expmod a n n) a) (carmichael-helper n (- a 1)))))
  (carmichael-helper n (- n 1)))

(carmichael 1009)
(carmichael 1010)
(carmichael 2465)
