#lang sicp

; Exercises for SICP chapter 1.3.4

; Helpers pulled from earlier assignments or text
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

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

(define (average a b)
  (/ (+ a b) 2))

(define (deriv g)
  (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))

(define dx 0.00001)

(define (newton-transform g)
  (lambda (x) (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (cube x) (* x x x))

(define (square x) (* x x))

(define (inc x) (+ x 1))

(define (average-damp f)
  (lambda (x) (average x (f x))))

; Exercise 1.40
(define (cubic a b c)
  (lambda (x) (+ (cube x) (* a (square x)) (* b x) c)))

(newtons-method (cubic 2 3 4) 1)

; Exercise 1.41
(define (double f)
  (lambda (x) (f (f x))))

((double inc) 1)

(((double (double double)) inc) 5)

; Exercise 1.42
(define (compose f g)
  (lambda (a) (f (g a))))

((compose square inc) 6)

; Exercise 1.43
(define (repeated f n)
  (define (helper g n)
    (if (= n 1)
        g
        (helper (compose f g) (- n 1))))
  (helper f n))

((repeated square 2) 5)

; Exercise 1.44
(define (smoothed f)
  (lambda (x) (average (f x) (f (+ x dx)))))

(define (n-smoothed f n)
  (lambda (x) (repeated smoothed n)))

; Exercise 1.45
(fixed-point-of-transform
 (lambda (y) (/ 10 (* y y y y y y y)))
 (repeated average-damp 3)
 2.0)

(define (power a b)
  (if (= b 1) a
      (* a (power a (- b 1)))))

(define (nth-root n x)
  (fixed-point-of-transform
   (lambda (y) (/ x (power y (- n 1))))
   (repeated average-damp (floor (/ (log n) (log 2))))
   2.0))

(nth-root 8 10)

; Exercise 1.46
(define (iterative-improve good-enough? improve)
  (lambda (guess)
    (if (good-enough? guess)
        guess
        ((iterative-improve good-enough? improve) (improve guess)))))

(define (sqrt x)
  ((iterative-improve
   (lambda (n) (< (abs (- (square n) x)) 0.001))
   (lambda (n) (average n (/ x n))))
   2.0))

(sqrt 16)

(define (fixed-point-new f initial-guess)
  ((iterative-improve
    (lambda (n) (< (abs (- n (f n))) 0.00001))
    (lambda (n) (f n)))
   initial-guess))

(fixed-point-new cos 1.0)
