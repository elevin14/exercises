#lang sicp

; Exercises for SICP chapter 2.1

; Helpers pulled from earlier assignments or text
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (numer x) (car x))
(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

; Exercise 2.1

(define (make-rat n d)
  (let ((g (gcd (abs n) (abs d))))
    (if (> (* n d) 0)
        (cons (/ (abs n) g) (/ (abs d) g))
        (cons (/ (- (abs n)) g) (/ (abs d) g)))))

(print-rat (make-rat -5 -10))

; Exercise 2.2
(define (make-segment start end) (cons start end))
(define (start-segment s) (car s))
(define (end-segment s) (cdr s))

(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))

(define (midpoint-segment s)
  (make-point
   (/ (+ (x-point (end-segment s)) (x-point (start-segment s))) 2)
   (/ (+ (y-point (end-segment s)) (y-point (start-segment s))) 2)))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  121
  (display (y-point p))
  (display ")"))

(print-point (midpoint-segment (make-segment (make-point 1 -1) (make-point 5 5))))

; Exercise 2.3
; Assume rectangle stays in grid, constructor using 3 points
(define (make-rectangle p1 p2 p3)
  (cons p1 (cons p2 (cons p3))))

(define (corner-1 r) (car r))
(define (corner-2 r) (car (cdr r)))
(define (corner-3 r) (car (cdr (cdr r))))

(define (side-length-1 r)
  (+ (abs (- (x-point (corner-1 r)) (x-point (corner-2 r))))
     (abs (- (y-point (corner-1 r)) (y-point (corner-2 r))))))

(define (side-length-2 r)
  (+ (abs (- (x-point (corner-2 r)) (x-point (corner-3 r))))
     (abs (- (y-point (corner-2 r)) (y-point (corner-3 r))))))

(define (perimeter r) (* 2 (+ (side-length-1 r) (side-length-2 r))))
(define (area r) (* (side-length-1 r) (side-length-2 r)))

; Alternate constructor using 2 segments
(define (make-rectangle-2 s1 s2)
  (cons s1 s2))

(define (corner-1-2 r) (start-segment (car r)))
(define (corner-2-2 r) (end-segment (car r)))
(define (corner-3-2 r) (end-segment (cdr r)))
; If side-length was changed to point to the correct methods, perimeter and area would work
