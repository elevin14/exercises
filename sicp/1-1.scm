#lang sicp


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