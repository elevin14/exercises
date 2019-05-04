#lang sicp

; Exercises for SICP chapter 2.1.4

; Helpers pulled from earlier assignments or text
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

; Exercise 2.7
(define (make-interval a b) (cons a b))
(define (upper-bound x) (cdr x))
(define (lower-bound x) (car x))

; Exercise 2.8
(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

(define (sub-interval2 x y)
  (add-interval
   x
   (make-interval (- (lower-bound y))
                  (- (upper-bound y)))))

; Exercise 2.9
(define (width x) (/ (- (upper-bound x) (lower-bound x)) 2))
; Let two intervals be n+-w/2 and m+-v/2.
; The sum is n+m+-(w/2+v/2), showing that the width of the sums is a function of
; the two input widths.
; For a product (n+-w/2)(m+-v/2), you will get different terms, but always some will have
; a mix of n/m and w/v, it is impossible to isolate the vw terms. Therefore, width of
; a product depends not only on the original widths, but also the numbers themselves.
; This can easily be seen in practice. (100+-1)(2+-1) = (99-303), vs (2+-1)(2+-1) = (1-9)

; Exercise 2.10
(define (div-interval x y)
  (if (> (* (upper-bound y) (lower-bound y)) 0)
      (mul-interval
       x
       (make-interval (/ 1.0 (upper-bound y))
                      (/ 1.0 (lower-bound y))))
      (error "Interval spans 0")))

; Exercise 2.11
(define (mul-interval x y)
  (let ((xl? (> (lower-bound x) 0))
        (xu? (> (upper-bound x) 0))
        (yl? (> (lower-bound y) 0))
        (yu? (> (upper-bound y) 0))
        (xl (lower-bound x))
        (xu (upper-bound x))
        (yl (lower-bound y))
        (yu (upper-bound y)))
    (cond ((and xl? xu? yl? yu?) (make-interval (* xl yl) (* xu yu)))
          ((and (not xl?)  xu? yl? yu?) (make-interval (* xl yu) (* xu yu)))
          ((and xl? (not xu?) yl? yu?) (make-interval (* xu yl) (* xu yu)))
          ((and (not xl?) (not xu?) (not yl?) yu?) (make-interval (* xl yu) (* xl yl)))
          ((and (not xl?) xu? (not yl?) (not yu?)) (make-interval (* xu yl) (* xl yl)))
          ((and (not xl?) (not xu?) (not yl?) (not yu?)) (make-interval (* xu yu) (* xl yl)))
          ((and (not xl?) (not xu?) yl? yu?) (make-interval (* xl yu) (* xu yl)))
          ((and xl? xu? (not yl?) (not yu?)) (make-interval (* xu yl) (* xl yu)))
          (else (make-interval (min (* xl yu) (* xu yl)) (max (* xl yl) (* xu yu)))))))

; Exercise 2.12
(define (make-center-percent c p)
  (make-interval (- c (* c (/ p 100.0))) (+ c (* c (/ p 100.0)))))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (percent i)
  (* (/ (- (upper-bound i) (center i)) (center i)) 100.0))


; Exerceise 2.13
; Say we have two tolerances, c1 with t1 and c2 with t2. Looking at the max product,
; c1(1+t1)c2(1+t2) = c1c2(1+t1+t2+t1t2). This is in the same form as c(1+t), so
; t = t1 + t2 + t1t2. Assuming small tolerances, t1t2 will tend towards zero, so the
; tolerance of a product is t1 + t2.

; Exerceise 2.14
(define (print-center-percent x)
  (newline)
  (display (center x))
  (display "+/-")
  (display (percent x))
  (display "%"))

(print-center-percent (add-interval (make-center-percent 10 10) (make-center-percent 10 5)))
(print-center-percent (div-interval (make-center-percent 100 .01) (make-center-percent 100 .01)))
(print-center-percent (div-interval (make-center-percent 100 1) (make-center-percent 200 1)))

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))
(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval
     one (add-interval (div-interval one r1)
                       (div-interval one r2)))))

(print-center-percent (par1 (make-center-percent 100 1) (make-center-percent 200 1)))
(print-center-percent (par2 (make-center-percent 100 1) (make-center-percent 200 1)))

; Exercise 2.15
; Eva Lu Ator is correct, each additional usage of ranges adds uncertainty because they really
; represent a tree of possible arithmatic, not a simple algebraic reduction.

; Exerceise 2.16
; Anything is possible. It would seem that you should devise a way to reduce all functions to
; minimum terms before using the objects above.