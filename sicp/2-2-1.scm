#lang sicp

; Exercises for SICP chapter 2.2.1

; Helpers pulled from earlier assignments or text
(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))

(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (map proc (cdr items)))))

; Exercise 2.17
(define (last-pair list1) (list-ref list1 (- (length list1) 1)))

(last-pair (list 23 72 149 34))
(last-pair (list 1))

; Exercise 2.18
(define (reverse list1)
  (if (= (length list1) 1)
      list1
      (append (reverse (cdr list1)) (cons (car list1) nil))))

(reverse (list 1 4 9 16 25))

; Exercise 2.19
(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination
                 coin-values))
            (cc (- amount
                   (first-denomination
                    coin-values))
                coin-values)))))

(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define (first-denomination coins) (car coins))
(define (except-first-denomination coins) (cdr coins))
(define (no-more? coins) (= (length coins) 0))

(cc 100 us-coins)
; The order in the coins list does not affect the answer, only effeciency of the algorithm.
; The invariant of answer = (answer-coin value) + (answer without that coin) does not depend
; on size of the coin.

; Exercise 2.20
(define (same-parity i . w)
  (define (same-parity-iter i o w)
    (if (= 0 (length w))
        o
        (if (= (remainder (car w) 2) (remainder i 2))
            (same-parity-iter i (append o (list (car w))) (cdr w))
            (same-parity-iter i o (cdr w)))))
  (same-parity-iter i (list i) w))

(same-parity 1 2 3 4 5 6 7)
(same-parity 2 3 4 5 6 7)

; Exercise 2.21
(define (square x) (* x x))

(define (square-list items)
  (if (null? items)
      nil
      (cons (square (car items)) (square-list (cdr items)))))

(define (square-list2 items)
  (map (lambda (x) (square x)) items))

(square-list (list 1 2 3 4))
(square-list2 (list 1 2 3 4))

; Exercise 2.22
; Louis's second method will not work because you are (cons answer...), which does not form a
; correct list structure. You would need to append the answer and square lists.

; Exercise 2.23
(define (for-each proc items)
  (if (null? items)
      #t
      ((proc (car items))
       (for-each proc (cdr items)))))

(for-each (lambda (x)
            (newline)
            (display x))
          (list 57 321 88))

