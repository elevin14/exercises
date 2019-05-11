#lang sicp

; Exercises for SICP chapter 2.3.1

; Helpers pulled from earlier assignments or text
(define (memq item x)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))

; Exercise 2.53
(list 'a 'b 'c)
(list (list 'george))
(cdr '((x1 x2) (y1 y2)))
(cadr '((x1 x2) (y1 y2)))
(pair? (car '(a short list)))
(memq 'red '((red shoes) (blue socks)))
(memq 'red '(red shoes blue socks))

; Exercise 2.54
(define (equal? a b)
  (if (and (pair? a) (pair? b))
      (and (equal? (car a) (car b)) (equal? (cdr a) (cdr b)))
      (eq? a b)))

(equal? '(this is a list) '(this is a list))
(equal? '(this is a list) '(this (is a) list))

; Exercise 2.55
(car ''abracadabra)
; The first quote makes the phase 'abracadabra evalutate to symbols instead of expressions.
; So (quote (quote abc)), or '(quote abc). The car of that is quote.