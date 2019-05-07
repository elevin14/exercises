#lang sicp

; Exercises for SICP chapter 2.2.3

; Helpers pulled from earlier assignments or text
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (enumerate-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))

; Exercise 2.33
(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) nil sequence))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))

(append (list 1 2 3) (list 4 5 6))
(map (lambda (x) (+ x 1)) (list 1 2 3 4))
(length (list 1 2 3))

; Exerceise 2.34
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) (+ this-coeff (* x higher-terms)))
              0
              coefficient-sequence))

(horner-eval 2 (list 1 1 1 3))
(horner-eval 2 (list 1 3 0 5 0 1))

; Exercise 2.35
(define (count-leaves t)
  (accumulate + 0 (map (lambda (x) 1) (enumerate-tree t))))

(count-leaves (list 1 (list 2 (list 3 4)) 5))

; Exercise 2.36
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (accumulate (lambda (x y) (cons (car x) y)) nil seqs))
            (accumulate-n op init (accumulate (lambda (x y) (cons (cdr x) y)) nil seqs)))))

(accumulate-n + 0 (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))

