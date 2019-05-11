#lang sicp
(#%require sicp-pict)
(#%require graphics/graphics)
(open-graphics)
(define vp (open-viewport "A Picture Language" 500 500))


; Exercises for SICP chapter 2.2.4

; Helpers pulled online for graphics
(define draw (draw-viewport vp))
(define (clear) ((clear-viewport vp)))
(define line (draw-line vp))

(define (vector-to-posn v)
  (make-posn (* 500 (car v)) (* 500 (cdr v))))

(define (segments->painter segment-list)   
  (lambda (frame)     
    (for-each     
     (lambda (segment)        
       (line         
        (vector-to-posn ((frame-coord-map frame) (start-segment segment)))         
        (vector-to-posn ((frame-coord-map frame) (end-segment segment)))))      
     segment-list)))

; Helpers pulled from earlier assignments or text
(define shade diagonal-shading)

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))

(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v) (edge1-frame frame))
               (scale-vect (ycor-vect v) (edge2-frame frame))))))

;(define (segments->painter segment-list)
;  (lambda (frame)
;    (for-each
;     (lambda (segment)
;       (line
;        ((frame-coord-map frame)
;         (start-segment segment))
;        ((frame-coord-map frame)
;         (end-segment segment))))
;     segment-list)))

(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter (make-frame
                  new-origin
                  (sub-vect (m corner1) new-origin)
                  (sub-vect (m corner2) new-origin)))))))

; Exercise 2.44
(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

(paint (corner-split shade 3))

; Exercise 2.45
(define (split a b)
  (lambda (painter n)
    (if (= n 0)
        painter
        (let ((smaller ((split a b) painter (- n 1))))
          (a painter (b smaller smaller))))))

(define right-split2 (split beside below))
(define up-split2 (split below beside))

(define (corner-split2 painter n)
  (if (= n 0)
      painter
      (let ((up (up-split2 painter (- n 1)))
            (right (right-split2 painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split2 painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))

(paint (corner-split2 shade 3))

; Exerceise 2.46
(define (make-vect x y) (cons x y))
(define (xcor-vect v) (car v))
(define (ycor-vect v) (cdr v))

(define (add-vect v w)
  (make-vect
   (+ (xcor-vect v) (xcor-vect w))
   (+ (ycor-vect v) (ycor-vect w))))

(define (sub-vect v w)
  (make-vect
   (- (xcor-vect v) (xcor-vect w))
   (- (ycor-vect v) (ycor-vect w))))

(define (scale-vect s v)
  (make-vect
   (* s (xcor-vect v))
   (* s (ycor-vect v))))



; Exerceise 2.47
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))
(define (origin-frame f) (car f))
(define (edge1-frame f) (cadr f))
(define (edge2-frame f) (caddr f))

(define (make-frame2 origin edge1 edge2)
  (cons origin (cons edge1 edge2)))
(define (origin-frame2 f) (car f))
(define (edge1-frame2 f) (cadr f))
(define (edge2-frame2 f) (cddr f))

; Exercise 2.48
(define (make-segment v1 v2) (cons v1 v2))
(define (start-segment s) (car s))
(define (end-segment s) (cdr s))

; Exercise 2.49
; Had to use draw functionality. Coordinate plane not quite working as expected.
(define standard-frame (make-frame (make-vect 0 0) (make-vect 1 0) (make-vect 0 1)))

((segments->painter (list (make-segment (make-vect 0 0) (make-vect 0 1))
                          (make-segment (make-vect 0 1) (make-vect 1 1))
                          (make-segment (make-vect 1 1) (make-vect 1 0))
                          (make-segment (make-vect 1 0) (make-vect 0 0))))
 standard-frame)

((segments->painter (list (make-segment (make-vect 0 0) (make-vect 1 1))
                          (make-segment (make-vect 0 1) (make-vect 1 0))))
 standard-frame)

((segments->painter (list (make-segment (make-vect 0 .5) (make-vect .5 1))
                          (make-segment (make-vect .5 1) (make-vect 1 .5))
                          (make-segment (make-vect 1 .5) (make-vect .5 0))
                          (make-segment (make-vect .5 0) (make-vect 0 .5))))
 standard-frame)

; Exercise 2.50
(define (flip-horiz painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

(define (rotate180 painter)
  (transform-painter painter
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 0.0)))

(define (rotate270 painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

; Exercise 2.51
(define (below painter)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-upper
           (transform-painter
            painter1
            split-point
            (make-vect 1.0 0.5)
            (make-vect 0.0 1.0)))
          (paint-lower
           (transform-painter
            painter2
            (make-vect 0.0 0.0)
            (make-vect 1.0 0.0)
            split-point)))
      (lambda (frame)
        (paint-upper frame)
        (paint-lower frame)))))

(define (below2 painter)
  (rotate90 (beside (rotate270 painter) (rotate270 painter))))