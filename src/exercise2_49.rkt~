#lang racket
(require sicp-pict)
(define kirby (load-painter "img/kirby.gif"))

(define (flipped-pairs painter)
  (let ((painter2 (beside painter (flip-vert painter))))
    (below painter2 painter2)))

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

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

#;(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))

;?????
(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

(define (fipped-pairs painter)
  (let ((combine4 (square-of-four identity flip-vert
                                  identity flip-vert)))
    (combine4 painter)))

(define (square-limit painter n)
  (let ((combine4 (square-of-four flip-horiz identity
                                  rotate180 flip-vert)))
    (combine4 (corner-split painter n))))

;(paint (square-limit kirby 1))

(define (split 1th-divide 2th-divide)
  (lambda (painter n)
    (if (= n 0)
        painter
        (let ((smaller ((split 1th-divide 2th-divide) painter (- n 1))))
          (1th-divide painter (2th-divide smaller smaller))))))

(define right-split2 (split beside below))
(define up-split2 (split below beside))

#;(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (original-frame frame)
     (add-vect (scale-vect (xcor-vect v)
                           (edge-vect frame))
               (scale-vect (ycor-vect v)
                           (edge2-frame frame))))))

(define (make-vect x y) (cons x y))
(define (xcor-vect v) (car v))
(define (ycor-vect v) (cdr v))
(define (add-vect v1 v2) (cons (+ (xcor-vect v1) (xcor-vect v2)) (+ (ycor-vect v1) (ycor-vect v2))))
(define (sub-vect v1 v2) (cons (- (xcor-vect v1) (xcor-vect v2)) (- (ycor-vect v1) (ycor-vect v2))))
(define (scala-vect s v) (cons (* s (xcor-vect v)) (* s (ycor-vect v))))

(define (make-frame1 origin edge1 edge2)
  (list origin edge1 edge2))

(define (make-frame2 origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define (origin1 frame)
  (car frame))
(define (edge1-1 frame)
  (cadr frame))
(define (edge1-2 frame)
  (caddr frame))

(define (origin2 frame)
  (car frame))
(define (edge2-1 frame)
  (car (cdr frame)))
(define (edge2-2 frame)
  (cdr (cdr frame)))

(define (make-segment v1 v2) (cons v1 v2))
(define (start-segment s) (car s))
(define (end-segment s) (cdr s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (outline)
  (lambda (painter)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define v1 (make-vect 1 2))
(define v2 (make-vect 3 4))
(define v3 (make-vect 5 6))

(define frame1 (make-frame1 v1 v2 v3))
(define frame2 (make-frame2 v1 v2 v3))

