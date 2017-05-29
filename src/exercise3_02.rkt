#lang racket

(define (make-monitored mf)
  (let ((num 0))
    (define (dispatch m)
      (cond ((eq? m 'how-many-calls?) num)
            ((eq? m 'reset-count) (set! num 0) num)
            (else (begin (set! num (+ num 1))
                         (mf m)))))
    dispatch))

(define s (make-monitored sqrt))
(s 100)
(s 'how-many-calls?)