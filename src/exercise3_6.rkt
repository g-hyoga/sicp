#lang racket
(require "package/random.rkt")

(define (rand-generater)
  (let ((x 1))
    (define (generate)
      (set! x (rand-update x))
      x)
    (define (reset v)
      (set! x v))
  
    (define (dispatch p)
      (cond ((eq? p 'generate) (generate))
            ((eq? p 'reset) reset)))
    dispatch))

(define gene (rand-generater))
(gene 'generate)
((gene 'reset) 1)
(gene 'generate)