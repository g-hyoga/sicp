#lang racket

(define (square v) (* v v))

(define (f g) (g 2))

(f square)

(f (lambda (z) (* z (+ z 1))))

(f f)

