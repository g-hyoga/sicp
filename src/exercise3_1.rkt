#lang racket 

(define (make-accumulator sum)
  (lambda (n)
    (begin (set! sum (+ sum n))
           sum)))

(define sum (make-accumulator 10))
(sum 5)
(sum 10)

  




