#lang racket

(require racket/stream)

(define (sqrt-improbe guess x)
  (average guess (/ x guess)))

(define (sqrt-stream x)
  (define guesses
    (stream-cons
      1.0
      (stream-map (lambda (guess) (sqrt-improve guess x))
                  guesses))
    guesses))



