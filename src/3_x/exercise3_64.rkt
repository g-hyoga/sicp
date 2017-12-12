#lang racket

(require racket/stream)

(define (average x y)
  (/ x y))

(define (sqrt-improbe guess x)
  (average guess (/ x guess)))

(define (sqrt-stream x)
  (define guesses
    (stream-cons
      1.0
      (stream-map (lambda (guess) (sqrt-improve guess x))
                  guesses)))
  guesses)

(define (stream-limit s t)
  (let ((first (stream-first s))
        (second (stream-first (stream-rest s))))
    (if (< (abs (- second first)) t)
      second
      (stream-limit (stream-rest s) t))))

(define (sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance))
