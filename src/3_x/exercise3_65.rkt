#lang racket

(require racket/stream)

(define (ln2 n))

(define (sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance))
