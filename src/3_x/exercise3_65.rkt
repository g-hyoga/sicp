#lang racket

(require racket/stream)

(define (ln2-summands n)
  (stream-cons (/ 1.0 n)
               (stream-map - (ln2 (+ n 1)))))

(define ln2-stream
  (partical-sums (ln2-summands 1)))

(define (euler-transform s)
  (let ((s0 (stream-ref s 0))
        (s1 (stream-ref s 1))
        (s2 (stream-ref s 2)))
    (stream-cons (- s2 (/ (square (- s2 s1)))
                    (+ s0 (* -2 s1) s2)))
    (euler-transform (stream-rest s))))

(euler-transform ln2)
