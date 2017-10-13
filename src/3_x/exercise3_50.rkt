#lang racket

;-----------------------------------------------------------------

(require racket/stream)

(define strem-null? stream-empty?)

(define the-empty-stream (stream '()))

(define (cons-stream a b)
  (stream-cons a b))

(define stream-car stream-first)
(define stream-cdr stream-rest)

(define (memo-proc proc)
  (let ((already-run? #f) (result #f))
    (lambda ()
      (if (not already-run?)
          (begin (set! result (proc))
                 (set! already-run? true)
                 result)
          result))))

(define (delay delayed-object)
  (memo-proc delayed-object))

(define (stream-map proc . argstreams)
  (if (strem-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1) high))))

;-----------------------------------------------------------------


(define s (stream-enumerate-interval 1 2))

(stream-for-each (lambda (x) 
                   (display x)
                   (display "\n"))
                 (stream-map (lambda (x)
                                (+ x x))
                             s))


