#lang racket

(require racket/stream)
(define stream-null? stream-empty?)

(define the-empty-stream empty-stream)

(define (stream-enumerate-interval low high)
  (if (> low high)
    the-empty-stream
    (stream-cons
      low
      (stream-enumerate-interval (+ low 1) high))))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor))
              stream))

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
    the-empty-stream
    (stream-cons
      (apply proc (map stream-first argstreams)) 
      (apply stream-map 
             (cons proc (map stream-rest argstreams))))))

(define (add-stream s1 s2) (stream-map + s1 s2))

(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else 
          (let ((s1car (stream-first s1))
                (s2car (stream-first s2)))
            (cond ((< s1car s2car)
                   (stream-cons
                     s1car
                     (merge (stream-rest s1) s2)))
                  ((> s1car s2car)
                   (stream-cons
                     s2car
                     (merge s1 (stream-rest s2))))
                  (else
                    (stream-cons
                      s1car
                      (merge (stream-rest s1) (stream-rest s2)))))))))

(define s (stream-cons 1 (merge (scale-stream s 2)
                                (merge (scale-stream s 3) (scale-stream s 5)))))

(define (mul-series s1 s2)
  (stream-cons (* (stream-first s1) (stream-first s2)) 
               (add-stream ??
                           ??)))

(define s1 (stream-enumerate-interval 0 5))
(define s2 (stream-enumerate-interval 5 10))

(stream-ref (mul-series s1 s2) 0)
(stream-ref (mul-series s1 s2) 1)
(stream-ref (mul-series s1 s2) 2)
(stream-ref (mul-series s1 s2) 3)

