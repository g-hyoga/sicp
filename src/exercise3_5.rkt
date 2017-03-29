#lang racket

(define (rand) (* 1.0 (random 123456789)))

(define (estimate-pi trials)
  (sqrt (/ 6 (monte-carlo trials cesaro-test))))

(define (cesaro-test)
  (= (gcd (rand) (rand)) 1))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1)
                 (+ trials-passed 1)))
          (else (iter (- trials-remaining 1)
                      trials-passed))))
  (iter trials 0))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (* (/ (rand) 123456789) range))))

(define (estimate-integral p x1 x2 y1 y2 trials)
  (* (monte-carlo trials p)
     (* (abs (- x2 x1)) (abs (- y2 y1)))))
  
(define (square x) (* x x))
(define (test)
  (< (+ (square (random-in-range -1 1)) (square (random-in-range -1 1)))
     (square 1)))
(define s (estimate-integral test -1.0 1.0 -1.0 1.0 1000000))

(/ s (square 1.0))
(estimate-pi 1000000)
