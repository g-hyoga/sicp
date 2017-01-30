#lang racket

(define (square n) (* n n))

(define (factorial_1 n)
	(if (= n 1)
		1
		(* n (factorial_1 (- n 1)))))



(define (farctorial_2 n)
	(fact-iter 1 1 n))

(define (fact-iter product counter max-count)
	(if (> counter max-count)
		product
		(fact-iter (* counter product)
							 (+ counter 1)
							 max-count)))

(define (factorial n)
	(define (iter product counter)
		(if (> counter n)
			product
			(iter (* counter product) (+ counter 1))))
	(iter 1 1))


(define (fib_1 n)
	(cond ((= n 0) 0)
				((= n 1) 1)
				(else (+ fib_1(- n 1) + fib_1(- n 2)))))

(define (fib_2 n)
	(fib-iter 1 0 n))
(define (fib-iter a b count)
	(if (= count 0)
		b
		(fib-iter (+ a b) a (- count 1))))

(define (count-charge amount) (cc amount 5))
(define (cc amount kinds-of-coins)
	(cond ((= amount 0) 1)
				((or (< amount 0) (= kinds-of-coins 0)) 0)
				(else (+ (cc amount (- kinds-of-coins 1)) 
								 (cc (- amount (first-denomination kinds-of-coins))
												kinds-of-coins)))))
(define (first-denomination kinds-of-coins)
	(cond ((= kinds-of-coins 1) 1)
				((= kinds-of-coins 2) 5)
				((= kinds-of-coins 3) 10)
				((= kinds-of-coins 4) 25)
				((= kinds-of-coins 5) 50)))

(define (expt-1 b n)
	(if (= n 0)
		1
		(* b (expt-1 b (- n 1)))))

(define (expt-iter base counter product)
	(if (= 0 counter)
				product
				(expt-iter base (- counter 1) (* base product))))

(define (expt-2 b n)
	(expt-iter b n 1))

(define (fast-expt b n)
	(cond ((= n 0) 1) 
				((even?) (square (fast-expt b (/ n 2))))
				(else (* b (fast-expt b (- n 1))))))


(define (gcd a b)
	(if (= b 0)
		a
		(gcd b (remainder a b))))

(define (smallest-divisor n) (find-divisor n 2))

(define (find-divisor n test-divisor)
	(cond ((> (square test-divisor) n) n)
				((divides? test-divisor n) test-divisor)
				(else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b) (= (remainder b a) 0))

(define (prime? n)
	(= (smallest-divisor n)))

(define (expmod base exp m)
	(cond ((= exp 0) 1)
				((even? exp) (remainder (square (expmod base (/ exp 2) m)) m))
				(else (remainder (* base (expmod base (- exp 1) m)) m))))

(define (fermat-test n)
	(define (try-it a)
		(= (expmod a n n) a))
	(try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
	(cond ((= times 0) true)
				((fermat-test n) (fast-prime? n (- times 1)))
				(else false)))

(fast-prime? 127 10)
(fermat-test 127)

(define (f-iter n)
	(cond (= n 0) (display "end")
				(else (f-iter (- n 1)))))


(define (cube x) (* x x x))

(define (sum-intergers a b)
	(if (> a b)
		0
		(+ a (sum-intergers (+ a 1) b))))

(define (sum-cubes a b)
	(if (> a b)
		0
		(+ (cube a)
			 (sum-cube (+ a 1) b))))

(define (pi-sum a b)
	(if (> a b)
		0
		(+ (/ 1.0 (* a (+ a 2a)))
			 (pi-sum (+ a 4) b))))

(define (sum term a next b)
	(if (> a b)
		0
		(+ (term a)
			 (sum term (next a) next b))))

(define (sum-cubes2 a b)
	(sum cube a inc b))

(define (intergral f a b dx)
	(define (add-dx x)
		(+ x dx))
	(* (sum f (+ a (/ dx 2.0)) add-dx b)
		 dx))

(define (simpson-intergral f a b dx))

(define (pi-sum a b)
	(sum (lambda (x) (/ 1.0 (* x (+ x 2))))
			 a
			 (lambda (x) (+ x 4))
			 b))

(define (integral f a b dx)
	(* (sum f
					(+ a (/ dx 2.0))
					(lambda (x) (+ x dx))
					b)
		 dx))

(define (search f neg-point pos-point)
	(let ((midpoint (average neg-point pos-point)))
		(if (close-enogh? neg-point pos-point)
			midpoint
			(let ((test-value (midpoint)))
				(cond ((positive? test-value) (search f neg-point midpoint))
							((negative? test-value) (search f midpoint pos-point))
							(else midpoint))))))

(define (close-enogh? x y) (< (abs (- x y)) 0.001))

(define (half-interval-method f a b)
	(let ((a-value (f a))
				(b-value (f b)))
		(cond ((and (negative? a-value) (positive? b-value)) (search f a b))
					((and (negative? b-value) (positive? a-value)) (search f b a))
					(else (error "Values are not of opposite sign" a b)))))

(half-interval-method sin 2.0 4.0)






















