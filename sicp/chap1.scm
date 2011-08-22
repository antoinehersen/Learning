#lang racket
(define (abs x)
  (cond ((> x 0) x)
        ((= 0 x) 0)
        ((< x 0) (- x))))

(define (abs2 x)
  (cond ((< x 0) (- x))
        (else x)))

(define (p) (p))


;; Ch 1.1.7 Square root by Newton's Method


(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve-guess guess x) x)))

(define (good-enough? guess x)
  (< (abs (- x (* guess guess))) 0.001))

(define (improve-guess guess x)
  (/ (+ (/ x guess) guess) 2))

(define (square x)
  (* x x))

(define (average a b)
  (/ (+ a b) 2))

;; block structure definition
(define (sqrt x)
  (define (good-enough? guess x)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess x)
    (average guess (/ x guess)))
  (define (sqrt-iter guess x)
    (if (good-enough? guess x)
        guess
        (sqrt-iter (improve guess x) x)))
  (sqrt-iter 1.0 x))

;; We can get rid of the x since it is already in scope

(define (sqrt-nox x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))



;; 1.2  Procedures and the Processes They Generate

(define (fact x)
(define (fact-iter prod curr)
  (if (> curr x)
      prod
      (fact-iter (* prod curr) (+ 1 curr))))
(fact-iter 1 1))


;; Ackerman function
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

;;  f(n) = n if n<3 and f(n) = f(n - 1) + 2f(n - 2) + 3f(n - 3) if n> 3.

(define (f n)
  (if (< n 3)
      n
      (+ (f (- n 1)) (* 2 (f (- n 2))) (* 3 (f (- n 3))))))

(define (f2 n)
(define (f-iter a b c count)
  (if (= count 2 )
      c
      (f-iter b c (+ c (* 2 b) (* 3 a)) (- count 1))))
  (f-iter 0 1 2 n))
