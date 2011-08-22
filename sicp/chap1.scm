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