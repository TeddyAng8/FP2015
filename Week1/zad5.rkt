#lang racket

(define (square x) (* x x x))

(define (sum-of-squares n)
  (define (sum-of-squares-h k)
    (cond ((= k n)
           #f)
          ((= n (+ (square (floor (sqrt k)))
                   (square (floor (sqrt (- n k))))))
           #t)
          (else
           (sum-of-squares-h (+ k 1)))))
  (sum-of-squares-h 1))
