#lang racket

(define (prime? n)
  (define (helper i)
    (cond
      [(= n 1) false]
      [(= i 1) true]
      [else
       (cond
         [(= (remainder n i) 0) false]
         [else (helper (sub1 i))])]))
 
(helper (sub1 n) ))
