#lang racket
(define (product-digits n)
  (cond
  [(< n 10) n]
  [ else (* (remainder n 10) (product-digits (quotient n 10)))]))
