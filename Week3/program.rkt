(define (nth-beast-number n)
  (string->number (string-repeat "666" n)))
  
(define (next-hack-number n)
  (define (hack? x)
    (if (and (palindrome? (binary x)) (= (remainder(occurrences 1 (binary x)) 2)1) )
        x
        (hack? (add1 x))))
  (hack? (add1 n)))

(define (p_score n)
  (define (helper x result)
    (cond
      [(palindrome? x) (add1 result)]
      [else (helper (+ x (reverse-int x)) (add1 result))]))
  (helper n 0))
