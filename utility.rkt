#lang racket
(define elementAt
  (lambda (lst index)
    (cond
      ((not (list? lst)) "this is not a list")
      ((null? lst) "this is an empty list or index out of bound.")
      ((equal? index 0) (car lst))
      (else (elementAt (cdr lst) (- index 1)))
        )
    )
  )
                  
(define run-bool-exp
  (lambda (op num1 num2)
    (cond
      ((equal? op '>) (> num1 num2))
      ((equal? op '<) (< num1 num2))
      ((equal? op '>=) (>= num1 num2))
      ((equal? op '<=) (<= num1 num2))
      ((equal? op '==) (= num1 num2))
      ((equal? op '&&) (and num1 num2))
      ((equal? op '||) (or num1 num2))
      (else (not num1))
      )
    )
  )

(define run-math-exp
  (lambda (op num1 num2)
    (cond
      ((equal? op '+) (+ num1 num2))
      ((equal? op '-) (- num1 num2)) 
      ((equal? op '*) (* num1 num2)) 
      ((equal? op '/) (/ num1 num2)) 
      ((equal? op '//) (quotient num1 num2))  
      ((equal? op '%) (modulo num1 num2))  
      (else #false)
      )
    )
  )

(provide (all-defined-out))