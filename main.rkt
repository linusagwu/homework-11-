#lang racket
(require "utility.rkt")
(require "parser.rkt")
(require "runner.rkt")

(define env '((a 1) (b 2) (c 5)))
(resolve env 'a)

(define sample-code '(call (function () (ask (bool != a b) (math - a b) (math + a b))) (a)))
;(define sample-code '(call (function (x y z) (math + (math + x y) z)) (2 a (math + 1 1))))
(display (neo-parser sample-code))
(define parsed-neo-code (neo-parser sample-code))
(run-neo-parsed-code parsed-neo-code env)
