#lang info

(define name "elements")
(define deps '("base"
               "rackunit-lib"
               "syntax-spec-v3"
               "syntax/parse"
               "syntax/to-string"
               "racket/hash"))
(define license 'MIT)
(define scribblings '(("scribblings/main.scrbl" () (experimental) "elements")))