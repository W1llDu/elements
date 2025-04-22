#lang info

(define name "elements")
(define deps '("base"
               "rackunit-lib"
               "syntax-spec-v3"
               #;"syntax/parse"
               #;"scribble/example"
               #;"racket/hash"
               #;"syntax/to-string"))
(define license 'MIT)
(define scribblings '(("scribblings/main.scrbl" () (experimental) "elements")))