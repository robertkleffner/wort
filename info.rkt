#lang info
(define collection "wort")
(define deps '("base"
               "rackunit-lib"
               "brag"
               "br"))
(define build-deps '("scribble-lib" "racket-doc"))
(define scribblings '(("scribblings/wort.scrbl" ())))
(define pkg-desc "A core concatenative language with type inference")
