#lang info
(define collection "wort")
(define deps '("base"
               "rackunit-lib"
               "brag"
               "beautiful-racket"))
(define build-deps '("scribble-lib" "racket-doc"))
(define scribblings '(("scribblings/wort.scrbl" ())))
(define pkg-desc "A core concatenative language with type inference")
