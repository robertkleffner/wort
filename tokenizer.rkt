#lang br
(require "lexer.rkt")

(define (make-tokenizer ip [path #f])
  (port-count-lines! ip)
  (define (next-token) (wort-lexer ip))
  next-token)

(provide make-tokenizer)