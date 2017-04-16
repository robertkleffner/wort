#lang br/quicklang

(require "ast.rkt")
(require "eval.rkt")
(require "infer.rkt")

(define-macro (wort-module-begin PARSE-TREE)
  #'(#%module-begin PARSE-TREE))
(provide (rename-out [wort-module-begin #%module-begin]))

;; we reverse the returned evaluation stack to make it
;; easier to read. if you want to use the result stack
;; as a stack in racket, you probably don't want to reverse
;; it first; as the value on top of the stack will be
;; the first element in the list.
(define-macro (wrt-program EXPR)
  #'(reverse (eval-wort (check-type EXPR))))
(provide wrt-program)

(define-macro (wrt-expr WORDS ...)
  #'(list WORDS ...))
(provide wrt-expr)

(define-macro (wrt-word WORD)
  #'WORD)
(provide wrt-word)

(define-macro (wrt-prim PRIM)
  #'(cond
      [(eq? PRIM "true") #t]
      [(eq? PRIM "false") #f]
      [#t (ast-prim PRIM)]))
(provide wrt-prim)

(define-macro (wrt-block "{" WORDS "}")
  #'(ast-block WORDS))
(provide wrt-block)

(define-macro (wrt-bind "bind" VAR "(" WORDS ")")
  #'(ast-bind VAR WORDS))
(provide wrt-bind)

(define-macro (wrt-let "let" VAR "=" ARGS "(" BODY ")")
  #'(ast-let VAR ARGS BODY))
(provide wrt-let)