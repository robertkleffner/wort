#lang racket

(require "ast.rkt")

;; ===================================================
;; ===================================================
;; STACK MACHINE EVALUATION
;; ===================================================
;; ===================================================

;; type Expression = Word
;; An expression is a list of operators, commonly called
;; 'words' in Forth and related languages.

;; type MachineState = (Stack Value, Expression)
;; A machine consists of:
;; 1. a stack of values, which will end up as the result
;;    of evaluation
;; 2. an expression to evaluate
(struct machine (stack expr) #:transparent)

;; eval-barley :: Expression -> Stack | Error
;; Runs the expression e on an initial (empty)
;; stack. The machine terminates when the
;; expression is 'empty' (expressions are just
;; a list of operators). If no transition rule
;; can be applied and the expression part is not
;; empty, then the machine is 'stuck'.
(define (eval-wort e)
  ;; step-eval :: MachineState -> Stack | Error
  (define (step-eval state)
    (if (empty? (machine-expr state))
        (machine-stack state)
        (step-eval (step state))))
  ;; step :: MachineState -> MachineState | Error
  (define (step state)
    (displayln state)
    (match state
      ;; Pushing values
      [(machine s (list c e ...))
       #:when (or (number? c)
                  (boolean? c)
                  (ast-block? c))
       (machine (cons c s) e)]
      
      ;; Value binding
      [(machine (list v s ...) (list (ast-bind name body) e ...))
       (machine s (append (subst name (list v) body) e))]
      
      ;; Let binding
      [(machine s (list (ast-let name arg body) e ...))
       (machine s (append (subst name arg body) e))]
      
      ;; Primitives
      [(machine (list n1 n2 s ...) (list (ast-prim "add") e ...))
       (machine (list* (+ n1 n2) s) e)]
      
      [(machine (list (ast-block exp) s ...) (list (ast-prim "call") e ...))
       (machine s (append exp e))]
      
      [(machine (list (ast-block exp) s ...) (list (ast-prim "fix") e ...))
       (machine (list (ast-block (list* (ast-block exp) (ast-prim "fix"))) s)
                (append exp e))]
      
      [(machine (list #t v1 v2 s ...) (list (ast-prim "if") e ...))
       (machine (list* v1 s) e)]
      
      [(machine (list #f v1 v2 s ...) (list (ast-prim "if") e ...))
       (machine (list* v2 s) e)]
      
      [(machine (list n1 n2 s ...) (list (ast-prim "eq") e ...))
       #:when (and (number? n1) (number? n2))
       (machine (list* (eq? n1 n2) s) e)]
      
      [(machine (list n1 n2 s ...) (list (ast-prim "less") e ...))
       #:when (and (number? n1) (number? n2))
       (machine (list* (< n2 n1) s) e)]

      ;; Stuck (should never occur if inference passes!)
      [_
       (error "looks like we're stuck!")]))
    
      
  (step-eval (machine (list) e)))
(provide eval-wort)