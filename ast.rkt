#lang racket

(provide ast-prim ast-block ast-bind ast-let)
(provide ast-prim? ast-block? ast-bind? ast-let?)
(provide ast-prim-name ast-block-body ast-bind-name
         ast-bind-body ast-let-name ast-let-arg ast-let-body)
(provide subst)

#|

type Expr = Word*

type Word
   = Prim String
   | Int
   | Var
   | Block Expr
   | Bind Var Expr
   | Let Var Expr Expr

Int = racket int
Var = racket string
Prim = struct ast-prim
Block = struct ast-block
Bind = struct ast-bind
Let = struct ast-let

|#

(struct ast-prim (name) #:transparent)
(struct ast-block (body) #:transparent)
(struct ast-bind (name body) #:transparent)
(struct ast-let (name arg body) #:transparent)

;; subst :: Var, Expr, Expr -> Expr
;; Substitutes the expression 'rep' for every free
;; occurrence of the variable 'name' in the target
;; expression 'target'.
;; We cannot simply replace 'name' with the substituted
;; expression; rather we must concatenate it with the expressions
;; on either side (since our expressions are lists, not trees).
;; That's why we have 'flatten'.
(define (subst name rep target)
  (define (flatten ls)
    (match ls
      [(list)
       (list)]
      [(list (list se ...) e ...)
       (append se (flatten e))]
      [(list x e ...)
       (cons x (flatten e))]))
  (define (subst-rec t)
    (cond
      [(list? t)
       (flatten (map (lambda (x) (subst-rec x)) t))]
      [(string? t)
       (if (eq? name t) rep t)]
      [(ast-block? t)
       (ast-block (subst-rec (ast-block-body t)))]
      [(ast-bind? t)
       (ast-bind (ast-bind-name t)
                 (if (eq? name (ast-bind-name t))
                     (ast-bind-body t)
                     (subst-rec (ast-bind-body t))))]
      [(ast-let? t)
       (ast-let (ast-let-name t)
                (subst-rec (ast-let-arg t))
                (if (eq? name (ast-let-name t))
                    (ast-let-body t)
                    (subst-rec (ast-let-body t))))]
      [#t t]))
  (subst-rec target))