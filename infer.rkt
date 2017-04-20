#lang racket

(require "ast.rkt")

(define (check-type expr)
  (reset-var)
  (let ([t (second (infer (gamma empty) expr))])
    (displayln (string-append "inferred type: " (pretty-type t)))
    (unify-ind t (fun-type (list) (list (fresh-seq))))
    expr))
(provide check-type)

(module+ test
  (require rackunit))

;; type Expr = Word*

#|
type Type
    = Int
    | Bool
    | IVar String
    | SVar String
    | Fun [Type] [Type]

type Scheme = Scheme [SVar | IVar] Type

One of the invariants we maintain throughout the
type system is that sequence variables can only
occur as the last element in a list of types.
|#
(struct ivar (n) #:transparent)
(struct svar (n) #:transparent)
(struct fun-type (in out) #:transparent)
(struct prim-type (name) #:transparent)

(struct scheme (vars mono) #:transparent)

(define (pretty-type t)
  (match t
   [(ivar n) n]
   [(svar n) (string-append n "...")]
   [(fun-type is os) (string-append "(" (pretty-type is) " -> " (pretty-type os) ")")]
   [(list ts ...)
    (string-join (map pretty-type ts) " ")]
   [(prim-type p) (string-titlecase p)]
   [(scheme vs t) (string-append "forall "
                                 (string-join (map pretty-type vs) ",")
                                 "."
                                 (pretty-type t))]))

;; get-name : SVar | IVar -> String
(define (get-name v)
  (match v
    [(ivar n) n]
    [(svar n) n]))

;; get-scheme-vnames : Scheme -> [String]
(define (get-scheme-vnames s)
  (map get-name (scheme-vars s)))

;; type TypeEnvironment = [(Var,Scheme)]
;; Another invariant we maintain is that all variables
;; have a function type in the environment.
(struct gamma (vs) #:transparent)
(define (extend-env n t env)
  (gamma (list* (list n t) (gamma-vs env))))

;; ftv :: Type | Scheme | Gamma -> [String]
(define (ftv t)
  (match t
    [(list ts ...) (append* (map ftv ts))]
    [(ivar n) (list (ivar n))]
    [(svar n) (list (svar n))]
    [(fun-type in out) (append (ftv in) (ftv out))]
    [(scheme vs mono) (filter (lambda (v) (not (member v vs))) (ftv mono))]
    [(gamma vs) (append* (map (lambda (s) (ftv (second s))) vs))]
    [_ empty]))

(module+ test
  (check-equal? (list) (ftv (list (prim-type "int"))))
  (check-equal? (list (ivar "n") (svar "a"))
                (ftv (list (ivar "n") (svar "a"))))
  (check-equal? (list (ivar "n") (svar "a"))
                (ftv (fun-type (list (ivar "n")) (list (svar "a")))))
  (check-equal? (list (svar "a"))
                (ftv (scheme (list (ivar "n"))
                             (fun-type (list (ivar "n"))
                                       (list (svar "a"))))))
  (check-equal? (list (svar "a"))
                (ftv (gamma (list (list "x"
                                        (scheme (list (ivar "n"))
                                                (fun-type (list (ivar "n"))
                                                          (list (svar "a"))))))))))

;; ===================================================
;; ===================================================
;; SUBSTITUTIONS
;; ===================================================
;; ===================================================

;; type Subst = [(String,Seq)]

;; subst : Subst, (Type | Seq | Gamma | Scheme) -> (Type | Seq | Gamma | Scheme)
(define (subst s t)
  (define (flatten ls)
    (match ls
      [(list) (list)]
      [(list (list e1 ...) e2 ...) (append e1 (flatten e2))]
      [(list t e2 ...) (list* t (flatten e2))]))
  
  ;; in-scheme : [String] -> (String, Seq) -> Bool
  (define (not-in-scheme vs)
    (lambda (vp)
      (not (member (car vp) vs))))
  
  (cond
    [(list? t)
     (flatten (map (lambda (x) (subst s x)) t))]

    [(fun-type? t)
     (fun-type (subst s (fun-type-in t))
               (subst s (fun-type-out t)))]

    [(ivar? t)
     (match (assoc (ivar-n t) s)
       [#f t]
       [other (second other)])]

    [(svar? t)
     (match (assoc (svar-n t) s)
       [#f t]
       [other (second other)])]

    [(scheme? t)
     (scheme
      (scheme-vars t)
      (subst (filter (not-in-scheme (get-scheme-vnames t)) s) (scheme-mono t)))] 

    [(gamma? t)
     (gamma (map (lambda (x)
                   (list (first x) (subst s (second x))))
                 (gamma-vs t)))]

    [#t t]))

(module+ test
  (check-equal?
   (subst (list (list "a" (list (svar "b")))) (list (svar "a")))
   (list (svar "b"))))

;; compose-subst : Subst, Subst -> Subst
(define (compose-subst s1 s2)
  (define (combine-subs s1 s2)
    (match s2
      [(list) s1]
      [(list (list v t) s2s ...)
       (if (assoc v s1)
           (combine-subs s1 s2s)
           (list* (list v t) (combine-subs s1 s2s)))]))
  (define (sub-in-type s)
    (lambda (pair)
      (list (first pair) (subst s (second pair)))))
  (combine-subs (map (sub-in-type s1) s2) s1))
  

;; ===================================================
;; ===================================================
;; TYPE INFERENCE
;; ===================================================
;; ===================================================

(define ind 0)
(define (reset-var) (set! ind 0))
;; fresh-var : -> String
(define (fresh-var)
  (let ([x (string-append "a" (number->string ind))])
    (set! ind (add1 ind))
    x))
(define (fresh-ind) (ivar (fresh-var)))
(define (fresh-seq) (svar (fresh-var)))

;; infer : TypeEnvironment, Expr -> (Subst, Type)
(define (infer env expr)
  (match expr
    [(list)
     (let ([a (fresh-seq)])
       (list empty (fun-type (list a) (list a))))]
    [(list e ... w)
     (let* ([r1 (infer env e)]
            [s1 (first r1)]
            [t1 (second r1)] 
            [r2 (infer-word (subst s1 env) w)]
            [s2 (first r2)]
            [t2 (second r2)]
            [phi (unify (fun-type-out t1) (fun-type-in t2))])
       (list (compose-subst (compose-subst phi s2) s1)
             (subst phi (fun-type (fun-type-in t1) (fun-type-out t2)))))]))

(module+ test
  (check-equal?
   (second (infer (gamma (list)) (list (ast-prim "call"))))
   (fun-type (list (svar "a1") (fun-type (list (svar "a1")) (list (svar "a2"))))
             (list (svar "a2"))))
  (reset-var)
  (check-equal?
   (second (infer (gamma (list)) (list (ast-bind "x" (list "x")))))
   (fun-type (list (svar "a4") (ivar "a2"))
             (list (svar "a4") (ivar "a2"))))
  (reset-var)
  (check-equal?
   (second (infer (gamma (list)) (list (ast-let "x" (list (ast-prim "add")) (list "x")))))
   (fun-type (list (svar "a4") (prim-type "int") (prim-type "int"))
             (list (svar "a4") (prim-type "int")))))

;; infer-word : TypeEnvironment, Word -> (Subst, Type)
(define (infer-word env word)
  (match word
    ;; numeric constant
    [c #:when (number? c)
       (let ([a (fresh-seq)])
         (list empty (fun-type (list a) (list a (prim-type "int")))))]

    ;; boolean constant
    [c #:when (boolean? c)
       (let ([a (fresh-seq)])
         (list empty (fun-type (list a) (list a (prim-type "bool")))))]

    ;; block constant
    [(ast-block e)
     (let* ([r1 (infer env e)]
            [s1 (first r1)]
            [t1 (second r1)]
            [a (fresh-seq)])
       (list s1 (fun-type (list a) (list a t1))))]

    ;; primitive word
    [(ast-prim n)
     (list empty (infer-prim n))]

    ;; variable reference
    [c #:when (string? c)
       (list empty (inst (second (assoc c (gamma-vs env)))))]

    ;; value binding
    [(ast-bind n e)
     (let* ([a (fresh-seq)]
            [b (fresh-ind)]
            [r1 (infer (extend-env n
                                   (scheme (list a) (fun-type (list a) (list a b)))
                                   env)
                       e)]
            [s1 (first r1)]
            [t1 (second r1)])
       (list s1 (fun-type (append (fun-type-in t1) (flatten (list (subst s1 b))))
                          (fun-type-out t1))))]

    ;; expression binding
    [(ast-let n e1 e2)
     (let* ([r1 (infer env e1)]
            [s1 (first r1)]
            [t1 (second r1)]
            [envp (subst s1 env)]
            [r2 (infer (extend-env n (gen envp t1) envp) e2)]
            [s2 (first r2)]
            [t2 (second r2)])
       (list (compose-subst s2 s1) t2))]))

;; infer-prim : String -> Type
(define (infer-prim name)
  (match name
    ["add"
     (let ([a (fresh-seq)])
       (fun-type (list a (prim-type "int") (prim-type "int"))
                 (list a (prim-type "int"))))]
    ["call"
     (let ([a (fresh-seq)]
           [b (fresh-seq)])
       (fun-type (list a (fun-type (list a) (list b))) (list b)))]
    ["fix"
     (let ([a (fresh-seq)]
           [b (fresh-seq)])
       (fun-type (list a (fun-type (list a (fun-type (list a) (list b))) (list b)))
                 (list b)))]
    ["if"
     (let ([a (fresh-seq)]
           [b (fresh-ind)])
       (fun-type (list a b b (prim-type "bool")) (list a b)))]
    ["eq"
     (let ([a (fresh-seq)])
       (fun-type (list a (prim-type "int") (prim-type "int"))
                 (list a (prim-type "bool"))))]
    ["less"
     (let ([a (fresh-seq)])
       (fun-type (list a (prim-type "int") (prim-type "int"))
                 (list a (prim-type "bool"))))]))

;; inst : Scheme | Type -> Type
(define (inst sch)
  (define (freshen v)
    (match v
      [(svar s) (list s (list (fresh-seq)))]
      [(ivar s) (list s (list (fresh-ind)))]))
  (match sch
    [(scheme vs t)
     (let ([fresh (map (lambda (v) (freshen v)) vs)])
       (subst fresh t))]
    [t t]))

(module+ test
  (reset-var)
  (check-equal? (inst (scheme (list (ivar "a")) (list (ivar "a"))))
                (list (ivar "a0")))
  (reset-var)
  (check-equal? (inst (scheme (list) (list (ivar "a"))))
                (list (ivar "a"))))

;; gen : TypeEnvironment, Type -> Scheme
(define (gen env ty)
  (let ([env-ftv (ftv env)])
    (scheme (filter (lambda (x) (not (member x env-ftv))) (ftv ty)) ty)))

(module+ test
  (check-equal? (gen (gamma (list)) (list (ivar "a")))
                (scheme (list (ivar "a")) (list (ivar "a"))))
  (check-equal? (gen (gamma (list (list "x" (fun-type (list) (list (ivar "a"))))))
                     (list (ivar "a")))
                (scheme (list) (list (ivar "a")))))

;; ===================================================
;; ===================================================
;; UNIFICATION
;; ===================================================
;; ===================================================

;; unify : Seq, Seq -> Subst | Error
(define (unify s1 s2)
  (match* (s1 s2)
    ;; both sequences empty
    [((list) (list))
     (list)]

    ;; if two individual types are equal, their unifier
    ;; is empty, so just unify the rest of the sequences
    [((list t1s ... t1) (list t2s ... t2))
     #:when (equal? t1 t2)
     (unify t1s t2s)]

    ;; sequence variable unifies with any sequence that
    ;; doesn't contain that variable
    [((list (svar v1)) t2s)
     #:when (not (member (svar v1) (ftv t2s)))
     (list (list v1 t2s))]

    ;; same as above, but sequence variable is on the other side
    [(t1s (list (svar v2)))
     #:when (not (member (svar v2) (ftv t1s)))
     (list (list v2 t1s))]

    ;; two individual types aren't unifiable, so unify them
    [((list t1s ... t1) (list t2s ... t2))
     #:when (and (not (svar? t1)) (not (svar? t2)))
     (let* ([phi (unify-ind t1 t2)]
            [phi2 (unify (subst phi t1s) (subst phi t2s))])
       (compose-subst phi2 phi))]

    [(l r) (displayln "inference failed: sequence unification error")
           (displayln (string-append "left seq: " (pretty-type l)))
           (displayln (string-append "right seq: " (pretty-type r)))
           (error "will not run due to inference failure")]))

;; unify-ind : Type, Type -> Subst | Error
(define (unify-ind t1 t2)
  (if (equal? t1 t2)
      (list)
      (match* (t1 t2)
        [((ivar v1) t2p)
         #:when (not (member (ivar v1) (ftv t2p)))
         (list (list v1 (list t2p)))]

        [(t1p (ivar v2))
         #:when (not (member (ivar v2) (ftv t1p)))
         (list (list v2 (list t1p)))]

        [((fun-type i1 o1) (fun-type i2 o2))
         (let* ([phi (unify i1 i2)]
                [phi2 (unify (subst phi o1) (subst phi o2))])
           (compose-subst phi2 phi))]

        [(l r) (displayln "inference failed: individual unification error")
               (displayln (string-append "left: " (pretty-type l)))
               (displayln l)
               (displayln (string-append "right: " (pretty-type r)))
               (displayln r)
               (error "type checking failure")])))

(module+ test
  (check-equal? (unify-ind (fun-type (list) (list)) (fun-type (list) (list)))
                (list)))