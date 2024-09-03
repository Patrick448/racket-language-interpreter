#lang racket

(require dcc019/util/env
         dcc019/util/memory
         dcc019/classes/ast)

(provide value-of-program)

; Representação de procedimentos para escopo estático

; proc-val :: Var x Expr x Env -> Proc
(define (proc-val var exp Δ) ; call by value
  (lambda (val)
    (value-of exp (extend-env var (newref val) Δ))))

; apply-proc :: Proc x ExpVal -> ExpVal  
(define (apply-proc proc val)
  (proc val))

; Criação de ambiente estendido com procedimento recursivo
(define (extend-env-rec name var body env)
  (lambda (svar)
    (if (equal? svar name)
        (newref (proc-val var body (extend-env-rec name var body env)))
        (apply-env env svar))))

; value-of :: Exp -> ExpVal
(define (value-of exp Δ)
;(print-env Δ)
  (match exp
    [(ast:int n) n]
    [(ast:dif e1 e2) (begin (- (value-of e1 Δ) (value-of e2 Δ)))]
    [(ast:zero? e) (zero? (value-of e Δ))]
    [(ast:if e1 e2 e3) (if (value-of e1 Δ) (value-of e2 Δ) (value-of e3 Δ))]
    [(ast:var v) (deref (apply-env Δ v))]
    [(ast:let (ast:var x) e1 e2) (begin  (value-of e2 (extend-env x (value-of e1 Δ) Δ)))]
    [(ast:proc (ast:var v) e) (proc-val v e Δ)]
    [(ast:call e1 e2) (apply-proc (value-of e1 Δ) (value-of e2 Δ))] ; call by value
    [(ast:letrec (ast:var f) (ast:var v) e1 e2) (value-of e2 (extend-env-rec f v e1 Δ))]
    [(ast:begin es) (foldl (lambda (e v) (value-of e Δ)) (value-of (first es) Δ) (rest es))]
    [(ast:assign (ast:var x) e) (begin
                                  (setref! (apply-env Δ x) (value-of e Δ)) ;set the value in the store
                                  42)] ; return the 42 value
    [(ast:new (ast:var n) args) (extend-env n (value-of-fields (apply-env (apply-env class-env n) 'fields) empty-env) empty-env)]
    [(ast:send (ast:var n) (ast:var method) args) (begin (exec-method (get-method (car (car (apply-env Δ n))) method) args (apply-env Δ n)) 
    ;(display class-env)
      ;(display (rest (car (apply-env Δ n)) ))
    )]
    [e (raise-user-error "unimplemented-construction: " e)]
    )
    )

(define (get-method classname methodname) (
  apply-env (apply-env (apply-env class-env classname) 'methods) methodname
)
)

(define (associate-arg params args env)

(match params
  [(list (ast:var n)) (extend-env n (value-of (car args) env) env)]
  [(list p1 pr) (associate-arg p1 args (associate-arg pr (rest args) env))]
)
)


(define (exec-method method args env)
  
  (match method
  [(list params body)(value-of body (associate-arg params args env))]

  )
)

(define (create-object-env class Δ)
  (match class
     [(ast:decl (ast:var name) super fields methods)
      (begin
        (extend-env name (value-of-fields fields empty-env) Δ)
      )
    ]
  )
)


(define class-env init-env)

(define (value-of-program prog)
;(display prog)

  (empty-store)
  (match prog
    [(ast:prog decls exp)
     (begin
      
       ; you must collect all the classes declared and building its respectively environment
       ; execute the prog expression in the correct environment
       (set! class-env (value-of-class decls class-env))
       (value-of exp init-env))])  
    )

(define (value-of-class decl Δ)
  (match decl
    [(ast:decl name super fields methods)
      (begin
        (extend-env (class-name name) 
            (extend-env 'methods (create-methods-env methods Δ) 
              (extend-env 'fields fields Δ) ) Δ)
      )
    ]
    [(list c1 cr)
      (begin 
        (value-of-class c1 (value-of-class cr Δ))
      )
    ]
  )
)



(define (create-fields-env fields Δ)(extend-env 'fields fields empty-env))

(define (create-methods-env methods Δ)
  (match methods 
    [(ast:method (ast:var name) params body) (extend-env name (list params body) Δ)]
    [(list m1 mr)
      (begin 
        (create-methods-env mr (create-methods-env m1 Δ))
      )]
  )
)


(define (class-name name)
(match name
  [
    (ast:var n) n
  ]
))

(define (value-of-fields fields Δ)
  ;(display fields)
  (match fields
    [(ast:var name) (extend-env name 'null Δ)]
    [(list f1 fr)
      (begin 
        (value-of-fields fr (value-of-fields f1 Δ))
      )]
 )
)

