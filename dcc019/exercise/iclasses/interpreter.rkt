#lang racket

(require dcc019/util/env3
         dcc019/util/memory
         dcc019/exercise/iclasses/ast)

(provide value-of-program)

(define class-data init-env)

(define class-data-hash (make-hash))

;(hash-set! class-data-hash 'key1 "value1")
;(hash-set! class-data-hash 'key2 42)
;(hash-set! class-data-hash 'key3 (make-hash (list (cons 'key4 "value4") (cons 'key5 42))))


; value-of :: Exp -> ExpVal
(define (value-of exp Δ)
  (match exp
    [(ast:int v) v]
    [(ast:bool v) v]
    [(ast:dif e1 e2) (- (value-of e1 Δ) (value-of e2 Δ))]
    [(ast:zero? e)  (zero? (value-of e Δ))]
    [(ast:not e) (not (value-of e Δ))]
    [(ast:if e1 e2 e3) (if (value-of e1 Δ) (value-of e2 Δ) (value-of e3 Δ))]
    [(ast:var v) (apply-env Δ v)] ; esta implementação só funciona para variáveis imutáveis
    [(ast:let (ast:var x) e1 e2) (value-of e2 (extend-env x (value-of e1 Δ) Δ))]
    [(ast:send e (ast:var mth) args) (display "send expression unimplemented")]
    [(ast:super (ast:var c) args) (display "super expression unimplemented")]
    [(ast:self) (display "self expression unimplemented")]
    [(ast:new (ast:var c) args) (create-class-instance c args)]
    [e (raise-user-error "unimplemented-construction: " e)]
    ))

; result-of :: Stmt -> Env -> State -> State
(define (result-of stmt Δ)
  (match stmt
    [(ast:assign (ast:var x) e) (extend-env x (value-of e Δ) Δ)]
    [(ast:print e) (printf "PRRRIIIINTING! ~a\n" (value-of e Δ))
                   #;(display "print unimplemented") Δ]
    [(ast:return e) (value-of e Δ)]
    [(ast:block '()) Δ]
    [(ast:block stmts)(result-of (ast:block (rest stmts)) (result-of (first stmts) Δ))
    ;(result-of first-stmt Δ) (result-of rest-stmt Δ)
    ]
    [(ast:if-stmt e s1 s2) (if (value-of e Δ) (result-of s1 Δ) (result-of s2 Δ))]
    [(ast:while e s)  (while-loop e s Δ)]
    [(ast:local-decl (ast:var x) s) (result-of s (extend-env x 'null Δ))]
    [(ast:send e (ast:var mth) args) (
      let* 
        ( (instance-name (match e [(ast:var v) v]))
          (class-instance (value-of e Δ))
          (class-name (get-class-name class-instance))
        )
     (extend-env instance-name [exec-method (get-method class-name mth) class-instance args] Δ)
    )
      ]
    [(ast:super (ast:var mth) args) ( begin 
      (displayln 'aaaaa)
      (displayln Δ)
      (let* (
        [current-class-instance Δ]
        [current-class-name (get-class-name current-class-instance)]
        [super-class-name (hash-ref (hash-ref class-data-hash current-class-name) 'parent)]
        [method (get-method super-class-name mth)]
      )
      [exec-method method Δ args])
    )]
    [e (raise-user-error "unimplemented-construction: " e)]
    ))


(define (resolve-exps exps Δ)
    (map (lambda (exp) (value-of exp Δ)) exps)
)

(define (get-method class-name method-name)
  (hash-ref (hash-ref (hash-ref class-data-hash class-name) 'methods) method-name)
)

(define (assoc-param-args params args Δ)
  ;zip params args in a list of (param arg) pairs
    [foldl (lambda (assoc env)
           (extend-env (first assoc) (second assoc) env))
         Δ
         (map (lambda (param arg) (list param arg)) params args)]
  
)


(define (get-param-names params)
  (map (lambda (param) (match param [(ast:var name) name])) params)
)

(define (exec-method method Δ args)(
  match method
  [(ast:method name params body)
    (
      let* 
        ( 
          (params-names (get-param-names params))
          (params-args (resolve-exps args Δ))
          (new-env (assoc-param-args params-names params-args Δ))
        )
        (
          result-of body new-env
        )
    )
  ]
 ; (display 'bbbb)
 ; (result-of body Δ)
  )
)

(define (get-class-name class-instance)
  (apply-env class-instance '~type)
)

(define (create-classes decls) 
  (for-each (
    lambda (decl)
      (
        match decl
        [(ast:decl (ast:var name) (ast:var parent) fields methods) 

          (hash-set! class-data-hash name (
            make-hash (list 
            (cons 'parent parent) 
            (cons 'fields fields) 
            (cons 'methods (make-methods-hash methods)))
          ))
          ]
      ) 
      (displayln #\space)
      ) decls)
)

(define (make-methods-hash methods) (
  make-hash (map (
    lambda(method)(
      match method
      [(ast:method (ast:var name) params body) (cons name method)]
    )
  ) methods)

))

(define (get-super-class-name class-name)
  (hash-ref (hash-ref class-data-hash class-name) 'parent)
)

(define (create-class-instance classname args)(
 ;display-hash-table (hash-ref class-data-hash classname) 0
 let* (
  [init-method (hash-ref (hash-ref (hash-ref class-data-hash classname) 'methods) "initialize")]
  [fields (hash-ref (hash-ref class-data-hash classname) 'fields)]
  [env-with-fields (extend-env-with-fields fields (extend-env '~type classname init-env))]
  [env-with-super (extend-env '~self 'reference-to-memory env-with-fields)]
  [instance env-with-super]
  )
  (begin 

    (display (exec-method init-method instance args))
    instance
  )
  
))

(define (extend-env-with-fields fields Δ)
  (match fields
    [(list (ast:var name)) (extend-env name 'null Δ)]
    [(list f1 fr)
    (begin 
      (extend-env-with-fields f1 Δ)
      (extend-env-with-fields fr Δ)
    )]
 )
)


;; Recursive function to display key-value pairs (including nested hash tables)
(define (display-hash-table ht indent)
  ;; Display opening brace
  (displayln (string-append (make-string indent #\space) "{"))
  ;; Iterate over each key-value pair
  (for-each
    (lambda (key)
      (let ((value (hash-ref ht key)))
        (if (hash? value)
            ;; If the value is a hash table, recurse with increased indentation
            (begin
              (displayln (format "~a~a: " (make-string (+ indent 2) #\space) key))
              (display-hash-table value (+ indent 4)))
            ;; Otherwise, just print the key and value
            (displayln (format "~a~a: ~a" (make-string (+ indent 2) #\space) key value)))))
    (hash-keys ht))
  ;; Display closing brace
  (displayln (string-append (make-string indent #\space) "}")))



(define (while-loop test stmt Δ)
  ;(display test)
  (if (value-of test Δ) (while-loop test stmt (result-of stmt Δ)) Δ)
)

(define (value-of-program prog)
  (empty-store)
  (match prog
    [(ast:prog decls stmt)
     (begin
       ; you must collect all the classes declared and building its respectively environment
       ; execute the prog expression in the correct environment
       (create-classes decls)
       (result-of stmt init-env)
       (void)
      )
    ]
  )
  (display-hash-table class-data-hash 0)
 
  )

