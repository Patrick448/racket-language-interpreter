#lang racket

(require dcc019/util/env3
         dcc019/util/memory
         dcc019/exercise/iclasses/ast)

(provide value-of-program)

(define class-data init-env)

(define class-data-hash (make-hash))

(struct object-ref (class field-refs) #:transparent)

(struct ref (addr) #:transparent)

; value-of :: Exp -> ExpVal
(define (value-of exp Δ)
  (match exp
    [(ast:int v) v]
    [(ast:bool v) v]
    [(ast:dif e1 e2) (- (value-of e1 Δ) (value-of e2 Δ))]
    [(ast:zero? e)  (zero? (value-of e Δ))]
    [(ast:not e) (not (value-of e Δ))]
    [(ast:if e1 e2 e3) (if (value-of e1 Δ) (value-of e2 Δ) (value-of e3 Δ))]
    [(ast:var v) (match (apply-env Δ v)
            [(ref v) (deref v)]
            [n n]
      )
    ] 
    [(ast:let (ast:var x) e1 e2) (value-of e2 (extend-env x (value-of e1 Δ) Δ))]
    [(ast:send e (ast:var mth) args) (
       let*-values 
          ( 
            ((obj-ref-ref) (value-of e Δ))   
            ((instance-env) (create-instance-env3 obj-ref-ref ))
            ((class-name) (get-class-name instance-env))
            ((method classfound) (get-method-rec class-name mth))
            ((instance-env) (create-instance-env-cast obj-ref-ref classfound))
            
          )
        [exec-method2 method instance-env (resolve-exps args Δ)]
      )
    ]
    [(ast:super (ast:var c) args) ( begin 
      (let*-values (
       
        ;pegar nome da superclasse
        [(super-class-name)(apply-env Δ '~super)]
         ;construir instancia a partir do ponteiro contido no ~self com cast para a superclasse
        [(instance-env) (create-instance-env-cast (ref-addr (apply-env Δ '~self)) super-class-name)]
        ;buscar método na superclasse e ver onde foi encontrado
        [(method classfound) (get-method-rec (apply-env instance-env '~type) c)]
        ;constuir instancia com cast para a classe onde foi encontrado
        [(instance-env) (create-instance-env-cast (ref-addr (apply-env Δ '~self)) classfound)]
      
      )
      [exec-method2 method instance-env (resolve-exps args Δ)])
    )]
    [(ast:self) (ref-addr (apply-env Δ '~self))]
    [(ast:new (ast:var c) args) (create-class-instance2 c (resolve-exps args Δ))]
    [e (raise-user-error "unimplemented-construction: " e)]
    ))

; result-of :: Stmt -> Env -> State -> State
(define (result-of stmt Δ)
  (match stmt
    [(ast:assign (ast:var x) e) (begin  (setref! (ref-addr (apply-env Δ x)) (value-of e Δ)) Δ)]
    [(ast:print e)  (printf "PRRRIIIINTING! ~a\n" (value-of e Δ)) Δ]
    [(ast:return e) (value-of e Δ)]
    [(ast:block stmts)(begin 
     ( for-each (lambda (stmt) (result-of stmt Δ)) stmts)
      Δ
    )]
    [(ast:if-stmt e s1 s2) (if (value-of e Δ) (result-of s1 Δ) (result-of s2 Δ))]
    [(ast:while e s)  (while-loop e s Δ)]
    [(ast:local-decl (ast:var x) s) (result-of s (extend-env x (ref (newref 'null)) Δ))]
    [(ast:send e (ast:var mth) args) (
        begin 
        (let*-values 
          ( 
            ((obj-ref-ref) (value-of e Δ))   
            ((instance-env) (create-instance-env3 obj-ref-ref))
            ((class-name) (get-class-name instance-env))
            ((method classfound) (get-method-rec class-name mth))
            ((instance-env) (create-instance-env-cast obj-ref-ref classfound))
          )
        [exec-method2 method instance-env (resolve-exps args Δ)])
     
      )
    ]
    [(ast:super (ast:var mth) args) ( begin 
      (let*-values (
       
        ;pegar nome da superclasse
        [(super-class-name)(apply-env Δ '~super)]
         ;construir instancia a partir do ponteiro contido no ~self com cast para a superclasse
        [(instance-env) (create-instance-env-cast (ref-addr (apply-env Δ '~self)) super-class-name)]
        ;buscar método na superclasse e ver onde foi encontrado
        [(method classfound) (get-method-rec (apply-env instance-env '~type) mth)]
        ;constuir instancia com cast para a classe onde foi encontrado
        [(instance-env) (create-instance-env-cast (ref-addr (apply-env Δ '~self)) classfound)]
      
      )
      [exec-method2 method instance-env (resolve-exps args Δ)])
    )]
    [e (raise-user-error "unimplemented-construction: " e)]
    ))


(define (resolve-exps exps Δ)
    (map (lambda (exp) (value-of exp Δ)) exps)
)

(define (get-method-rec class-name method-name)
  ( 
    let* (

      [class-data (hash-ref class-data-hash class-name)]
      [super-class-name (hash-ref class-data 'parent)]
      [method (get-method class-data method-name)]
      ;[xxx( displayln method)]
    )
    (if method
      (values method class-name)
      (get-method-rec super-class-name method-name)
    )
  )
)



(define (get-method class-name method-name)
  (hash-ref (hash-ref class-name 'methods) method-name #f)
)


(define (assoc-param-args-ref params args Δ)
  ;zip params args in a list of (param arg) pairs
    [foldl (lambda (assoc env)
           (extend-env (first assoc) (ref (newref (second assoc))) env))
         Δ
         (map (lambda (param arg) (list param arg)) params args)]
  
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
          (resolved-args (resolve-exps args Δ))
          (new-env (assoc-param-args params-names resolved-args Δ))
        )
        (begin ;(displayln new-env)
          (result-of body new-env)
        )
    )
  ]
  )
)

(define (exec-method2 method Δ args)(
  match method
  [(ast:method name params body)
    (
      let* 
        ( 
          (params-names (get-param-names params))
          
          (params-args args)
          
          (new-env (assoc-param-args-ref params-names params-args Δ))
          
        )
         (begin ;(displayln new-env)
          (result-of body new-env)
        )
    )
  ]
 
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

    (exec-method init-method instance args)
    ;(displayln instance)
    instance
  )
  
))

(define (create-field-refs fields)
  (map (lambda (field) (ref (newref 'null))) fields)
)


(define (create-instance-env instance)
  (let*(
    [class-name (match instance [(object-ref class field-refs) class])]
    [fields (hash-ref (hash-ref class-data-hash class-name) 'fields)]
    [field-names (get-field-names fields)]
    [refs (match instance [(object-ref class field-refs) field-refs])]
    [superclass (get-super-class-name class-name)]
    [base-env (extend-env '~super superclass init-env)]
   
  )
  [begin
    
    (
      foldl 
        (lambda (fieldasoc env) (extend-env (first fieldasoc) (second fieldasoc) env))
        base-env
        (map (lambda (field ref) (list field ref)) field-names refs))]
))

(define (create-instance-env2 classname classref)
  (let*(
    [fields (hash-ref (hash-ref class-data-hash classname) 'fields)]
    [field-names (get-field-names fields)]
    [superclass (get-super-class-name classname)]
    [base-env (extend-env '~super superclass init-env)]
    [base-env (extend-env '~type classname base-env)]
    [first-field-ref (deref classref)]
    [n (length field-names)]
    [last-field-ref (+ first-field-ref n)]
    [refs (range first-field-ref last-field-ref)]
    
  )
  [begin
    
    (
      foldl 
        (lambda (fieldasoc env) (extend-env (first fieldasoc) (ref (second fieldasoc)) env))
        base-env
        (map (lambda (field ref) (list field ref)) field-names refs))]
))

(define (get-field-names fields)
  (map (lambda (field) (match field [(ast:var name) name])) fields)
)


(define (create-instance-env3 obj-ref-ref)
  (let*(

    [obj-ref (deref obj-ref-ref)]
    [classname (object-ref-class obj-ref)]
    [fields (hash-ref (hash-ref class-data-hash classname) 'fields)]
    [field-names (get-field-names (get-all-fields classname))]
    [superclass (get-super-class-name classname)]
    [base-env (extend-env '~super superclass init-env)]
    [base-env (extend-env '~type classname base-env)]
    [refs (object-ref-field-refs obj-ref)]
    [instance-env (
      foldl 
        (lambda (fieldasoc env) (extend-env (first fieldasoc) (ref (second fieldasoc)) env))
        base-env
        (map (lambda (field ref) (list field ref)) field-names refs))
    ]
    [instance-env (extend-env '~self (ref obj-ref-ref) instance-env)]
  )
  (begin
   ; (displayln instance-env)
    
    instance-env)
))


(define (create-instance-env-cast obj-ref-ref type)
  (let*(
    [obj-ref (deref obj-ref-ref)]
    [classname type]
    [fields (hash-ref (hash-ref class-data-hash classname) 'fields)]
    [field-names (get-field-names (get-all-fields classname))]
    [superclass (get-super-class-name classname)]
    [base-env (extend-env '~super superclass init-env)]
    [base-env (extend-env '~type classname base-env)]
    [refs (object-ref-field-refs obj-ref)]
    
    [instance-env (
      foldl 
        (lambda (fieldasoc env) (extend-env (first fieldasoc) (ref (second fieldasoc)) env))
        base-env
        (map (lambda (field ref) (list field ref)) field-names (take refs (length field-names))))
    ]
    [instance-env (extend-env '~self (ref obj-ref-ref) instance-env)]
  )
  (begin
   ; (displayln instance-env)
    
    instance-env)
))


(define (get-all-fields classname)
  (
    let* (
      [fields (hash-ref (hash-ref class-data-hash classname) 'fields)]
      [superclass (get-super-class-name classname)]
      [super-fields (if (and superclass (not (string=? "object" superclass))) (get-all-fields superclass) '())]
    )
    (reverse (append fields super-fields))
  )
)


(define (create-class-instance2 classname args)
(
 let* (

    [init-method (hash-ref (hash-ref (hash-ref class-data-hash classname) 'methods) "initialize" #f)]
    [fields (get-field-names (get-all-fields classname))]
    [field-refs (create-field-refs fields)]
    [obj-ref (newref (object-ref classname (map newref field-refs)))]
    [instance-env (create-instance-env3 obj-ref)]

  )
  (begin 

   (when init-method (exec-method2 init-method instance-env args))
    
    obj-ref
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
 
  )
