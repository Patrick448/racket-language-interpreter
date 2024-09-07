#lang racket

(provide (all-defined-out))

;; Environment represented as a list of bindings
(define empty-env (list))
(define init-env empty-env)

;; Extends the environment by adding a new variable binding
(define (extend-env var value env)
  (cons (list var value) env))

;; Applies environment to lookup a variable
(define (apply-env env var)
  (let ((binding (assoc var env)))
    (if binding
        (second binding)
        (error "Variable not found in environment" var))))

;; Helper function to print the environment
(define (print-env env)
  (for-each (lambda (binding)
              (displayln (format "~a = ~a" (first binding) (second binding))))
            env))

;; Stack of environments
(define env-stack (list empty-env))

;; Push a new environment onto the stack
(define (push-env)
  (set! env-stack (cons empty-env env-stack)))

;; Pop the top environment off the stack
(define (pop-env)
  (if (null? (cdr env-stack))
      (error "Cannot pop the global environment.")
      (set! env-stack (cdr env-stack))))

;; Extend the top environment on the stack
(define (extend-top-env var value)
  (let ((top-env (car env-stack)))
    (set! env-stack (cons (extend-env var value top-env) (cdr env-stack)))))

;; Lookup variable in the top environment
(define (lookup var)
  (apply-env (car env-stack) var))


;;Updates environment with new value
(define (update-env var value)
  (let ((binding (assoc var (car env-stack))))
    (if binding
        (set-cdr! binding value)
        (error "Variable not found in environment" var))))