;Autores: Patrick Canto de Carvalho - 201935026
;Maria Eduarda de Medeiros Simonassi - 202365119A
#lang racket

(provide (all-defined-out))

;; Environment represented as a list of bindings
(define empty-env
  (list))

(define init-env empty-env)

;; Extends the environment by adding a new variable binding
(define (extend-env var value env)
  (cons (list var value) env))

;; Apply environment to lookup a variable
(define (apply-env env var)
  (let ((binding (assoc var env)))
    (if binding
        (second binding)
        (error "Variable not found in environment" var))))

;; Helper function to print the environment
(define (print-env env)
  ((for-each (lambda (binding)
              (displayln (format "~a = ~a" (first binding) (second binding))))
            env)))


