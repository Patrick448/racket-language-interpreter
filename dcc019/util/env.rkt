#lang racket

;(provide (all-defined-out))

; Definição do Environment
;(define empty-env
 ; (lambda (var)
  ;  (error "No bind")))

;(define (extend-env var value env)
 ; (lambda (svar)
  ;  (if (equal? svar var) value
   ;     (apply-env env svar))))

;(define (apply-env env var)
 ; (env var))

;(define init-env empty-env)


(provide (all-defined-out))

; Define the empty environment as an empty list
(define empty-env '())

; Extend the environment by adding a (var, value) pair to the front of the environment list
(define (extend-env var value env)
  (cons (cons var value) env))

; Apply the environment by searching for the variable in the list
(define (apply-env env var)
  (cond
    [(null? env) (error "No bind")]
    [(equal? (caar env) var) (cdar env)]
    [else (apply-env (cdr env) var)]))

; Function to print all bindings in the environment
(define (print-env env)
  (for-each (lambda (binding)
              (printf "~a: ~a\n" (car binding) (cdr binding)))
            env))

(define init-env empty-env)


