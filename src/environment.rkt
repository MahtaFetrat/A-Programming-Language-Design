#lang eopl

(provide apply-scope ;environment is internal to the scope
         extend-scope
         new-local-scope
         new-global-scope)

;environment datatype ------------------------------------------------------------------------
;environment is a list of pairs of variable names and their bound value
(define environment? (list-of pair?))

(define empty-env (lambda () (list)))

(define apply-env
  (lambda (env var)
    (let ((found (assoc var env)))
      (if found
          (cdr found)
          (eopl:error 'apply-env "no bound value for variable ~s" var)))))

(define extend-env
  (lambda (env var val)
    (cons (cons var val) env))) ;because 'assoc' will always return the first pair found



(define globe (empty-env))

;scope datatype ------------------------------------------------------------------------------
;we are either in the global scope or somewhere in the scope of a function
(define-datatype scope scope?
  (global-scope)
  (local-scope
    (global-var-list (list-of symbol?)) ;list of variables defined as 'global'
    (env environment?))) ;local env of the function

(define new-global-scope (lambda () (global-scope)))
(define new-local-scope (lambda () (local-scope (list) (empty-env))))

;gets the proper local or global value for a variable
(define apply-scope
  (lambda (sc var)
    (cases scope sc
      (global-scope () (apply-env globe var))
      (local-scope (global-var-list env)
        (cond
          ((member var global-var-list) (apply-env globe var))
          ((assoc var env) (apply-env var env))
          (else (eopl:error 'apply-scope "no bound value for variable ~s" var)))))))

;extends either the local env or the globe env based on wether it is 'global', in the global scope or not
(define extend-scope
  (lambda (sc var val)
    (cases scope sc
      (global-scope () (set! globe (extend-env globe var val)))
      (local-scope (global-var-list env)
        ((cond
          ((member var global-var-list) (set! globe (extend-env globe var val)))
          (else (extend-env env var val))))))))
    
  



