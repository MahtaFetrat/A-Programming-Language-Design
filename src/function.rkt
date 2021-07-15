#lang eopl

(require "environemnt.rkt"
         "grammar.rkt"
         "interpreter.rkt")

(define-datatype function
  (a-function
   (ID symbol?)
   (params params?)
   (statements statements?)
   (scope scope?)))

(define apply-function
  (lambda (func arg-list outer-scope)
    (cases function func
      (a-function (ID params statements scope)
                  (let ((scope (extend-scope scope ID (apply-scope ID outer-scope))))
                    (let ((scope (foldl (lambda (x y) (answer-scope (value-of-param-with-default y x)))) scope params))
                      (let ((thunk-scope (get-thunk-environment outer-scope)))
                        (let ((scope (letrec ((add-args-to-scope (arg-list params scope)
                                                                 (if (null? arg-list)
                                                                     scope
                                                                     (cases param-with-default (car params)
                                                                       (a-param-with-default (ID exp)
                                                                                             (add-args-to-scope
                                                                                              (cdr arg-list)
                                                                                              (cdr params)
                                                                                              (extend-scope scope ID (a-thunk (car arg-list) thunk-scope))))))))
                                       (add-args-to-scope arg-list params scope))))
                          (value-of-statements statements scope)))))))))
                    
            
