#lang racket

(require "environment.rkt")

"!!!try to keep as few helper functions and datatypes as you can in this file. this module is only for 'value-of-functions'"

(define value-of-program
  (lambda (pgm) ;program
    (cases program pgm
      (a-program (statements) (value-of-statements (new-global-scope))))))

(define value-of-statements
  (lambda (sts scope)
    (cases statements sts
      (single-statement (st)
                        (value-of-statement st scope))
      (mult-statements (sts st)
                       (let ((ans (value-of-statements sts scope)))
                         (if (not (return-message? ans) (break-message? ans) (continue-message? ans))
                             (value-of-statement st (answer-scope ans))
                             ans))))))

(define value-of-statement
  (lambda (st scope)
    (cases statement st
      (;simple-statement
       (value-of-simple-statement (st scope)))
      (;compund-statement
       (value-of-compound-statement (st scope))))))

(define value-of-simple-statement
  (lambda (st scope)
    (cases simple-statement ...
     ... returns an answer with the message of the simple statement and potential value of return or None)))

(define value-of-compound-statement
  (lambda (st scope)
    (cases compound-statement ...
      (if-st (statements ...)
             (use value-of-statements to get an answer))
      (for-st (statements ...)
              (use value-of-statements to get an answer, also use messages like break, cont, ... to control the loop))
      (def-func (...)
        (create a func containing its body and its argument and an empty new-local-scope, add this function to scope, ... )
      ... all the cases at the end return an answer with value, scope , ...))))

(define value-of-expression
  (lambda (exp scope)
    (cases expression
      .
      .
      .
      (;a function call
       the apply-function procedure must do the following:
           1- extend the local scope with the function itself for recursive call (the function value itself, not globally, as python does, 'cause changes to function name don't effect the globe unless defined as 'global' later)
           2-extend the local scope with the default values, make sure to do this step after 1, 'cause arguments can overrite function name with another value, just as in python
           3-the function body, containing 'Statements' is evaluated with the local scope
           4-finally note that function scopes, since they are local, dont influence the outer body after return. the influence on the globe will be properly applied btw. in other words, dont pass the scope in the answer of this function call to other sibling statements (unlike as we did in value-of-satements)))))




       
                         

