#lang eopl

(require "environment.rkt"
         "grammar.rkt")

"!!!try to keep as few helper functions and datatypes as you can in this file. this module is only for 'value-of-functions'"

(define value-of-program
  (lambda (pgm) ;program
    (cases program pgm
      (a-program (statements) (value-of-statements (new-global-scope))))))

(define value-of-statements
  (lambda (sts scope)
    (cases statements sts
      (mult-statements (sts st)
                       (let ((ans (value-of-statements sts scope)))
                         (if (nor (return-message? ans) (break-message? ans) (continue-message? ans))
                             (value-of-statement st (answer-scope ans))
                             ans)))
      (single-statement (st)
                        (value-of-statement st scope)))))

(define value-of-statement
  (lambda (st scope)
    (cases statement st
      (a-simple-statement
       (value-of-simple-stmt (st scope)))
      (a-compund-statement
       (value-of-compound-stmt (st scope))))))

(define value-of-simple-stmt
  (lambda (st scope)
    (cases simple-stmt st
      (assignment-simple-stmt (assignment) (value-of-assignment assignment scope))
      (return-simple-stmt (return-stmt) (value-of-return-stmt return-stmt scope))
      (global-simple-stmt (global-stmt)(value-of-global-stmt global-stmt scope))
      (pass-stmt () (answer '() '() scope))
      (break-stmt () (answer '() 'break scope))
      (continue-stmt () (answer '() 'continue scope)))))

(define value-of-assignment
  (lambda (as scope)
    (cases assignment as
      (an-assignment (ID exp)
                     (let ((exp-val (answer-val (value-of-expression exp scope))))
                       (answer exp-val '() (extend-scope sc ID exp-val)))))))

(deifne value-of-return-stmt
        (lambda (return-st scope)
          (cases return-stmt return-st
            (empty-return-stmt () (answer '() 'return scope))
            (exp-return-stmt (exp)
                             (let ((exp-val (answer-val (value-of-expression exp scope))))
                               (answer exp-val 'return scope))))))

(define value-of-global-stmt
  (lambda (global-st scope)
    (cases global-stmt global-st
      (a-global-stmt (ID)
                     (answer '() '() (add-to-global-var-list scope ID))))))
                                 
(define value-of-compound-stmt
  (lambda (st scope)
    (cases compound-stmt st
      (func-def-comp-stmt (func-def) (value-of-function-def func-def scope))
      (if-comp-stmt (if-comp-stmt) (value-of-if-stmt if-stmt scope))
      (for-comp-stmt (for-stmt) (value-of-for-stmt for-stmt scope)))))

(define value-of-function-def
  (lambda (func-def scope)
    (cases function-def func-def
      (params-function-def (ID params sts) (answer (function ID params sts (new-local-scope scope)) '() scope))
      (zero-param-func-def (ID sts) (answer (function ID '() sts (new-local-scope scope)) '() scope)))))

(define value-of-if-stmt
  (lambda (if-st scope)
    (cases if-stmt if-st
      (an-if-stmt (exp sts else-block)
                  (let ((ans (value-of-expression exp scope)))
                    (if (answer-val ans)
                        (value-of-statements sts scope)
                        (value-of-else-block else-block scope)))))))

(define value-of-else-block
 (lambda (else-bl scope)
   (cases else-block else-bl
     (an-else-block (stss)
                    (value-of-statements sts scope)))))

(define value-of-for-stmt
  (lambda (for-st scope)
    (cases for-stmt sor-st
      (a-for-stmt (ID exp sts)
                  (let ((iterable (answer-val (value-of-exp exp))))
                    (value-of-for-bodies ID iterable sts scope))))))

(define value-of-for-bodies
  (lambda (ID iterable sts scope)
    (if (null? iterable)
        (answer '() '() scope)
        (let ((ans (value-of-statements sts (extend-scope scope ID (car iterable)))))
          (if (break-message? ans)
              (answer '() '() (answer-scope ans))
              (value-of-for-bodies ID (cdr iterable) sts (answer-scope ans)))))))
                  
(define value-of-expression
  (lambda (exp scope)
    (cases expression exp
      (an-expression (disj)
                     (value-of-disjunction disj scope)))))
"
       a function call
       the apply-function procedure must do the following:
           1- extend the local scope with the function itself for recursive call (the function value itself, not globally, as python does, 'cause changes to function name don't effect the globe unless defined as 'global' later)
           2-extend the local scope with the default values, make sure to do this step after 1, 'cause arguments can overrite function name with another value, just as in python
           3-the function body, containing 'Statements' is evaluated with the local scope
           4-finally note that function scopes, since they are local, dont influence the outer body after return. the influence on the globe will be properly applied btw. in other words, dont pass the scope in the answer of this function call to other sibling statements (unlike as we did in value-of-satements)))))
"

(define value-of-disjunction
  (lambda (disj scope)
    (cases disjunction disj
      (single-disjunction (conj)
                          (value-of-conjunction))
      (mult-disjunction (disj conj)
                        (let ((exp-val1 (answer-val (value-of-disjunction disj scope)))
                              (exp-val2 (answer-val (value-of-conjunction conj scope))))
                          (answer (or exp-val1 exp-val2) '() scope))))))

(define value-of-conjunction
  (lambda (conj scope)
    (cases conjunction conj
      (single-conjunction (inv)
                          (value-of-inversion inv scope))
      (mult-conjunction (conj inv)
                        (let ((exp-val1 (answer-val (value-of-conjunction conj scope)))
                              (exp-val2 (answer-val (value-of-inversion inv scope))))
                          (answer (and exp-val1 exp-val2) '() scope))))))

(define value-of-inversion
  (lambda (inv scope)
    (cases inversion inv
      (not-inversion (inv)
                     (let ((exp-val1 (answer-val (value-of-inversion inv scope)))
                           (answer (not exp-val1) '() scope))))
      (a-comparison (comp)
                    (value-of-comparison comp scope)))))

(define value-of-comparison
  (lambda (comp scope)
    (cases comparison comp
      (mult-comparison (sum cmp-op-sum-p)
                       (let ((val1 (value-of-sum sum scope))
                             (val2 (value-of-compare-op-sum-pairs cmp-op-sum-p scope)))
                         ...))
      (single-comparison (sum)
                         (let ((val (value-of-sum sum scope)))
                           ...)))))

(define value-of-compare-op-sum-pairs
  (lambda (cmp-op-sum-ps scope)
    (cases compare-op-sum-pairs cmp-op-sum-ps
      (single-compare-op-sum-pairs (cmp-op-sum-p)
                                   (let ((val (value-of-compare-op-sum-pair cmp-op-sum-p scope)))
                                     ...))
      (mult-compare-op-sum-pairs (cmp-op-sum-ps cmp-op-sum-p)
                                 (let ((val1 (value-of-compare-op-sum-pairs cmp-op-sum-ps scope))
                                       (val2 (value-of-compare-op-sum-pair cmp-op-sum-p scope)))
                                   ...)))))

(define value-of-compare-op-sum-pair
  (lambda (cmp-op-sum-p scope)
    (cases compare-op-sum-pair cmp-op-sum-p
      (eq-sum-pair (eq-sum)
                   ...)
      (lt-sum-pair (lt-sum)
                   ...)
      (gt-sum-pair (gt-sum)
                   ...))))

(define value-of-eq-sum-pair
  (lambda (pair scope)
    (cases eq-sum-pair pair
      (an-eq-sum (sum)
                 ...))))

(define value-of-lt-sum-pair
  (lambda (pair scope)
    (cases lt-sum-pair pair
      (an-lt-sum (sum)
                 ...))))

(define value-of-gt-sum-pair
  (lambda (pair scope)
    (cases gt-sum-pair pair
      (a-gt-sum (sum)
                 ...))))

(define value-of-sum
  (lambda (sum scope)
    (cases sum sum
      (add-sum (sum term)
               ...)
      (sub-sum (sum term)
               ...)
      (single-sum (term)
                  ...))))

(define value-of-term
  (lambda (term scope)
    (cases term term
      (mul-term (term factor)
                ...)
      (div-term (term factor)
                ...)
      (single-term (factor)
                   ...))))

(define value-of-factor
  (lambda (factor scope)
    (cases factor factor
      (pos-factor (factor)
                  ...)
      (neg-factor (factor)
                  ...)
      (single-factor (pow)
                     ...))))

(define value-of-power
  (lambda (power scope)
    (cases power power
      (a-pow (atom factor)
             ...)
      (a-primary (primary)
                 ...))))

(define value-of-primary
  (lambda (primary scope)
    (cases primary primary
      (an-atom (atom)
               ...)
      (index-cases (primary exp)
                   ...)
      (zero-arg-func-call (primary)
                          ...)
      (args-func-call (primary args)
                      ...))))
