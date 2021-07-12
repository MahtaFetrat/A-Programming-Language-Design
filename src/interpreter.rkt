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
    (cases expression exp
      (an-expression (exp)
                     (let ((val (value-of-disjunction exp scope)))
                       ...))
      (;a function call
       the apply-function procedure must do the following:
           1- extend the local scope with the function itself for recursive call (the function value itself, not globally, as python does, 'cause changes to function name don't effect the globe unless defined as 'global' later)
           2-extend the local scope with the default values, make sure to do this step after 1, 'cause arguments can overrite function name with another value, just as in python
           3-the function body, containing 'Statements' is evaluated with the local scope
           4-finally note that function scopes, since they are local, dont influence the outer body after return. the influence on the globe will be properly applied btw. in other words, dont pass the scope in the answer of this function call to other sibling statements (unlike as we did in value-of-satements)))))

(define value-of-disjunction
  (lambda (disj scope)
    (cases disjunction disj
      (single-disjunction (disj)
                          (let ((val (value-of-conjunction exp scope)))
                            ...))
      (mult-disjunction (disj conj)
                        (let ((val1 (value-of-disjunction disj scope))
                              (val2 (value-of-conjunction conj scope)))
                          ...)))))

(define value-of-conjunction
  (lambda (conj scope)
    (cases conjunction conj
      (single-conjunction (conj)
                          ...)
      (mult-conjunction (conj inv)
                        (let ((val1 (value-of-conjunction conj scope))
                              (val2 (value-of-inversion inv scope))))
                        ...))))

(define value-of-inversion
  (lambda (inv scope)
    (cases inversion inv
      (not-inversion (inv)
                     ...)
      (a-comparison (comp)
                    (let ((val (value-of-comparison comp scope)))
                      ...)))))

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
