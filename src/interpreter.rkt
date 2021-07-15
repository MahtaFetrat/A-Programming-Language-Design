#lang eopl

(require "environment.rkt"
         "grammar.rkt"
         "answer.rkt")

(define value-of-program
  (lambda (pgm) ;program
    (cases program pgm
      (a-program (statements) (value-of-statements (new-global-scope))))))

(define value-of-statements
  (lambda (sts scope)
    (cases statements sts
      (mult-statements (sts st)
                       (let ((ans (value-of-statements sts scope)))
                         (if (not (or (return-message? ans) (break-message? ans) (continue-message? ans)))
                             (value-of-statement st (answer-scope ans))
                             ans)))
      (single-statements (st)
                        (value-of-statement st scope)))))

(define value-of-statement
  (lambda (st scope)
    (cases statement st
      (a-compound-stmt (st)
       (value-of-compound-stmt (st scope)))
      (a-simple-stmt (st)
       (value-of-simple-stmt (st scope))))))

(define value-of-simple-stmt
  (lambda (st scope)
    (cases simple-stmt st
      (assignment-simple-stmt (assignment) (value-of-assignment assignment scope))
      (return-simple-stmt (return-stmt) (value-of-return-stmt return-stmt scope))
      (global-simple-stmt (global-stmt)(value-of-global-stmt global-stmt scope))
      (pass-stmt () (an-answer '() '- scope))
      (break-stmt () (an-answer '() 'break scope))
      (continue-stmt () (an-answer '() 'continue scope)))))

(define value-of-assignment
  (lambda (as scope)
    (cases assignment as
      (an-assignment (ID exp)
                     (an-answer exp-val '- (extend-scope scope ID (a-thunk exp (get-thunk-scope scope))))))))

(define value-of-return-stmt
        (lambda (return-st scope)
          (cases return-stmt return-st
            (empty-return-stmt () (an-answer '- 'return scope))
            (exp-return-stmt (exp)
                             (let ((exp-val (answer-val (value-of-expression exp scope))))
                               (an-answer exp-val 'return scope))))))

(define value-of-global-stmt
  (lambda (global-st scope)
    (cases global-stmt global-st
      (a-global-stmt (ID)
                     (an-answer '() '- (add-to-global-var-list scope ID))))))
                                 
(define value-of-compound-stmt
  (lambda (st scope)
    (cases compound-stmt st
      (func-def-comp-stmt (func-def) (value-of-function-def func-def scope))
      (if-comp-stmt (if-stmt) (value-of-if-stmt if-stmt scope))
      (for-comp-stmt (for-stmt) (value-of-for-stmt for-stmt scope)))))

(define value-of-function-def
  (lambda (func-def scope)
    (cases function-def func-def
      (params-func-def (ID params sts)
                           (let ((new-func (function ID params sts (new-local-scope scope))))
                             (an-answer '() '- (extend-scope scope ID new-func))))
      (zero-param-func-def (ID sts)
                           (let ((new-func (function ID '() sts (new-local-scope scope))))
                             (an-answer '() '- (extend-scope scope ID new-func)))))))

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
        (an-answer '() '- scope)
        (let ((ans (value-of-statements sts (extend-scope scope ID (car iterable)))))
          (if (break-message? ans)
              (an-answer '() '- (answer-scope ans))
              (value-of-for-bodies ID (cdr iterable) sts (answer-scope ans)))))))
                  
(define value-of-expression
  (lambda (exp scope)
    (cases expression exp
      (an-expression (disj)
                     (value-of-disjunction disj scope)))))

(define value-of-disjunction
  (lambda (disj scope)
    (cases disjunction disj
      (single-disjunction (conj)
                          (value-of-conjunction))
      (mult-disjunction (disj conj)
                        (let ((exp-val1 (answer-val (value-of-disjunction disj scope)))
                              (exp-val2 (answer-val (value-of-conjunction conj scope))))
                          (an-answer (or exp-val1 exp-val2) '- scope))))))

(define value-of-conjunction
  (lambda (conj scope)
    (cases conjunction conj
      (single-conjunction (inv)
                          (value-of-inversion inv scope))
      (mult-conjunction (conj inv)
                        (let ((exp-val1 (answer-val (value-of-conjunction conj scope)))
                              (exp-val2 (answer-val (value-of-inversion inv scope))))
                          (an-answer (and exp-val1 exp-val2) '- scope))))))

(define value-of-inversion
  (lambda (inv scope)
    (cases inversion inv
      (not-inversion (inv)
                     (let ((exp-val1 (answer-val (value-of-inversion inv scope)))
                           (an-answer (not exp-val1) '- scope))))
      (a-comparison (comp)
                    (value-of-comparison comp scope)))))

(define value-of-comparison
  (lambda (comp scope)
    (cases comparison comp
      (mult-comparison (sum cmp-op-sum-ps)
                       (let ((left-hand-operand (answer-val (value-of-sum sum scope))))
                         (an-answer (cmp-res (value-of-compare-op-sum-pairs left-hand-operand cmp-op-sum-ps scope)) '- scope)))
      (single-comparison (sum)
                         (value-of-sum sum scope)))))

(define value-of-compare-op-sum-pairs
  (lambda (left-hand-operand cmp-op-sum-ps scope)
    (cases compare-op-sum-pairs cmp-op-sum-ps
      (single-compare-op-sum-pairs (cmp-op-sum-p)
                                   (value-of-compare-op-sum-pair left-hand-operand cmp-op-sum-p scope))
      (mult-compare-op-sum-pairs (cmp-op-sum-ps cmp-op-sum-p)
                                 (let ((cmp-ans (value-of-compare-op-sum-pairs left-hand-operand cmp-op-sum-ps scope)))
                                   (if (cmp-res cmp-ans)
                                       (value-of-compare-op-sum-pair (cmp-right-hand-operand cmp-ans) cmp-op-sum-p scope)
                                       cmp-ans))))))

(define value-of-compare-op-sum-pair
  (lambda (left-hand-operand cmp-op-sum-p scope)
    (cases compare-op-sum-pair cmp-op-sum-p
      (eq-sum-pair (eq-sum)
                   (value-of-eq-sum left-hand-operand eq-sum scope))
      (lt-sum-pair (lt-sum)
                   (value-of-lt-sum left-hand-operand lt-sum scope))
      (gt-sum-pair (gt-sum)
                   (value-of-gt-sum left-hand-operand gt-sum scope)))))

(define value-of-eq-sum
  (lambda (left-hand-operand eq-s scope)
    (cases eq-sum eq-s
      (an-eq-sum (sum)
                 (a-cmp-answer(equal? left-hand-operand sum) sum)))))

(define value-of-lt-sum
  (lambda (left-hand-operand lt-s scope)
    (cases lt-sum lt-s
      (an-lt-sum (sum)
                 (a-cmp-answer(< left-hand-operand sum) sum)))))

(define value-of-gt-sum
  (lambda (left-hand-operand gt-s scope)
    (cases gt-sum gt-s
      (a-gt-sum (sum)
                (a-cmp-answer(> left-hand-operand sum) sum)))))

(define value-of-sum
  (lambda (sum scope)
    (cases sum sum
      (add-sum (sum term)
               (let ((exp-val1 (answer-val (value-of-sum sum scope)))
                     (exp-val2 (answer-val (value-of-term term scope))))
                 (cond
                   ((boolean? exp-val1) (an-answer (or exp-val1 exp-val2) '- scope))
                   ((py-list? exp-val1) (an-answer (append exp-val1 exp-val2) '- scope))
                   (else (an-answer (+ exp-val1 exp-val2) '- scope)))))
      (sub-sum (sum term)
               (let ((exp-val1 (answer-val (value-of-sum sum scope)))
                     (exp-val2 (answer-val (value-of-term term scope))))
                 (an-answer (- exp-val1 exp-val2) '- scope)))
      (single-sum (term)
                  (value-of-term term scope)))))

(define value-of-term
  (lambda (term scope)
    (cases term term
      (mul-term (term factor)
               (let ((exp-val1 (answer-val (value-of-sum sum scope))))
                 (if (boolean? exp-val1)
                     (if (false? exp-val1)
                         (an-answer #f '- scope)
                         (let ((exp-val2 (answer-val (value-of-term term scope))))
                           (an-answer (and exp-val1 exp-val2) '- scope)))
                     (if (zero? exp-val1)
                         (an-answer 0 '- scope)
                         (let ((exp-val2 (answer-val (value-of-term term scope))))
                           (an-answer (* exp-val1 exp-val2) '- scope))))))
      (div-term (term factor)
               (let ((exp-val1 (answer-val (value-of-sum sum scope)))
                     (exp-val2 (answer-val (value-of-term term scope))))
                 (an-answer (/ exp-val1 exp-val2) '- scope)))
      (single-term (factor)
                   (value-of-factor factor scope)))))

(define value-of-factor
  (lambda (fact scope)
    (cases factor fact
      (pos-factor (factor)
                  (an-answer factor '- scope))
      (neg-factor (factor)
                  (an-answer (- factor) '- scope))
      (single-factor (pow)
                     (value-of-pow pow scope)))))

(define value-of-power
  (lambda (pow scope)
    (cases power power
      (a-pow (atom factor)
                  (an-answer (expt atom factor) '- scope))
      (a-primary (primary)
                 (value-of-primary primary scope)))))

(define value-of-primary
  (lambda (prim scope)
    (cases primary prim
      (an-atom (atom)
               (value-of-atom atom scope))
      (index-cases (primary exp)
                   (let ((p-list (answer-val (value-of-primary primary scope)))
                         (exp-val (answer-val (value-of-expression exp scope))))
                     (value-of-expression (list-ref p-list exp-val) scope)))
      (zero-arg-func-call (primary)
                          (let ((func (answer-val (value-of-primary primary scope))))
                            (an-answer (apply-func func '() scope) '- scope)))
      (args-func-call (primary args)
                          (let ((func (answer-val (value-of-primary primary scope))))
                            (an-answer (apply-func func args scope) '- scope))))))
(define value-of-atom
  (lambda (atom scope)
    (if (symbol? atom)
        (let ((scope-val (apply-scope scope atom)))
          (if (thunk? scope-val)
              (let ((ans (value-of-thunk scope-val scope)))
                (an-answer (answer-val ans) '- (answer-scope ans)))
              (an-answer scope-val '- scope))))))

(define value-of-thunk
  (lambda (th scope)
    (cases thunk th
      (a-thunk (exp scope)
               (let ((exp-val (value-of-expression exp scope)))
                 (answer exp-val '- (extend-scope exp-val scope)))))))

(define value-of-param-with-default
  (lambda (pwd scope)
    (cases param-with-default pwd
      (a-param-with-default (ID exp)
                            (let ((exp-val (answer-val (value-of-expression exp))))
                              (an-answer exp-val '- (extend-scope scope ID exp-val)))))))
      
