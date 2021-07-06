#lang eopl

(provide (all-defined-out))

;1. P rogram → Statements EOF
(define-datatype program program?
  (a-program
   (statements statement?)))

;2. Statements → Statement ‘; ‘ | Statements Statement ‘; ‘
(define-datatype statements statements?
  (single-statement
   (statement statement?))
  (mult-statement
   (statements statements?)
   (sattemen statement?)))

;3. Statement → Compound_stmt | Simple_stmt
(define-datatype statement statement?
  (a-compound-statement
   (comp-statement compound-statement?))
  (a-simple-statement
   (sim_statement simple-statement?)))

"
.
.
.
rest of the grammar from number  4 to 30 and 32 to 33 will we defined in the same way"

;31. Atom → ID | ‘True‘ | ‘False‘ | ‘None‘ | NUMBER | List
(define atom?
  (lambda (a) (or (symbol? a) (boolean? a) (null? a) (number? a) (py-list? a))))

"Atom is defined this way for the ease of computations on racket defined values,
otherwise it would be some define-datatype for our oown defined NUMBER, BOOLEAN, ...
so better to define py-list as racket list of Expressions so that predefined functions like cons, append, ... will work for it"



               