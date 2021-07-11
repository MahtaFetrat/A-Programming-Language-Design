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
   (statement statement?)))

;3. Statement → Compound_stmt | Simple_stmt
(define-datatype statement statement?
  (a-compound-stmt
   (compound-stmt compound-stmt?))
  (a-simple-stmt
   (simple-stmt simple-stmt?)))

;4. Simple_stmt → Assignment | Return_stmt | Global_stmt
(define-datatype simple-stmt simple-stmt?
  (an-assign-stmt
   (assignment assignment?))
  (a-return-stmt
   (return-stmt return-stmt?))
  (a-global-stmt
   (global-stmt global-stmt?))
  (pass-stmt)
  (break-stmt)
  (continue-stmt))

;5. Compound_stmt → Function_def | If_stmt | F or_stmt
(define-datatype compound-stmt compound-stmt?
  (a-function-def
   (function-def function-def?))
  (an-if-stmt
   (if-stmt if-stmt?))
  (a-for-stmt
   (for-stmt for-stmt?)))

;6. Assignment → ID ‘ = ‘ Expression
(define-datatype assignment assignment?
  (an-assignment
   (ID symbol?)
   (exp expression?)))

;7. Return_stmt → ‘return‘ | ‘return‘ Expression
(define-datatype return-stmt return-stmt?
  (empty-return-stmt)
  (exp-return-stmt
   (exp expression?)))

;8. Global_stmt → ‘global‘ ID
(define-datatype global-stmt global-stmt?
  (a-global-stmt
   (ID symbol?)))

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



               
