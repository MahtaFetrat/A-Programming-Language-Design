#lang eopl

(provide (all-defined-out))

;1. Program → Statements EOF
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

;5. Compound_stmt → Function_def | If_stmt | For_stmt
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

;9. Function_def → ‘def‘ ID ‘(‘ Params ‘)‘ ‘ : ‘ Statements|‘def‘ ID ‘() : ‘ Statements
(define-datatype function-def function-def?
  (zero-param-func-def
   (ID symbol?)
   (statements statements?))
  (params-func-def
   (ID symbol?)
   (params params?)
   (statements statements?)))

;10. Params → Param_with_default | Params ‘, ‘ Param_with_default
(define-datatype params params?
  (single-param
   (param-with-default param-with-default?))
  (mult-param
   (params params?)
   (param-with-default param-with-default?)))

;11. Param_with_default → ID ‘ = ‘ Expression
(define-datatype param-with-default param-with-default?
  (a-param-with-default
   (ID symbol?)
   (exp expression?)))

;12. If_stmt → ‘if‘ Expression ‘ : ‘ Statements Else_block
(define-datatype if-stmt if-stmt?
  (an-if-stmt
   (exp expression?)
   (statements statements?)
   (else-block else-block?)))

;13. Else_block → ‘else‘ ‘ : ‘ Statements
(define-datatype else-block else-block?
  (an-else-block
   (statements statements?)))

;14. For_stmt → ‘for‘ ID ‘in‘ Expression ‘ : ‘ Statements
(define-datatype for-stmt for-stmt?
  (a-for-stmt
   (ID symbol?)
   (exp expression?)
   (statements statements?)))
 
;15. Expression → Disjunction
(define-datatype expression expression?
  (an-expression
   (disjunction disjunction?)))

;16. Disjunction → Conjunction | Disjunction ‘or‘ Conjunction
(define-datatype disjunction disjunction?
  (single-disjunction
   (conjunction conjunction?))
  (mult-disjunction
   (conjunction conjunction?)
   (disjunction disjunction?)))

;17. Conjunction → Inversion | Conjunction ‘and‘ Inversion
(define-datatype conjunction conjunction?
  (single-conjunction
   (inversion inversion?))
  (mult-conjunction
   (inversion inversion?)
   (conjunction conjunction?)))

;18. Inversion → ‘not‘ Inversion | Comparison
(define-datatype inversion inversion?
  (not-inversion
   (invresion inversion?))
  (a-comparison
   (comparison comparison?)))

;19. Comparison → Sum Compare_op_Sum_pairs | Sum
(define-datatype comparison comparison?
  (single-comparison
   (sum sum?))
  (mult-comparison
   (sum sum?)
   (compare-op-sum-pairs compare-op-sum-pairs?)))

;20. Compare_op_Sum_pairs → Compare_op_Sum_pair| Compare_op_Sum_pairs Compare_op_Sum_pair
(define-datatype compare-op-sum-pairs compare-op-sum-pairs?
  (single-compare-op-sum-pairs
   (compare-op-sum-pair compare-op-sum-pair?))
  (mult-compare-op-sum-pairs
   (compare-op-sum-pairs compare-op-sum-pairs?)
   (compare-op-sum-pair compare-op-sum-pair?)))

;21. Compare_op_Sum_pair → Eq_Sum | Lt_Sum| Gt_Sum
(define-datatype compare-op-sum-pair compare-op-sum-pair?
  (eq-sum-pair
   (eq-sum eq-sum?))
  (lt-sum-pair
   (lt-sum lt-sum?))
  (gt-sum-pair
   (gt-sum gt-sum?)))

;22. Eq_Sum → ‘ == ‘ Sum
(define-datatype eq-sum eq-sum?
  (an-eq-sum
   (sum sum?)))

;23. Lt_Sum → ‘ < ‘ Sum
(define-datatype lt-sum lt-sum?
  (a-lt-sum
   (sum sum?)))

;24. Gt_Sum → ‘ > ‘ Sum
(define-datatype gt-sum st-sum?
  (a-gt-sum
   (sum sum?)))

;25. Sum → Sum ‘ + ‘ Term | Sum ‘ - ‘ Term | Term
(define-datatype sum sum?
  (single-sum
   (term term?))
  (add-sum
   (sum sum?)
   (term term?))
  (sub-sum
   (sum sum?)
   (term term?)))

;26. Term → Term ‘ ∗ ‘ Factor | Term ‘/‘ Factor | Factor
(define-datatype term term?
  (single-term
   (factor factor?))
  (mul-term
   (term term?)
   (factor factor?))
  (div-term
   (term term?)
   (factor factor?)))
   
;27. Factor → ‘ + ‘ Factor | ‘ - ‘ Factor | Power
(define-datatype factor factor?
  (single-factor
   (power power?))
  (pos-factor
   (factor factor?))
  (neg-factor
   (factor factor?)))
  
;28. Power → Atom ‘ ∗ ∗‘ Factor | Primary
(define-datatype power power?
  (a-primary
   (primary primary?))
  (a-pow
   (atom atom?)
   (factor factor?)))
   
;29. Primary → Atom | Primary ‘[‘ Expression ‘]‘ | Primary ‘()‘| Primary ‘(‘ Arguments ‘)‘
(define-datatype primary primary?
  (an-atom
   (atom atom?))
  (index-access
   (primary primary?)
   (exp expression?))
  (zero-arg-func-call
   (primary primary?))
  (args-func-call
   (primary primary?)
   (arguments arguments?)))

;30. Arguments → Expression | Arguments ‘, ‘ Expression
(define-datatype arguments arguments?
  (single-arguments
   (exp expression?))
  (mult-arguments
   (arguments arguments?)
   (exp expression?)))
   
;32. List → ‘[‘ Expressions ‘]‘ | ‘[]‘
(define-datatype py-list py-list?
  (empty-py-list)
  (exps-py-list
   (exps expressions?)))

;33. Expressions → Expressions ‘, ‘ Expression | Expression
(define expressions?
  (single-expressions
   (exp expression?))
  (mult-expressions
   (exps expressions?)
   (exp expression?)))
         
   
"Atom is defined this way for the ease of computations on racket defined values,
otherwise it would be some define-datatype for our own defined NUMBER, BOOLEAN, ...
so later, its better to redefine py-list as racket list of Expressions so that predefined functions like cons, append, ... will work for it"



               
