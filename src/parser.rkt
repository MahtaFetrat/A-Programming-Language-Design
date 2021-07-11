#lang racket

(require parser-tools/yacc)

(require "lexer.rkt"
         "grammar.rkt")

(define py-parser
  (parser
   (start program)
   (end EOF)
   (error (lambda (tok-ok? tok-name tok-value) (error 'parser "problem parsing '~s ~s'" tok-name to-value)))
   (tokens a b)
   (grammar
    (program
     ((statements EOF) (a-program $1)))
    (statements
     ((statement SEMICOL) (single-statement $1))
     ((statements statements SEMICOL) (mult-statement $1 $2)))
    (statement
     ((simple-stmt) (a-single-stmt $1))
     ((compound-stmt) (a-compound-stmt $1)))
    (simple-stmt
     ((assignment) (assignment-simple-stmt $1))
     ((return-stmt) (return-simple-stmt $1))
     ((global-stmt) (global-simple-stmt $1))
     ((PASS) (pass-stmt))
     ((CONTINUE) (continue-stmt))
     ((BREAK) (break-stmt)))
    (compound-stmt
     ((function-def) (func-def-comp-stmt $1))
     ((if-stmt) (if-comp-stmt $1))
     ((for-stmt) (for-comp-stmt $1)))
    (assignment
     ((ID ASSIGN expression) (an-assignment $1 $3)))
    (return-stmt
     ((RETURN) (empty-return-stmt))
     ((RETURN expression) (exp-return-stmt $2)))
    (global-stmt
     ((GLOBAL ID) (a-global-stmt $2)))
    (function-def
     ((DEF ID OPEN-PAR params CLOSE-PAR COLON statements) (params-func-def $2 $4 $7))
     ((DEF ID ZERO-ARG COLON statements) (zero-param-func-def $2 $5)))
    (params
     ((param-with-defaulta) (single-param $1))
     ((params COMMA params-with-default) (mult-param $1 $3)))
    (param-with-default
     ((ID ASSIGN expression) (a-param-with-default $1 $3)))
    (if-stmt
     ((IF expression COLON statements else-block) (an-if-stmt $2 $4 $5)))
    (else-block
     ((ELSE COLON statments) (an-else-stmt $3)))
    (for-stmt
     ((FOR ID IN expression COLON statements) (a-for-stmt $2 $4 $6)))
    (expression
     ((disjunction) (an-expression $1)))
    (disjuction
     ((conjuction) (single-disjunction $1))
     ((disjunction OR conjuction) (mult-disjunction $1 $3)))
    (conjuction
     ((inversion) (single-conjuction $1))
     ((conjuction AND inversion) (mult-conjuction $1 $3)))
    (inverion
     ((NOT inversion) (not-inversion $2))
     ((comparison) (a-comparison $1)))
    (comparison
     ((sum compare-op-sum-pairs) (mult-comparison $1 $2))
     ((sum) (single-comparison $1)))
    (compare-op-sum-pairs
     ((compare-op-sum-pair) (single-compare-op-sum-pairs $1))
     ((compare-op-sum-pairs compare-op-sum-pair) (mult-compare-op-sum-pairs $1 $2)))
    (compare-op-sum-pair
     ((eq-sum) (eq-sum-pair $1))
     ((lt-sum) (lt-sum-pair $1))
     ((gt-sum) (gt-sum-pair $1)))
    (eq-sum
     ((EQUAL sum) (an-eq-sum $2)))
    (lt-sum
     ((LESS sum) (an-lt-sum $2)))
    (gt-sum
     ((GREATER sum) (a-gt-sum $2))))))
    
