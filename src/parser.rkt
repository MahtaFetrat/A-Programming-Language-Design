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
     ((ID ASSIGN expression) (a-param-with-default $1 $3))))))
    
