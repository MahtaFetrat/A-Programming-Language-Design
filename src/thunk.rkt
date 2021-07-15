#lang eopl

(require "environment.rkt"
         "grammar.rkt")

(provide (all-defined-out))

(define-datatype thunk thunk?
  (a-thunk
   (exp expression?)
   (scope scope?)))
