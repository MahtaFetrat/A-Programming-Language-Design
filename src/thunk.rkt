#lang racket

(require "environment.rkt"
         "grammar.rkt")

(define-datatype thunk
  (a-thunk
   (exp expression?)
   (scope scope?)))
