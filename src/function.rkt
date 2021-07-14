#lang eopl

(require "environemnt.rkt"
         "grammar.rkt")

(define-datatype function
  (a-function
   (ID symbol?)
   (params params?)
   (statements statements?)
   (scope scope?)))
