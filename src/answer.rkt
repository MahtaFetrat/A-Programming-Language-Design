#lang eopl

(require "environment.rkt"
         "grammar.rkt")

(define expval?
  (lambda (e) (or (number? e) (boolean? e) (null? e) (py-list? e))))

(define-datatype answer answer?
  (an-answer
   (value expval?)
   (message symbol?)
   (scope scope?)))

(define answer-val
  (lambda (ans)
    (cases answer ans
      (an-answer (val msg sc) val))))

(define answer-scope
  (lambda (ans)
    (cases answer ans
      (an-answer (val msg sc) sc))))

(define return-message?
  (lambda (ans)
    (cases answer ans
      (an-answer (val msg sc) (eqv? msg 'return)))))

(define continue-message?
  (lambda (ans)
    (cases answer ans
      (an-answer (val msg sc) (eqv? msg 'continue)))))

(define break-message?
  (lambda (ans)
    (cases answer ans
      (an-answer (val msg sc) (eqv? msg 'break)))))


;comparison answer ---------------------------------------------------------------
(define-datatype cmp-answer cmp-answer?
  (a-cmp-answer
   (result boolean?)
   (right-hand-operand atom?)))

(define cmp-res
  (lambda (cmp-ans)
    (cases cmp-answer cmp-ans
      (a-cmp-ans (res rh) res))))

(define cmp-right-hand-operand
  (lambda (cmp-ans)
    (lambda (cmp-ans)
      (cases cmp-answer cmp-ans
        (a-cmp-ans (res rh) rh)))))
    
   
