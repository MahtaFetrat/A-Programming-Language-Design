#lang racket

(require (lib "eopl.ss" "eopl")
         "lexer.rkt"
         "parser.rkt"
         "interpreter.rkt")


(define evaluate
  (lambda (file-inp)
    (let ((inp (apply string-append (file->lines file-inp))))
      (let ((pgm (lex-and-parse (open-input-string inp))))
        (value-of-program pgm)))))

      