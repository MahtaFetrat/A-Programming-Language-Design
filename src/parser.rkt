#lang racket

"
based on lexer information, one of the grammar types in 'grammar.rkt' will be created

for more detail: see the the sample parser provided in the doc:
...
(define simple-math-parser
           (parser
            (start exp)
            (end EOF)
            (error void)
            (tokens a b)
            (grammar
           (exp ((exp plus NUM) (list 'plusnumbers $1 $3)) ((NUM) (list 'anumber $1)))
             )))
...
the returning result of the 'simple-math-parser' is a list of required values. Instead, it could be an eopl datatype.

e.g:
...
(define simple-math-parser
           (parser
            (start exp)
            (end EOF)
            (error void)
            (tokens a b)
            (grammar
           (exp ((exp plus NUM) (plusnumbers $1 $3)) ((NUM) (a-number $1)))
             )))
...
where:
(define-datatype anumber anumber?
     (a-number
          (num number?)))
and plusnumber is another datatype like this"