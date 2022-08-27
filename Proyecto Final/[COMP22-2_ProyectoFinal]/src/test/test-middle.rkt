#lang nanopass

#| Compiladores
   Proyecto Final

   Escamilla Soto Cristopher Alejandro 314309253
   Montiel Manriquez Ricardo 314332662
   Villegas Salvador Kevin Ricardo - 314173739 |#

(provide (all-defined-out))

(require "../front.rkt")
(require "../middle.rkt")

(display (parser-L5 '(quot 5)))
(display " | type-const ")
(display (type-const (parser-L5 '(quot 5))))
(display "\n")

(display (parser-L5 '(quot #t)))
(display " | type-const ")
(display (type-const (parser-L5 '(quot #t))))
(display "\n")

(display (parser-L5 '(quot #\h)))
(display " | type-const ")
(display (type-const (parser-L5 '(quot #\h))))
(display "\n")

(display (parser-L6 '(letrec ([foo Lambda (lambda ([x Int]) x)]) (foo (const Int 5)))))
(display " | type-infer: ")
(display (type-infer (parser-L6 '(letrec ([foo Lambda (lambda ([x Int]) x)]) (foo (const Int 5))))))
(display "\n")

(display (parser-L6 '(let ([x List (list)]) x)))
(display " | type-infer: ")
(display (type-infer (parser-L6 '(let ([x List (list)]) x))))
(display "\n")

(display (parser-L6 '(let ([x List (list (const Int 1) (const Int 2) (const Int 3) (const Int 4))]) x)))
(display " | type-infer: ")
(display (type-infer (parser-L6 '(let ([x List (list (const Int 1) (const Int 2) (const Int 3) (const Int 4))]) x))))