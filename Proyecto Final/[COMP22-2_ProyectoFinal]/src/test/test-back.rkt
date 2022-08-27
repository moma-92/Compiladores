#lang nanopass

#| Compiladores
   Proyecto Final

   Escamilla Soto Cristopher Alejandro 314309253
   Montiel Manriquez Ricardo 314332662
   Villegas Salvador Kevin Ricardo - 314173739 |#

(provide (all-defined-out))

(require "../front.rkt")
(require "../middle.rkt")
(require "../back.rkt")

(display (parser-L7 '(letrec ([foo (Int → Int) (lambda ([x Int]) x)]) (foo (const Int 5)))))
(display " | assigment: ")
(display (symbol-table-var (parser-L7 '(letrec ([foo (Int → Int) (lambda ([x Int]) x)]) (foo (const Int 5))))))