#lang nanopass

#| Compiladores
   Proyecto Final

   Escamilla Soto Cristopher Alejandro 314309253
   Montiel Manriquez Ricardo 314332662
   Villegas Salvador Kevin Ricardo - 314173739 |#

(provide (all-defined-out))

(require "../front.rkt")

;; Ejemplo 1
(remove-one-armed-if
 (parser-LF
  '(if (and #t #f) (+ 42 0))
  )
 )

;; Ejemplo2
(remove-one-armed-if
 (parser-LF
  '(funF fun1 ((x1 Int) (x2 Int)) Int (if (and #t #f) (+ x1 x9 1)))
  )
 )

;; Ejemplo3
(remove-one-armed-if
 (parser-LF
  '(fun ((x1 Int) (x2 Int)) Int (if (and #t #f) (+ x1 x9 1)))
  )
 )

;; Ejemplo 4
(remove-one-armed-if
 (parser-LF
  '(if (and #t #f) (+ 42 0) (if (+ 19 77) (- 20 49)))
  )
 )

(remove-string (parser-L1 '(list 1 2 "holi")))
(remove-string (parser-L1 '(+ #\c #\h #\o #\l #\o)))
(remove-string (parser-L1 '(+ "caca" "huate")))

(identify-assigments (parser-L3 '(let ([foo Lambda (lambda ([x Int]) x)]) (foo 5))))

(curry-let (parser-L2 '(let ([x Int 4] [y Int 6]) (+ x y))))

(verify-arity (parser-L4 '(primapp + 2 3)))
;(verify-arity (parser-L4 '(primapp car 2 3))) ;Descomentar para poder verificar

(un-anonymous (parser-L3 '(lambda ([x Bool]) (if x 1 2))))

(verify-vars (parser-L4 '(let ([x Int 4]) (let ([y Int 6]) (primapp + x y)))))
;(verify-vars (parser-L4 '(primapp + 2 x))) ;Descomentar para poder verificar

(display (parser-L4 '(foo x y)))
(display " | curry: ")
(display (curry (parser-L4 '(foo x y))))
(display "\n")

(display (parser-L4 '(lambda ([x Int] [y Int]) (primapp + x y))))
(display " | curry: ")
(display (curry (parser-L4 '(lambda ([x Int] [y Int]) (primapp + x y)))))
(display "\n")