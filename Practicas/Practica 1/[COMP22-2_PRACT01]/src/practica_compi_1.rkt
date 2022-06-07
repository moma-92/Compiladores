#lang racket
;;Autor Cristopher Alejandro Escamilla Soto
;;Autor Ricardo Montiel Manriquez

;///////////////////////////////////  EJERCICIO 1  ////////////////////////////////////////
;Escribe una función que al ingresar la edad en años de una persona determine cuántos
;segundos, minutos y horas han pasado desde su nacimiento hasta el presente (o el momento en
;que es invocada esta función).
(define (ejercicio1 años)
  (printf "Has vivido : Dias Horas Minutos Segundos  " )
  (list (suma-años (calcula-año años))
        (*(suma-años (calcula-año años)) 24)
        (*(*(suma-años (calcula-año años)) 24) 60)
        (*(*(*(suma-años (calcula-año años)) 24) 60) 60))
  
  )

;; ////// Metodos Auxiliares para el inciso 1 //////

;; Función que calcula si un año fue viciesto:
(define (leap-year? year)
  (or (= 0 (modulo year 400))
      (and (= 0 (modulo year 4))
           (not (= 0 (modulo year 100)))))
  )
  
;; Funcion que calcula cual es su primer año
(define (calcula-año año)
  (+(- 2022 año) 1))

;; Funcion que calcula los dias considerando los años viciestos, apartir
;; de su primer año hasta el 2022
(define (suma-años año)
  (cond
   [(= año 2022) (if (leap-year? 2022)
                     366
                     365)]
   [(< año 2022)   
                   (if (leap-year? año)
                     (+ 366 (suma-años (+  año 1)))
                     (+ 365 (suma-años (+  año 1)))
                     ) ]
   )
  )

;; ////// |||||||||||||||||||||||||||||||||||//////
  
;///////////////////////////////////  EJERCICIO 2  ////////////////////////////////////////
;; Ejercicio 2
;; Escribe una función que tome una lista con al menos un elemento y un predicado como
;; argumento y elimine los elementos de la lista que no satisfagan el predicado.

(define (ejercicio2 lst fun )
     (cond
       ((null? lst)  lst)
       ((not(fun (car lst)) )     (ejercicio2 (cdr lst) fun))
       (else              (union-listas (list (car lst)) (ejercicio2 (cdr lst) fun)))) 
  
  )

;; ////// Metodos Auxiliares para el inciso 2 //////
  
;;Funcion auxiliar para unir listas sin elementos repetidos
;; union-listas: (listof a) (listof b)-> (listof a)
(define (union-listas l1 l2)
  (match l1
      ['() l2]
      [(cons x xs)
           (if (pertenece? x l2)
               (union-listas xs l2)
               (cons x (union-listas xs l2)))]))


;; Función que recibe una Lista1 y elimina todas las
;; precencias en ella del elemento x
;; elimina: number (list)-> (list)
(define (elimina x Lista1)
(if (null? Lista1)
    '()
    (if (eq? x (car Lista1))
        (elimina x (cdr Lista1))
        (cons (car Lista1) (elimina x (cdr Lista1))))))

;; Función que recibe un elemento a, una lista l y decide si a pertenece a l.
;; pertenece: a (listof a) -> boolean
(define (pertenece? a l)
 (cond 
    ((null? l) #f)
    ((equal? a (car l)))
    (else (pertenece? a (cdr l)))))

;; ////// |||||||||||||||||||||||||||||||||||//////

;///////////////////////////////////  EJERCICIO 3  ////////////////////////////////////////
;;  Escribe una definición para una estructura que sea una lista de números, llamada LON
;; (List Of Numbers).
  
(define-struct LON (cabeza cola)#:transparent)

  ;; Ejemplo (make-LON 1 '( 1 1 1 1))

;; Diseña una función any-even? que determina si dada una lista de números, bajo la defini-
;; ción anterior, contiene un número par.

(define (any-even? dato)
  (cond 
    ((even? (LON-cabeza dato )) #t)
    ((par?  (LON-cola dato)) #t) 
    (else #f)
    )
    )
;; Diseña una función total-length que consume una lista de números y calcula su longitud.
(define (total-length dato)
  (if (equal? null (LON-cabeza dato ))
                     0
                     (+ 1 (longitud (LON-cola dato )))
                     )
    )


;; ////// Metodos Auxiliares para el inciso 3 //////
;; Escribe una función que tome una lista con al menos un elemento y un predicado como
;; argumento y elimine los elementos de la lista que no satisfagan el predicado

;; Funcion que nos dice si existe un elemento en la lista que sea par
  (define (par? lst)
    (cond 
    ((null? lst) #f)
    ((even? (car lst)) #t) 
    (else (par?(cdr lst)))
    )
   )

;; Funcion que nos dice la longitud de una lista

  (define (longitud lst)
  (cond
     ((null? lst)  0)
     (else (+ 1 (longitud (cdr lst)))))
  )

;///////////////////////////////////  EJERCICIO 4  ////////////////////////////////////////
; El árbol de divisores de un número n tiene como raíz n, como hijo
; izquierdo su divisor mas pequeño y como hijo derecho el cociente.
; Las hojas del árbol son todos números primos.

(define-struct node (num left right) #:transparent)
(define-struct leaf (num)            #:transparent)

; a) div-tree que recibe un entero positivo n y regresa el árbol de divisores
; de n. Para esto es necesario definir una estructura tree.

(define (div-tree n)
  (letrec ([arbol (λ (n lista-primos)
                    (if (= (length lista-primos) 1)
                        (leaf n)
                        (node n (leaf (car lista-primos)) (arbol (/ n (car lista-primos)) (cdr lista-primos))))
                    )]
           )
    (arbol n (prime-fac n)))
  )

; b) prime-fac que dado un número n regresa una lista con la representación
; de n como producto de factores primos.

(define (prime-fac n)
  (letrec ([calcula-primo (λ (num a b) (cond
                                     [(= num 1) b]
                                     [(= (modulo num a) 0) (calcula-primo (/ num a) a (append b (list a)))]
                                     [else (calcula-primo num (+ 1 a) b)]))]
           )
    (calcula-primo n 2 '())
    )
  )

;///////////////////////////////////  EJERCICIO 5  ////////////////////////////////////////
; Los árboles binarios leafy son aquellos que tienen el símbolo 'leaf en todas
; las hojas. Además, los nodos internos no almacenan información es decir, los
; árboles leafy sólo representan la estructura de árboles binarios.



;///////////////////////////////////  EJERCICIO 6  ////////////////////////////////////////
; Da una definición que dada una lista de árboles binarios leafy, filtre los
; que son árboles binarios perfectos.