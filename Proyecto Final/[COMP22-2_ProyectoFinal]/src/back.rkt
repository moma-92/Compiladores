#lang nanopass

#| Compiladores
   Proyecto Final

   Escamilla Soto Cristopher Alejandro 314309253
   Montiel Manriquez Ricardo 314332662
   Villegas Salvador Kevin Ricardo - 314173739 |#

(require "front.rkt")
(require "middle.rkt")
(provide (all-defined-out))

;--------------------- Proceso 12 | assigment ---------------------

;; Función auxiliar para almacenar los símbolos de una expresión del lenguaje
(define (symbol-table e table)
  (nanopass-case (L7 Expr) e
                 [(begin ,e* ... ,e)
                  (begin (map (λ (x) (symbol-table x table)) e*))]
                 [(primapp ,pr ,[e*] ...)
                  (let params ([expr* e*])
                    (if (null? expr*)
                        table
                        (symbol-table (first expr*) (params (rest expr*)))))]
                 [(if ,e0 ,e1 ,e2)
                  (begin (symbol-table e0 table)
                         (symbol-table e1 table)
                         (symbol-table e2 table))]
                 [(lambda ([,x* ,t*] ...) ,body)
                  (symbol-table body table)]
                 [(let ([,x ,t ,e]) ,body)
                  (begin (hash-set! (symbol-table body table) x (cons t e))
                         (symbol-table body table))]
                 [(letrec ([,x ,t ,e]) ,body)
                  (begin (hash-set! (symbol-table body table) x (cons t e))
                         (symbol-table body table))]
                 [(letfun ([,x ,t ,e]) ,body)
                  (begin (hash-set! table x (cons t e))
                         (symbol-table body table))]
                 [(list ,e* ... ,e)
                  (begin (map (λ (x) (symbol-table x table)) e*))]
                 [(,e0 ,e1)
                  (begin (define t1 table)
                         (set! t1 (symbol-table e1 t1))
                         (define t2 t1)
                         (set! t2 (symbol-table e0 t2))
                         t2)]
                 [(while [,[e0]] ,e1)
                  (begin (symbol-table e0 table)
                         (symbol-table e1 table))]
                 [(for [,x ,[e0]] ,e1)
                  (begin (symbol-table e0 table)
                         (symbol-table e1 table))]
                 [else table]))

;; Genera la tabla de símbolos de una expresión del lenguaje
(define (symbol-table-var e)
  (nanopass-case (L7 Expr) e
                 [else (symbol-table e (make-hash))]))

;; Lenguaje L11
(define-language L8
  (extends L7)
  (Expr (e body)
        (- (let ([x* t* e*]) body* ... body)
           (letrec ([x* t* e*]) body* ... body)
           (letfun ([x* t* e*]) body* ... body))
        (+ (let x body)
           (letrec x body)
           (letfun x body))))

;; Parser para el lenguaje L12
(define-parser parser-L8 L8)

;; Elimina el valor asociado a los identificadores y el tipo correspondiente
(define-pass assigment : L7 (e) -> L8 (tabla)
  (Expr : Expr (e) -> Expr ()
        [(let ([,x ,t ,e]) ,[body]) `(let ,x ,body)]
        [(letrec ([,x ,t ,e]) ,[body]) `(letrec ,x ,body)]
        [(letfun ([,x ,t ,e]) ,[body]) `(letfun ,x ,body)])
  (values (Expr e) (symbol-table-var e)))

;--------------------- Proceso 13 | list-to-array ---------------------

;; Lenguaje L9
(define-language L9
  (extends L8)
  (Expr (e body)
        (- (list e* ...))
        (+ (array c t [e* ...]))))

(define-parser parser-L9 L9)

;; Convertimos las listas a un arreglo donde tendrémos la longitud, tipo de la lista y sus elementos
(define-pass list-to-array : L8 (e hash) -> L9 ()
  (Expr : Expr (e) -> Expr ()
        [(list ,[e*] ...) (let ([t (J (parser-L6 (unparse-L8 e)) hash)])
                            `(array ,(length e*) ,(third t) [,e* ...]))]))

;--------------------- Back-end ---------------------

(define (back expr)
  (assigment (middle expr)))