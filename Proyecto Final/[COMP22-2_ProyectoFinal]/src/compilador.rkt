#lang nanopass

#| Compiladores
   Proyecto Final

   Escamilla Soto Cristopher Alejandro 314309253
   Montiel Manriquez Ricardo 314332662
   Villegas Salvador Kevin Ricardo - 314173739 |#

(require "front.rkt")
(require "middle.rkt")
(require "back.rkt")

(require "test/test-front.rkt")
(require "test/test-middle.rkt")
(require "test/test-back.rkt")
(provide (all-defined-out))

;; Ejercicio 5, traducción al lenguaje C
(define (c-language e hash)
  (nanopass-case (L9 Expr) e
                 [,t (match t
                       ['Int "int"]
                       ['Bool "boolean"]
                       ['Char "char"])]
                 [,x (symbol->string x)]
                 [(const ,t ,c) (case t
                                  ['Int (number->string c)]
                                  ['Bool (if c "1" "0")]
                                  ['Char (string c)])]
                 [(primapp ,pr ,e* ...) (match pr
                                          ['+ (string-append (c-language (first e*) hash) " + " (c-language (second e*) hash) ";")]
                                          ['- (string-append (c-language (first e*) hash) " - " (c-language (second e*) hash) ";")]
                                          ['* (string-append (c-language (first e*) hash) " * " (c-language (second e*) hash) ";")]
                                          ['/ (string-append (c-language (first e*) hash) " / " (c-language (second e*) hash) ";")]
                                          ['< (string-append (c-language (first e*) hash) " < " (c-language (second e*) hash) ";")]
                                          ['> (string-append (c-language (first e*) hash) " > " (c-language (second e*) hash) ";")]
                                          ['and (string-append (c-language (first e*) hash) " && " (c-language (second e*) hash))]
                                          ['or (string-append (c-language (first e*) hash) " || " (c-language (second e*) hash))]
                                          ['not (string-append "!" (c-language (first e*) hash))]
                                          ['equal? (string-append (c-language (first e*) hash) " == " (c-language (second e*) hash))]
                                          ['iszereo? (string-append (c-language (first e*) hash) " == 0")]
                                          ['++ (string-append (c-language (first e*) hash) "++")]
                                          ['-- (string-append (c-language (first e*) hash) "--")]
                                          ['car (string-append (c-language (first e*) "[0]"))]
                                          ['length (string-append "sizeof(" (c-language e* hash) ")/sizeof(" (c-language (car e*) hash) ")")]
                                          ['cdr (string-append )])]
                 [(begin ,e* ... ,e) (let ([elements (map (λ (x) (c-language x hash)) (append e* (list e)))])
                                       (string-append "{\n" (saltos elements) "\n}"))]
                 [(if ,e0 ,e1 ,e2) (string-append "if(" (c-language e0 hash) "){\n  " (c-language e1 hash) "\n}else{\n  " (c-language e2 hash) "\n}")]
                 [(let ,x ,body) (let* ([t (c-language (car (hash-ref hash x)) hash)]
                                        [v (valor (cdr (hash-ref hash x)))])
                                   (string-append t " " (symbol->string x) " = " v ";\n  " (c-language body hash) ";"))]
                 [(letrec ,x ,body) (let* ([t (c-language (car (hash-ref hash x)) hash)]
                                        [v (valor (cdr (hash-ref hash x)))])
                                   (string-append t " " (symbol->string x) " = " v ";\n  " (c-language body hash) ";"))]
                 [(letfun ,x ,body) (let* ([t (c-language (car (hash-ref hash x)) hash)]
                                        [v (valor (cdr (hash-ref hash x)))])
                                   (string-append t " " (symbol->string x) " = " v ";\n  " (c-language body hash) ";"))]
                 [(,e0 ,e1) (string-append (c-language e0 hash) "(" (c-language e1 hash) ");")]
                 [(array ,c ,t [,e* ...]) (let ([elements (map (λ (x) (c-language x hash)) e*)])
                                            (string-append (c-language t hash) " m[" (number->string c) "] = {" (comas elements) "};"))]
                 [(while [,e0] ,e1) (string-append "while(" (c-language e0 hash) "){\n  " (c-language e1 hash) ";\n}")]
                 [(for [,x ,e0] ,e1) (let ([l (nanopass-case (L9 Expr) e0
                                                             [(array ,c ,t [,[e*] ...]) c])]
                                           [arr (c-language e0 hash)]
                                           [t (nanopass-case (L9 Expr) e0
                                                             [(array ,c ,t [,[e*] ...]) t])])
                                       (string-append arr "\nfor(int i = 0; i < " l "; i++){\n  " (c-language e1 hash) ";\n}"))]
                 [else (error e)]))

;; Función auxiliar para el manejo de valores extraidos de una expresión padre.
(define (valor expr)
  (define-values (ex tabla) (assigment expr))
  (c-language (list-to-array ex tabla) tabla))

;; Función auxiliar para poner comas "," entre los elementos.
(define (comas l)
  (if (equal? (length l) 1)
      (first l)
      (string-append (car l) ", " (comas (cdr l)))))

;; Función auxiliar para dar formato a las sentencias
(define (saltos l)
  (if (equal? (length l) 1)
      (string-append "  " (first l) ";")
      (string-append "  " (car l) ";\n" (saltos (cdr l)))))

;; Función para leer un archivo
(define (lee x)
  (read (open-input-file (string-append "./mt/" x ".mt"))))

;; Función para escribir un archivo
(define (escribe x text ext)
  (call-with-output-file (string-append x ext) #:exists 'replace
    (λ (out-port)
      (displayln text out-port))))

;; Función que genera el proceso de compilación en las tres etapas
;; Solo recibe el nombre el archivo a tratar sin extensión
(define (compila x)
  (escribe (string-append "front/" x) (front (parser-LF (lee x))) ".fe")
  (escribe (string-append "middle/" x) (middle (parser-LF (lee x))) ".me")
  (define-values (expr hash) (back (parser-LF (lee x))))
  (escribe (string-append "c/" x) (c-language (list-to-array expr hash) hash) ".c")
  (display "Compilado exitosamente\n"))