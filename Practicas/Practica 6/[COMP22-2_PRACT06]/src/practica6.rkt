#lang nanopass

#|
   Compiladores 2022-2
   Practica 6

   Autores:
   Montiel Manriquez Ricardo
   Escamilla Soto Cristopher Alejandro
|#

;; Definicion del lenguaje L10
(define-language L10
  (terminals
   (variable (x))
   (primitive (pr))
   (constant (c))
   (type (t))
   )
  (Expr (e body)
        x
        (const t c)
        (begin e* ... e)
        (primapp pr e* ...)
        (if e0 e1 e2)
        (lambda ([x t]) body)
        (let ([x t e]) body)
        (letrec ([x t e]) body)
        (letfun ([x t e]) body)
        (list e* ...)
        (e0 e1)))

;; Predicados para cada terminal
(define fun-count 0)

;; Predicado para variables
(define (variable? x)
  (and (symbol? x)
       (not (primitive? x))
       (not (constant? x))))

;; Definimos los operadores primitivos
;; Segun el leguaje son: + - * / length car cdr.
(define (primitive? x)
  (memq x '(+ - * / length car cdr)))

;; Definimos nuestras costantes:
;; Integer, Boolean, Char
(define (constant? x)
  (or (integer? x)
      (char? x)
      (boolean? x)))

;; Definimos nuestro Sistema de Tipos
;; Int | Chat | Bool | Lambda | List | (List of T) | (T -> T)
(define (type? x) (or (b-type? x) (c-type? x)))

(define (b-type? x) (memq x '(Bool Char Int List String Lambda)))

(define (c-type? x) (if (list? x)
                        (let* (
                               [f (car x)]
                               [s (cadr x)]
                               [t (caddr x)])
                          (or (and (equal? f 'List) (equal? s 'of) (type? t))
                              (and (type? f) (equal? s '→) (type? t))))
                        #f))

(define (arit? x) (memq x '(+ - * /)))

(define (lst? x) (memq x '(length car cdr)))

;; Definimos el parser de L10
(define-parser parser-L10 L10)

;; EJERCICIO 1

;; Definimos un nuevo lenguaje L11 en el cual el constructor lambda vuelve a ser multiparametrico
(define-language L11
  (extends L10)
  (Expr (e body)
        (- (lambda ([x t]) body))
        (+ (lambda ([x* t*] ...) body))))

;; Definimos el parser de L11
(define-parser parser-L11 L11)

(define (uncurry expr)
    (nanopass-case (L10 Expr) expr
        [(let ([,x ,t ,[e]]) ,[body])   (with-output-language (L11 Expr) `(let ([,x ,t ,e]) ,body))]
        [(letrec ([,x ,t ,[e]]) ,[body])(with-output-language (L11 Expr) `(letrec ([,x ,t ,e]) ,body))]
        [(letfun ([,x ,t ,[e]]) ,[body])(with-output-language (L11 Expr) `(letfun ([,x ,t ,e]) ,body))]
        [(begin ,[e*] ... ,[e])         (with-output-language (L11 Expr) `(begin ,e* ... ,e))]
        [(primapp ,pr ,[e*] ...)        (with-output-language (L11 Expr) `(primapp ,pr ,e* ...))]
        [(if ,[e0] ,[e1] ,[e2])         (with-output-language (L11 Expr) `(if ,e0 ,e1 ,e2))]
        [(const ,t ,c)                  (with-output-language (L11 Expr) `(const ,t ,c))]
        [(list ,[e*] ...)               (with-output-language (L11 Expr) `(list ,e* ...))]
        [(,[e0] ,[e1])                  (with-output-language (L11 Expr) `(,e0 ,e1))]
        [(lambda ([,x ,t]) ,body)       (parser-L11 `(lambda ,(lst-lambda expr '()) ,(unparse-L11 (uncurry (body-lambda expr)))))]
        [else (parser-L11 (unparse-L10 expr))]
     )
 )

;; EJERCICIO 2

;; Funcion  que genera la tabla de sımbolos de una expresion del lenguaje
;; symbol-table-var :: expr -> '#hash

(define (symbol-table-var expr)
    (nanopass-case (L11 Expr) expr
                    [else (aux expr (make-hash))]))

;; EJERCICIO 3

;; Definimos un nuevo lenguaje L12 en el que los constructores de asignacion solo reciben una variable.
(define-language L12
  (extends L11)
  (Expr (e body)
        (- (let ([x t e]) body)
           (letrec ([x t e]) body)
           (letfun ([x t e]) body))
        (+ (let x body)
           (letrec x body)
           (letfun x body))))

;; Definimos el parser de L12
(define-parser parse-L12 L12)

;; Función que modifica los constructores let, letrec y letfun, eliminando el valor asociado a los identificadores y el tipo correspondiente.
(define-pass assigment : L11 (ir) -> L12 (hash)
  (Expr : Expr (ir) -> Expr ()
        [(let ([,x ,t ,e]) ,[body]) `(let ,x ,body)]
        [(letrec ([,x ,t ,e]) ,[body]) `(letrec ,x ,body)]
        [(letrec ([,x ,t ,e]) ,[body]) `(letfun ,x ,body)])
  (values (Expr ir) (symbol-table-var ir)))

;; FUNCIONES AUXILIARES

;; Funcion que obtiene la lista de asignaciones que se tienen en las expresiones lambdas:
(define (lst-lambda expr acum)
    (nanopass-case (L10 Expr) expr
        [(lambda ([,x ,t]) ,body)  (append (list (list x t)) (lst-lambda body acum)) ]
        [else acum]))

;; Funcion que nos regresa el cuerpo de una expresion lambda
;; body-lambda:: expr -> expr | expr pertenece a L10
(define (body-lambda expr)
    (nanopass-case (L10 Expr) expr
        [(lambda ([,x ,t]) ,body) (body-lambda body)]
        [else expr]))

;; Funcion auxiliar que se encarga de la creacion de la tabla hash dependiendo del caso expr
(define (aux expr table)
    (nanopass-case (L11 Expr) expr
        [(let ([,x ,t ,e]) ,body)
         (begin
           (hash-set!
            (aux body table) x (cons t e))
          (aux body table))
         ]        
        [(letrec ([,x ,t ,e]) ,body)
         (begin
           (hash-set!
            (aux body table) x (cons t e))
           (aux body table))
         ]
        [(letfun ([,x ,t ,e]) ,body)
         (begin
           (hash-set!
            (aux body table) x (cons t e))
           (aux body table))
         ]
        [(,e0 ,e1)
         (begin
           (define fun1 table)
           (set! fun1 (aux e1 fun1))
           (define fun2 fun1)
           (set! fun2 (aux e1 fun2)) fun2)
         ]
        [(primapp ,pr ,[e*] ...)
         (let f ([e* e*])
           (if (null? e*)
               table
               (aux (first e*) (f (rest e*)))
            ))]
        [(begin ,e* ... ,e)
         (begin
           (map
            (lambda (e) (aux e table))
            e*))
         ]
        [(if ,e0 ,e1 ,e2)
         (begin
           (aux e0 table)(aux e1 table)(aux e2 table)
          )
         ]
        [(lambda ([,x* ,t*] ...) ,body) (aux body table)]
        [(list ,e* ... ,e)
         (begin
           (map
            (lambda (e) (aux e table))
            e*))
         ]
        [else table]))

;; EJEMPLOS

; Ejemplo 1 del ejercicio 1
(printf "Ejemplo 1 del Ejercicio 1 \n")
(printf "Entrada: (parser-L11 '(lambda ([x Int]) x))\n")
(printf "Salida: ") (parser-L11 '(lambda ([x Int]) x))
(printf "Respuesta deseada: (language:L11 '(lambda ((x Int)) x))\n")
(printf "\n")

; Ejemplo 2 del ejercicio 1
(printf "Ejemplo 2 del Ejercicio 1 \n")
(printf "Entrada: (uncurry (parser-L10 '(lambda ([x Int]) x)))\n")
(printf "Salida: ") (uncurry (parser-L10 '(lambda ([x Int]) x)))
(printf "Respuesta deseada: (language:L11 '(lambda ((x Int)) x))\n")
(printf "\n")

; Ejemplo 3 del ejercicio 1
(printf "Ejemplo 3 del Ejercicio 1 \n")
(printf "Entrada: (parser-L11 '(lambda ([x Int]) (lambda ([y Int]) (primapp + x y))))\n")
(printf "Salida: ") (parser-L11 '(lambda ([x Int]) (lambda ([y Int]) (primapp + x y))))
(printf "Respuesta deseada: (language:L11 '(lambda ((x Int)) (lambda ((y Int)) (primapp + x y))))\n")
(printf "\n")

; Ejemplo 4 del ejercicio 1
(printf "Ejemplo 4 del Ejercicio 1 \n")
(printf "Entrada: (uncurry (parser-L10 '(lambda ([x Int]) (lambda ([y Int]) (primapp + x y)))))\n")
(printf "Salida: ") (uncurry (parser-L10 '(lambda ([x Int]) (lambda ([y Int]) (primapp + x y)))))
(printf "Respuesta deseada: (language:L11 '(lambda ((x Int) (y Int)) (primapp + x y)))\n")
(printf "\n")

; Ejemplo 1 del ejercicio 2
(printf "Ejemplo 1 del Ejercicio 2 \n")
(printf "Entrada: (symbol-table-var (parser-L11 '(let ([ x Int (primapp + x x)]) (primapp * x x))))\n")
(printf "Salida: ") (symbol-table-var (parser-L11 '(let ([ x Int (primapp + x x)]) (primapp * x x))))
(printf "Respuesta deseada: '#hash((x . (Int . #<language:L11: (primapp + x x)>)))\n")
(printf "\n")

; Ejemplo 2 del ejercicio 2
(printf "Ejemplo 2 del Ejercicio 2 \n")
(printf "Entrada: (symbol-table-var (parser-L11 '(letrec ([x Int (const Int 2020)]) (primapp * x y))))\n")
(printf "Salida: ") (symbol-table-var (parser-L11 '(letrec ([x Int (const Int 2020)]) (primapp * x y))))
(printf "Respuesta deseada: '#hash((x . (Int . #<language:L11: (const Int 2020)>)))\n")
(printf "\n")

; Ejemplo 1 del ejercicio 3
(printf "Ejemplo 1 del Ejercicio 3 \n")
(printf "Entrada: (assigment (parser-L11 '(letrec ([foo (Int → Int) (lambda ([x Int]) x)]) (foo (const Int 5)))))\n")
(printf "Salida: ") (assigment (parser-L11 '(letrec ([foo (Int → Int) (lambda ([x Int]) x)]) (foo (const Int 5)))))
(printf "Respuesta deseada: (language:L12 '(letrec foo (foo (const Int 5))))
'#hash((foo . ((Int → Int) . #<language:L11: (lambda ((x Int)) x)>)))\n")
(printf "\n")

; Ejemplo 2 del ejercicio 3
(printf "Ejemplo 2 del Ejercicio 3 \n")
(printf "Entrada: (assigment (parser-L11 '(let ([x Int (primapp + x x)]) (primapp * x x))))\n")
(printf "Salida: ") (assigment (parser-L11 '(let ([x Int (primapp + x x)]) (primapp * x x))))
(printf "Respuesta deseada: (language:L12 '(let x (primapp * x x)))
'#hash((x . (Int . #<language:L11: (primapp + x x)>)))\n")
(printf "\n")

; Ejemplo 3 del ejercicio 3
(printf "Ejemplo 3 del Ejercicio 3 \n")
(printf "Entrada: (assigment (parser-L11 '(letrec ([ x Int (const Int 2020)]) (primapp * x y))))\n")
(printf "Salida: ") (assigment (parser-L11 '(letrec ([ x Int (const Int 2020)]) (primapp * x y))))
(printf "Respuesta deseada: (language:L12 '(letrec x (primapp * x y)))
'#hash((x . (Int . #<language:L11: (const Int 2020)>)))\n")
(printf "\n")