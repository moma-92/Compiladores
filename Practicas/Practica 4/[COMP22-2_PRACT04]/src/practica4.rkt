#lang nanopass
(require nanopass/base)

#|
   Compiladores 2022-2
   Practica 4

   Autores:
   Montiel Manriquez Ricardo
   Escamilla Soto Cristopher Alejandro
|#

;; Definicion del lenguaje LF
(define-language LF
   (terminals
   (variable (x))
   (primitive (pr))
   (constant (c))
   (list (l))
   (type (t)))
   (Expr (e body)
      x
      pr
      c
      l
      t
      (primapp pr e* ...)
      (begin e* ... e)
      (if e0 e1)
      (if e0 e1 e2)
      (lambda ([x* t*] ...) body* ... body)
      (let ([x* t* e*] ...) body* ... body)
      (letrec ([x* t* e*] ...) body* ... body)
      (list e e*)
      (e0 e1 ...)))

;; Predicados para cada terminal
(define (variable? x)
  (symbol? x))

(define (type? t)
  (or (equal? t 'Bool) (equal? t 'Int) (eq? t 'Char) (list? t) (eq? t 'Lambda) (equal? t 'String)))

(define (constant? x)
  (or (number? x) (boolean? x) (eq? x 'Char)))

(define (primitive? x)
  (or (procedure? x) (memq x '(+ * - / car or cdr and or length))))

; Parser de LF
(define-parser parse-LF LF)

;; Definicion del lenguaje L7
;#|
(define-language L7
  (extends LF)
  (Expr (e body)
        (- (let ([x* t* e*] ...) body* ... body)
           (letrec ([x* t* e*] ...) body* ... body))
        (+ (let ([x t e]) body* ... body)
           (letrec ([x t e]) body* ... body))))


; Parser de L7
(define-parser parse-L7 L7)
;|#

;; Ejercicio 1

(define-pass curry-let : LF (ir) -> L7 ()
  (Expr : Expr (ir) -> Expr ()
        [(let ([,x* ,t* , e*] ...) ,[body])
         (let f ([x* x*]
                 [t* t*]
                 [e* e*])
           (if (or (null? (cdr x*)) (null? (cdr t*)) (null? (cdr e*)))
                `(let ([,(car x*) ,(car t*) ,(car e*)]) ,body)
                `(let ([,(car x*) ,(car t*) ,(car e*)]) ,(f(cdr x*) (cdr t*) (cdr e*)))))]
        [(letrec ([,x* ,t* ,[e*]] ...) ,[body])
         (let f ([x* x*]
                 [t* t*]
                 [e* e*])
           (if (or (null? (cdr x*)) (null? (cdr t*)) (null? (cdr e*)))
                `(let ([,(car x*) ,(car t*) ,(car e*)]) ,body)
                `(let ([,(car x*) ,(car t*) ,(car e*)]) ,(f(cdr x*) (cdr t*) (cdr e*)))))]
        ))

; Ejemplo 1 del ejercicio 1
(printf "Ejemplo 1 del Ejercicio 1 \n")
(printf "Entrada: '(let ([x Int 4] [y Int 6]) (+ x y ))\n")
(printf "Salida: ") (curry-let (parse-LF '(let ([x Int 4] [y Int 6]) (+ x y))))
(printf "Respuesta deseada: '(let ([x Int 4]) (let ([y Int 6]) (+ x y )))\n")
(printf "\n")

;; Ejercicio 2

(define-pass identify-assigments : L7 (ir) -> L7 ()
  (Expr : Expr (ir) -> Expr ()
        [(let ([,x ,t ,e]) ,[body*] ... ,[body])
         (if (equal? t 'Lambda)
             `(letrec ([,x,t,e]) ,body* ... ,body)
             `(let ([,x,t,e]) ,body* ... body))]))

; Ejemplo 1 del ejercicio 2
(printf "Ejemplo 1 del Ejercicio 2 \n")
(printf "Entrada: '(let ([foo Lambda (lambda ([x Int]) x)]) (foo 5))\n")
(printf "Salida: ") (identify-assigments (parse-L7 '(let ([foo Lambda (lambda ([x Int]) x) ]) (foo 5))))
(printf "Respuesta deseada: '(letrec ([foo Lambda (lambda ([x Int]) x)]) (foo 5))\n")
(printf "\n")

;; Ejercicio 3

(define-language L8
  (extends L7)
  (Expr (e body)
        (+ (letfun ([x t e]) body* ... body))))

; Parser de L8
(define-parser parse-L8 L8)

; Creamos el proceso un-anonymous
(define-pass un-anonymous : L7 (e) -> L8 ()
  (Expr : Expr (e) -> Expr ()
        [(lambda ([,x*, t*] ...), [body*] ... ,[body])
          ` (letfun (,'foo Lambda (lambda ([,x* ,t*] ...) ,body* ... body)), 'foo)]))

; Ejemplo 1 del ejercicio 3
(printf "Ejemplo 1 del Ejercicio 3 \n")
(printf "Entrada: '(lambda ([x Bool]) (if x 1 2))\n")
(printf "Salida: ") (un-anonymous (parse-L7 '(lambda ([x Bool]) (if x 1 2) )))
(printf "Respuesta deseada: '(letfun ([foo Lambda (lambda ([x Bool]) (if x 1 2))]) foo)\n")
(printf "\n")


; Ejercicio 4
;Este proceso funciona como verificador de la sintaxis de las expresiones y
;consiste en verificar que el número de parámetros corresponde con la aridad de las primitivas.
;Si corresponde, se regresa la misma expresión en caso contrario se lanza un error, especificando
;que la expresión está mal construida.

;; Predicado auxiliar airidad? regresa si la aridad del operador es correcta concorde al operador.
(define (aridad? pr ar)
   (match pr
      ["+" (> ar 1)]
      ["-" (> ar 1)]
      ["*" (> ar 1)]
      ["/" (> ar 1)]
      ["length" (eq? 1 ar)]
      ["car" (eq? 1 ar)]
      ["cdr" (eq? 1 ar)]
     ;;Los agregamos pues concideramos que tambien forman parte de los operadores basicos de un
     ;;lenguaje
      ["and" (< 1 ar)]
      ["not" (eq? 1 ar)]
      ["or"  (< 1 ar)]
      ["<" (eq? ar 2)]
      [">" (eq? ar 2)]
      ["equal?" (eq? ar 2)]
      ["++" (eq? 1 ar)]
      ["--" (eq? 1 ar)]))

; Función que verifica si la cantidad de arguementos en funcion de las primitivas, es correcto.

(define-pass verify-arity : L8 (ir) -> L8()
   (Expr : Expr (ir) -> Expr ()
      [(primapp  ,pr, [e*] ...)
       ;;Utilizamos nuestra funcion auxiliar para determinar si nuestro operador contiene los
       ;;argumentos correctos
         (if (aridad? (symbol->string pr) (length e*))
            `(primapp ,pr, e* ...)
            (error (string-append "Error de Aridad " (symbol->string pr)", Recibio como argumentos (" (~v (length e*))")")))]))

; Ejemplo 1 del ejercicio 4
(printf "Ejemplo 1 del Ejercicio 4 \n")
(printf "Entrada: '(+ 2 3)\n")
(printf "Salida: ") (verify-arity (parse-L8 '(+ 2 3)))
(printf "Respuesta deseada: '(+ 2 3)\n")
(printf "\n")

; Ejemplo 2 del ejercicio 4
(printf "Ejemplo 2 del Ejercicio 4 \n")
(printf "Entrada: '( car 2 3)\n")
(printf "Salida: ") ;(verify-arity (parse-L8 '(primapp car 2 3)))
(printf "Respuesta deseada: error : Arity mismatch\n")
(printf "\n")


; Ejercicio 5
;Este proceso funciona como verificador de la sintaxis de las expresiones y
;consiste en verificar que la expresión no tenga variables libres, de existir variables libres se
;regresa un error en caso  contrario la salida es la misma expresión.

(define-pass verify-vars : L8 (ir) -> L8 ()
  (Expr : Expr (ir [env null]) -> Expr ()
        [,x
         ;;Nos apoyamos en la funcion memq para buscar una variable dentro de un ambiente
         ;;que esta representado por una lista, de esta forma sabremos si es libre o no
         (if (memq x env)
             x
             (error (string-append "Variable libre:" (symbol->string x))))]
        [(let ([,x ,t ,[e]]) ,[Expr : body (cons x env) -> body]) `(let ([,x ,t ,e]) ,body)]
        [(letfun ([,x ,t ,e]) ,[Expr : body (cons x env) -> body]) `(letfun ([,x ,t]) ,body)]
        [(letrec ([,x ,t ,[Expr : e (cons x env) -> e]]) , [Expr : body (cons x env) -> body]) `(letrec ([,x ,t ,e]) ,body)]
        [(lambda ([,x*, t*] ...) ,[Expr : body (append x* env) -> body])`(lambda ([,x* ,t*] ...) ,body)]))

; Ejemplo 1 del ejercicio 5
(printf "Ejemplo 1 del Ejercicio 5 \n")
(printf "Entrada: '(+ 2 x )\n")
(printf "Salida: ") ;(verify-vars (parse-L8 '(primapp + 2 x)))
(printf "Respuesta deseada: error : Free variable x\n")
(printf "\n")