#lang nanopass
(require nanopass/base)

#|
   Compiladores 2022-2
   Practica 5

   Autores:
   Montiel Manriquez Ricardo
   Escamilla Soto Cristopher Alejandro
|#

;; Definicion del lenguaje L8
(define-language L8
  (terminals
   (variable (x))
   (primitive (pr))
   (constant (c))
   (type (t)))
  (Expr (e body)
        x
        (quot c)
        (begin e* ... e)
        (primapp pr e* ...)
        (if e0 e1 e2)
        (lambda ([x* t*] ...) body* ... body)
        (let ([x t e]) body* ... body)
        (letrec ([x t e]) body* ... body)
        (letfun ([x t e]) body* ... body)
        (letrec ([x t e]) body* ... body)
        (list e* ...)
        (e0 e1 ...)))

;; Predicados para cada terminal

;; Predicado para variables
(define (variable? x)
  (symbol? x))

; Definimos nuestras tipos
; Agregamos String a los tipos pues consideramos que tambien forma parte
(define (type? x) (or (oldt? x) (newt? x)
                      (equal? x 'Int) (equal? x 'Bool) (eq? x 'String) (eq? x 'Char) (eq? x 'Lambda) (eq? x 'List)
                      (and (list? x) (equal? (first x) 'List) (equal? (cadr x) 'of) (type? (caddr x)) )
                      (and (list? x) (type? (first x))  (equal? (cadr x) '-> ) (type? (caddr x)))
                   ))

(define (oldt? x) (memq x '(Bool Char Int List Lambda)))

(define (newt? x)
  (if (list? x)
      (let* ([a (first x)]
             [b (second x)]
             [c (third x)])
        (or (and (equal? a 'List) (equal? b 'of) (type? c))
            (and (type? a) (equal? b '→) (type? c))))
      #f))

; Definimos nuestras costantes:
; Integer, Boolean, Char
(define (constant? c) (or (number? c) (boolean? c) (char? c)))

; Definimos los datos primitivos
; + - * / and or not lenght car first cdr
(define (primitive? x) (or (equal? x '+) (equal? x '-) (equal? x '*) (equal? x '/)
                           (equal? x 'and) (equal? x 'or) (equal? x 'not) (equal? x 'length)
                           (equal? x 'first) (equal? x 'cdr)))

;; Definimos el parser de L8
(define-parser parse-L8 L8)

;; EJERCICIO 1
; Definimos un nuevo lenguaje en el que se modificaran las definiciones de lambda
(define-language L9
  (extends L8)
  (Expr (e body)
        (- (lambda ([x* t*] ...) body* ... body)
           (e0 e1 ...))
        (+ (lambda ([x t]) body* ... body)
           (e0 e1))))

; Creamos el parser del nuevo lenguaje L9
(define-parser parse-L9 L9)

; Ahora si creamos la funcion que hara el proceso de curry-let
; Este proceso se encarga de currificar las expresiones lambda asi
; como las aplicaciones de funcion.
(define-pass curry : L8 (ir) -> L9 ()
  (Expr : Expr (e) -> Expr ()
        [(lambda ([,x*,t*] ...) ,[body*] ... ,[body])
         (let f ([bindingx* x*]
                 [bindingt* t*])
           (if (equal? (length bindingx*) 1)
               `(lambda ([, (first bindingx*) , (first bindingt*)]) ,body* ... ,body)
               `(lambda ([, (first bindingx*), (first bindingt*)]) ,(f (cdr bindingx*) (cdr bindingt*)))
            ))
         ]
        [(,[e0] ,[e1] ...)
         (let f ([binding0 e0]
                 [binding1 e1])
           (if (equal? (length binding1) 0)
               `,binding0
               (f `(,binding0 ,(first binding1)) (cdr binding1))
            ))
         ]))

; EJERCICIO 2
; Definimos un nuevo lenguaje en el que se modificaran las definiciones de quot
(define-language L10
  (extends L9)
  (Expr (e body)
        (- (quot c))
        (+ (const t c))))

; Definimos el parser del nuevo lenguaje L10
(define-parser parser-L10 L10)


; Funcion que nos regresa el tipo de una constante
(define (types? t)
  (cond
    [(equal? (constant? t) (number? t)) 'Int]
    [(equal? (constant? t) (boolean? t)) 'Bool]
    [(equal? t 'Char) 'Char]
   ))

    
; Creamos la funcion que hará el proceso de type-const
(define-pass type-const : L9 (ir) -> L10()
  (Expr : Expr (e) -> Expr ()
        [(quot ,c)
         (let f ([types* (types? c)])
           `(const ,types* ,c ))
         ]
   ))

;; EJERCICIO 3

; La funcion (unify t1 t2) se reduce a verificar si t1 es unificable
; con t2 sin regresar el unificador.
; ufify: t1 t2 -> boolean  | Donnde t1 y t2 son del mismo tipo
(define (unify t1 t2)
    (if (and (type? t1) (type? t2))
        (cond
            [(equal? t1 t2) #t]
            [(and (equal? 'List t1) (list? t2))  (equal? (car t2) 'List)]
            [(and (equal? 'List t2) (list? t1))  (equal? (car t1) 'List)]
            [(and (list? t1) (list? t2)) (and (unify (car t1) (car t2)))]
            [else #f])
        (error "Se esperaban 2 tipos")
     ))

; Esta funcion recibe una expresion del lenguaje y un contexto inicial, y regresa el tipo
; correspondiente a la expresion.

; J: (type L10) ambiente -> boolean  | Donnde t1 y t2 son del mismo tipo
(define (J e env)
    (nanopass-case (L10 Expr) e
                   
        ; variables utilizamos la funcion auxiluar
        ; para buscar directamente en el ambiente                   
        [,x  (search-type x env)]
        
        ; Cuando tenemos constantes pasamos el tipo
        ; que esta contenido en  el const        
        [(const ,t ,c ) t]
        
        ; Necesitamos encontrar la ultima exprexion
        ; y obtener su tipo        
        [(begin ,e* ... ,e) (J e env)]   

        [(primapp ,pr ,e* ...)
            (if (check-types pr  (map (lambda (x) (J x env)) e*) )
                (case pr
                    [(+ - * / length) 'Int]
                    [(first) (caddr (first  (map (lambda (x) (J x env)) e*)))]
                    [(cdr) (first  (map (lambda (x) (J x env)) e*) )])
                (error 'J "Existe un error de tipos dentro de una operacion"))]

        ; En el caso del if conocemos su estructura y por lo tanto debemos verificar que:
        ; La primera expresion(e0) sea de tipo Bool
        ; Las expresiones then(e1) else(e2) sean del mismo tipo      
        [(if ,e0 ,e1 ,e2)
            (if (and (equal?  (J e0 env) 'Bool)  (unify (J e1 env) (J e2 env)))
                (J e1 env)
                (error 'J "Existe un error en la estructuda del IF verifica que los tipos sean los correctos") )]

        ; Cuando tenemos lambdas el tipo sera definido por el cuerpo        
        [(lambda ([,x ,t]) ,body)  (list t '->  (J body (add-env x t env)))]

        ; Cuando tenemos let nuevamente revisamos el body y añadimos (x t) al ambiente
        [(let ([,x ,t ,e]) ,body)
            (if  (unify t (J e env))
                (J body (add-env x t env))
                (error 'J "El tipo no coincide con el de la expresion") )]

        ; Cuando tenemos letc nuevamente revisamos el body y añadimos (x t) al ambiente
        [(letrec ([,x ,t ,e]) ,body)
            (if  (unify t (J e (add-env x t env)))
                (J body (add-env x t env))
                (error 'J "El tipo no coincide con el de la expresion") )]

        ;; Cuando tenemos letfun nuevamente revisamos el body y añadimos (x t) al ambiente
        [(letfun ([,x ,t ,e]) ,body)
            (if  (and (equal? '-> (cadr t)) (unify t (J e env)) )
                (J body (add-env x t env))
                (error 'J "El tipo no coincide con el de la expresion") )]

        ; Cuando tenemos list
        ; Si es vacia devoldemos List
        ; Si no, regresamos los tipos de los elementos
        ; Si todos son del mismo tipo unificamos y devolvemos el tipo
        ; de lo contrario un error
        [(list ,e* ...)
            
            (if (empty? e*)
                'List
                (let* ([types (map (lambda (x) (J x env)) e*) ]
                        [t1 (first types)])
                    ; Utilizamos and map para verificar que todos
                    ; sean del mismo tipo
                    (if (andmap (lambda (x) (unify x t1)) types)
                        (list 'List 'of t1)
                        (error 'J "Los elementos no son todos de un mismo tipo")))  )]

        ; e0 deber ser T1->T2 y e1 ser T1
        [(,e0 ,e1)
            (let ([t0 (J e0 env)] [t1 (J e1 env)])
                (if (and (list? t0) (equal? (cadr t0) '->) (unify (car t0) t1))  
                    (caddr t0)                                                   
                    (error 'J "Los tipo no son compatibles para poder aplicar la funcion") )  )]
  ))

;; EJERCICIO 4

; Se encarga de quitar la anotacion de tipo Lambda y
; sustituirlas por el tipo ’(T → T) que corresponda a la definici ́on de la funcion. Y sustituye las
; anotaciones de tipo List por el tipo (List of T) de ser necesario.

; type-infer:: L10 -> L10

; Para let solo nos fijamos en el tipo de de t si es un List, lo denombra a List of
; Para let solo nos fijamos en el tipo de de t si es List o Lambda, colocamos T->T y List of.
; Para letfun siempre nos fijamos en el tipo de t, aplicamos ya sea lambda o list
(define-pass type-infer : L10(ir) -> L10()
    (Expr : Expr (ir) -> Expr ()
        
        [(let ([,x ,t ,[e]]) ,[body])
            (case t
                [(List) `(let ([,x ,(J e '()) ,e]) ,body) ]
                [else   `(let ([,x ,t ,e]) ,body) ])]
        [(letrec ([,x ,t ,[e]]) ,[body])
            (let ( [fixed-type (case t [(List Lambda) (J e '())] [else t]) ])
                `(letrec ([,x ,fixed-type ,e]) ,body)
            )]
        
        [(letfun ([,x ,t ,[e]]) ,[body])
            `(letfun ([,x ,(J e '()) ,e]) ,body)]
     ))

;; Funciones Auxiliares

; Funcion para buscar una variable dentro de un ambiente
; search-type:: x env -> boolean
(define (search-type var env)
    (cond
        [(empty? env) (error "Variable no encontrada en el ambiente")]
        [(eq? (caar env) var) (cadar env)] 
        [else (search-type var (cdr env))]
     ))  


; Valida tipo para cada operacion
; ::check-types:: pr arg -> boolean | pr  = operador
;                                     arg = argumentos
(define (check-types pr args )
    (case pr
        [(+ - * /) (andmap (lambda (x) (equal? x 'Int)) args) ] 
        [(car cdr length) (andmap (lambda (x) (and (list? x) (equal? (car x) 'List))) args)]
     ))

; Funcion que agrega la tupla (x,t) al ambiente
; add-env:: x t env -> list | x = variable
;                             t = tipo
;                             env = ambiente
(define (add-env x t env)
    (list (list x t) env))

;; EJEMPLOS

; Ejemplo 1 del ejercicio 1
(printf "Ejemplo 1 del Ejercicio 1 \n")
(printf "Entrada: (curry '(foo x y))\n")
(printf "Salida: ") (curry (parse-L8 '(foo x y)))
(printf "Respuesta deseada: '(( foo x ) y)\n")
(printf "\n")

; Ejemplo 2 del ejercicio 1
(printf "Ejemplo 2 del Ejercicio 1 \n")
(printf "Entrada: (curry '(lambda ([x Int] [y Int]) (+ x y)))\n")
(printf "Salida: ") (curry (parse-L8 '(lambda ([x Int] [y Int]) (primapp + x y))))
(printf "Respuesta deseada: '(lambda ([x Int]) (lambda ([y Int]) (+ x y)))\n")
(printf "\n")

; Ejemplo 1 del ejercicio 2
(printf "Ejemplo 1 del Ejercicio 2 \n")
(printf "Entrada: (type-const '(quot 5))\n")
(printf "Salida: ") (type-const (parse-L9 '(quot 5)))
(printf "Respuesta deseada: '(const Int 5)\n")
(printf "\n")

; Ejemplo 1 del ejercicio 3
(printf "Ejemplo 1 del Ejercicio 3 \n")
(printf "Entrada: (J (parser-L10 '(lambda ([x Int]) x)) '())\n")
(printf "Salida: ") (J (parser-L10 '(lambda ([x Int]) x)) '())
(printf "Respuesta deseada: '(Int → Int)\n")
(printf "\n")

; Ejemplo 1 del ejercicio 4
(printf "Ejemplo 1 del Ejercicio 4 \n")
(printf "Entrada: (J (parser-L10 '(lambda ([x Int]) x)) '())\n")
(printf "Salida: ") (type-infer (parser-L10 '(letrec ([foo Lambda (lambda ([x Int]) x)]) (foo (const Int 5)))))
(printf "Respuesta deseada: '(letrec ([foo (Int → Int) (lambda ([x Int]) x)]) (foo 5))\n")
(printf "\n")

; Ejemplo 2 del ejercicio 4
(printf "Ejemplo 2 del Ejercicio 4 \n")
(printf "Entrada: (type-infer (let ([x List (list)]) x))\n")
(printf "Salida: ") (type-infer (parser-L10 '(let ([x List (list)]) x)))
(printf "Respuesta deseada: '(let ([x List (list)]) x)\n")
(printf "\n")

; Ejemplo 3 del ejercicio 4
(printf "Ejemplo 3 del Ejercicio 4 \n")
(printf "Entrada: (type-infer (let ([x List (list 1 2 3 4)]) x))\n")
(printf "Salida: ") (type-infer (parser-L10 '(let ([x List (list (const Int 1) (const Int 2) (const Int 3) (const Int 4))]) x)))
(printf "Respuesta deseada: '(let ([x (List of Int) (list 1 2 3 4)]) x)\n")
(printf "\n")