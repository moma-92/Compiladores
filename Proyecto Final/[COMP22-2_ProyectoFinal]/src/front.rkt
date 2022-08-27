#lang nanopass

#| Compiladores
   Proyecto Final

   Escamilla Soto Cristopher Alejandro 314309253
   Montiel Manriquez Ricardo 314332662
   Villegas Salvador Kevin Ricardo - 314173739 |#

(provide (all-defined-out))

;; Definición del Lenguaje LF
(define-language LF
  (terminals
   (variable (x))
   (primitive (pr))
   (constant (c))
   (type (t))
   (list (l))
   (char (ch))
   (string (s)))
  (Expr (e body)
        x
        pr
        c
        t
        l
        ch
        s
        (quot c)
        (primapp pr e* ...)
        (begin e* ... e)
        (if e0 e1)
        (if e0 e1 e2)
        (lambda ([x* t*] ...) body* ... body)
        (let ([x* t* e*] ...) body* ... body)
        (letrec ([x* t* e*] ...) body* ... body)
        (list e* ...)
        (e0 e1 ...)
        ;; Ejercicio 1 | Extención con define
        (define x e)
        ;; Ejercicio 2 | Extención con while
        (while [e0] e1)
        ;; Ejercicio 3 | Extención con for
        (for [x e0] e1)))

;; Predicados

;; Variables
(define (variable? x)
  (symbol? x))

;; Tipos
(define (type? x)
  (or (oldt? x)
      (newt? x)))

(define (oldt? x)
  (memq x '(Bool Char Int List Lambda)))

(define (newt? x)
  (if (list? x)
      (let* ([a (first x)]
             [b (second x)]
             [c (third x)])
        (or (and (equal? a 'List) (equal? b 'of) (type? c))
            (and (type? a) (equal? b '→) (type? c))))
      #f))

;; Constantes
(define (constant? x)
  (or (boolean? x) (number? x) (char? x)))

;; Operadores primitivos
(define (primitive? x)
  (or (equal? x '+) (equal? x '-) (equal? x '*) (equal? x '/)
      (equal? x 'or) (equal? x 'and) (equal? x 'not)
      (equal? x 'length) (equal? x 'car) (equal? x 'cdr)
      (equal? x '<) (equal? x '>) (equal? x 'equal?) (equal? x 'iszero?)
      (equal? x '++) (equal? x '--)))

;; Parser para el lenguaje LF
(define-parser parser-LF LF)

;--------------------- Proceso 1 | remove-one-armed-if ---------------------

;; Extensión de lenguaje, eliminando expresión con if de una rama.
(define-language L1
  (extends LF)
  (Expr (e body)
        (- (if e0 e1))))

;; Parser
(define-parser parser-L1 L1)

;; Quita el IF
(define-pass remove-one-armed-if : LF (e) -> L1 ()
  (Expr : Expr (e) -> Expr ()
        [(if ,[e0] ,[e1]) `(if ,e0 ,e1 (void))]))

;--------------------- Proceso 2 | remove-string ---------------------

;; Extiende L1 con strings y listas
(define-language L2
  (extends L1)
  (terminals
   (- (string (s))))
  (Expr (e body)
        (- s)))

;; Parser para el lenguage LNS
(define-parser parser-L2 L2)

;; Definimos el proceso del compilador para eliminar strings
(define-pass remove-string : L1 (e) -> L2 ()
  (Expr : Expr (e) -> Expr ()
        ;[(list ,[e*] ... ,[e]) '(list ,e* ... ,e)]
        [,s `(list ,(string->list s) ...)]
        #|[,t ',(swap-typies t)]
        [(let ((,x* ,t*, e*) ...) ,body* ... ,body)
         '(let ((,x* ,(map swap-typies t*) ,e*) ...)
            ,(map remove-strings body*) ... ,(remove-strings body))]|#))

;--------------------- Proceso 3 | curry-let ---------------------

;; Lenguaje L3
(define-language L3
  (extends L2)
  (Expr (e body)
        (- (let ([x* t* e*] ...) body* ... body)
           (letrec ([x* t* e*] ...) body* ... body))
        (+ (let ([x* t* e*]) body* ... body)
           (letrec ([x* t* e*]) body* ... body))))

;; Parser para el lenguaje L3
(define-parser parser-L3 L3)

;; Currifica las expresiones let y letrec
;; curry-let ; LF (e) -> L7 (e)
(define-pass curry-let : L2 (e) -> L3 ()
  (Expr : Expr (e) -> Expr ()
        [(let ((,x* ,t* ,[e*]) ...) ,[body*] ... ,[body])
         (let params ([var* x*]
                      [type* t*]
                      [expr* e*])
           (if (equal? (length var*) 1)
               `(let ([,(car var*) ,(car type*) ,(car expr*)]) ,body* ... ,body)
               `(let ((,(car var*) ,(car type*) ,(car expr*)))
                  ,(params (cdr var*) (cdr type*) (cdr expr*)))))]
        [(letrec ((,x* ,t* ,[e*]) ...) ,[body*] ... ,[body])
         (let params ([var* x*]
                      [type* t*]
                      [expr* e*])
           (if (equal? (length var*) 1)
               `(let ([,(car var*) ,(car type*) ,(car expr*)]) ,body* ... ,body)
               `(let ((,(car var*) ,(car type*) ,(car expr*)))
                  ,(params (cdr var*) (cdr type*) (cdr expr*)))))]))

;--------------------- Proceso 4 | identify-assigments ---------------------

(define-pass identify-assigments : L3 (e) -> L3 ()
  (Expr : Expr (e) -> Expr ()
        [(let ([,x* ,t* ,[e*]]) ,[body*] ... ,[body])
         (if (equal? t* 'Lambda)
             `(letrec ([,x* ,t* ,e*]) ,body* ... ,body)
             `(let ([,x* ,t* ,e*]) ,body* ... ,body))]))

;--------------------- Proceso 5 | un-anonymous ---------------------

;; Lenguaje L4
(define-language L4
  (extends L3)
  (Expr (e body)
        (+ (letfun ([x* t* e*]) body* ... body))))

;; Parser para el lenguaje L8
(define-parser parser-L4 L4)

;; Asigna identificador a las funciones lambda
;; un-anonymous : L3 (e) -> L4 (e)
(define-pass un-anonymous : L3 (e) -> L4 ()
  (Expr : Expr (e) -> Expr ()
        [(lambda ((,x* ,t*) ...) ,[body*] ... ,[body])
         `(letfun ([foo Lambda (lambda ((,x* ,t*) ...) ,body* ... ,body)]) foo)]))

;--------------------- Proceso 6 | verify-arity ---------------------

;; Verifica la sintaxis de las expresiones primitivas
;; verify-arity : L4 (e) -> L4 (e)
(define-pass verify-arity : L4 (e) -> L4 ()
  (Expr : Expr (e) -> Expr ()
        [(primapp ,pr ,[e*] ...)
         (let ([len (length e*)]
               [elem (λ (pri l)
                       (list? (member pri l)))])
           (if (elem pr '(+ * and or))
               e
               (if (elem pr '(equal?))
                   (if (equal? len 2)
                       e
                       (error "error : Arity mismatch"))
                   (if (elem pr '(not length car cdr iszero? ++ --))
                       (if (equal? len 1)
                           e
                           (error "error : Arity mismatch"))
                       (if (elem pr '(- / < >))
                           (if (>= len 1)
                               e
                               (error "error : Arity mismatch"))
                           (error "error : Arity mismatch"))))))]))

;--------------------- Proceso 7 | verify-vars ---------------------

;; Verifica que no haya variables libres en las expresiones
;; verify-vars : L8 (e) -> L8 (e)
#|(define-pass verify-vars : L4 (e) -> L4 ()
  (Expr : Expr (e) -> Expr ()
        [,x (free-vars e '())]
        [(primapp ,pr ,[e*] ...) (free-vars e '())]
        [(begin ,[e*] ... ,[e]) (free-vars e '())]
        ;[(if ,[e0] ,[e1]) (free-vars e '())]
        ;[(if ,[e0] ,[e1] ,[e2]) (free-vars e '())]
        [(lambda ([,x* ,t*] ...) ,body* ... ,body) (free-vars e '())]
        [(let ([,x* ,t* ,[e*]]) ,body* ... ,body) (free-vars e '())]
        [(letrec ([,x* ,t* ,[e*]]) ,body* ... ,body) (free-vars e '())]
        [(letfun ([,x* ,t* ,[e*]]) ,body* ... ,body) (free-vars e '())]
        [(list ,[e*] ... ,[e]) (free-vars e '())]
        [(,[e0] ,[e1] ...) (free-vars e '())]))|#

(define-pass verify-vars : L4 (ir) -> L4 ()
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
        [(lambda ([,x*, t*] ...) ,[Expr : body (append x* env) -> body])`(lambda ([,x* ,t*] ...) ,body)]
        [(for [,x ,e0*] ,[Expr : e (cons x env) -> e1]) `(for [,x ,e0*] ,e1)]))

;; Función auxiliar que nos ayuda a verificar si una expresiones tiene variables libres
;; free-vars : L4 (e) -> listof var -> L4 (e)
(define (free-vars e l)
  (nanopass-case (L4 Expr) e
                 [,c c]
                 [,x (if (member x l)
                         x
                         (error (string-append "error : Free variable " (format "~a" `,x))))]
                 [(primapp ,pr ,[e*] ...)
                  (let ([e* (map (λ (x) (free-vars x l)) e*)])
                    (with-output-language (L4 Expr) `(primapp ,pr ,e* ...)))]
                 [(begin ,[e*] ... ,[e])
                  (let ([e* (map (λ (x) (free-vars x l)) e*)]
                        [e (free-vars e l)])
                    (with-output-language (L4 Expr) `(begin ,e* ... ,e)))]
                 #|[(if ,[e0] ,[e1])
                  (let ([e0 (free-vars e0 l)]
                        [e1 (free-vars e1 l)])
                    (with-output-language (L4 Expr) `(if ,e0 ,e1)))]
                 [(if ,[e0] ,[e1] ,[e2])
                  (let ([e0 (free-vars e0 l)]
                        [e1 (free-vars e1 l)]
                        [e2 (free-vars e2 l)])
                    (with-output-language (L4 Expr) `(if ,e0 ,e1 ,e2)))]|#
                 [(lambda ([,x* ,t*] ...) ,body* ... ,body)
                  (let ([body* (map (λ (x) (free-vars x (append l (list x*)))) body*)]
                        [body (free-vars body (append l (list x*)))])
                    (with-output-language (L4 Expr) `(lambda ([,x* ,t*] ...) ,body* ... ,body)))]
                 [(let ([,x* ,t* ,[e*]]) ,body* ... ,body)
                  (let ([e* (free-vars e* l)]
                        [body* (map (λ (x) (free-vars x (append l (list x*)))) body*)]
                        [body (free-vars body (append l (list x*)))])
                    (with-output-language (L4 Expr) `(let ([,x* ,t* ,e*] ,body* ... ,body))))]
                 [(letrec ([,x* ,t* ,[e*]]) ,body* ... ,body)
                  (let ([e* (free-vars e* l)]
                        [body* (map (λ (x) (free-vars x (append l (list x*)))) body*)]
                        [body (free-vars body (append l (list x*)))])
                    (with-output-language (L4 Expr) `(letrec ([,x* ,t* ,e*] ,body* ... ,body))))]
                 [(letfun ([,x* ,t* ,[e*]]) ,body* ... ,body)
                  (let ([e* (free-vars e* l)]
                        [body* (map (λ (x) (free-vars x (append l (list x*)))) body*)]
                        [body (free-vars body (append l (list x*)))])
                    (with-output-language (L4 Expr) `(letfun ([,x* ,t* ,e*] ,body* ... ,body))))]
                 [(list ,[e*] ... ,[e])
                  (let ([e* (map (λ (x) (free-vars x l)) e*)]
                        [e (free-vars e l)])
                    (with-output-language (L4 Expr) `(list ,e* ... ,e)))]
                 [(,[e0] ,[e1] ...)
                  (let ([e1 (map (λ (x) (free-vars x l)) e1)]
                        [e0 (free-vars e0 l)])
                    (with-output-language (L4 Expr) `(,e0 ,e1 ...)))]))

;--------------------- Proceso 8 | curry ---------------------

;; Lenguaje L5
(define-language L5
  (extends L4)
  (Expr (e body)
        (- (lambda ([x* t*] ...) body* ... body)
           (e0 e1 ...))
        (+ (lambda ([x t]) body* ... body)
           (e0 e1))))

;; Parser para el lenguaje L5
(define-parser parser-L5 L5)

;; Currifica las expresiones lambda así como las aplicaciones de función
;; curry : L4 (e) -> L5 (e)
(define-pass curry : L4 (e) -> L5 ()
  (Expr : Expr (e) -> Expr ()
        [(lambda ([,x* ,t*] ...) ,[body])
         (let params ([var* x*]
                      [type* t*])
           (if (equal? (length var*) 1)
               `(lambda ([,(car var*) ,(car type*)]) ,body)
               `(lambda ([,(car var*) ,(car type*)])
                  ,(params (cdr var*) (cdr type*)))))]
        [(,[e0] ,[e1] ...)
         (let params ([fun e0]
                      [arg e1])
           (if (equal? (length arg) 0)
               `,fun
               (params `(,fun ,(car arg)) (cdr arg))))]))

;--------------------- Front-end ---------------------

(define (front expr)
  (curry
   (verify-vars
    (verify-arity
     (un-anonymous
      (identify-assigments
       (curry-let
        (remove-string
         (remove-one-armed-if expr)))))))))