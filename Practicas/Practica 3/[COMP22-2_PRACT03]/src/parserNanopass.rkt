#lang nanopass

#|
   Compiladores 2022-2
   Practica 3

   Autores:
   Montiel Manriquez Ricardo
   Escamilla Soto Cristopher Alejandro
|#

(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         (prefix-in re- parser-tools/lex-sre)
         parser-tools/yacc)

#|Ejercicio 3|#

;; Definicion del lenguaje
(define-language LF
  (terminals
   (variable (x))
   (primitive (pr))
   (constant (c))
   (list (l))
   (string (s))
   (type (t)))
  (Expr (e body)
        x
        pr
        c
        l
        s
        t
        (begin e* ... e)
        (if e0 e1)
        (if e0 e1 e2)
        (fun ((x* t*) ...) t body* ... body)
        (let ((x* t* e*) ...) body* ... body)
        (funF x ((x* t*) ...) t body* ... body)
        (e0 e1 ...)
        (pr e* ... e)))

#|Ejercicio 4|#

;; Predicados para cada terminal
(define (variable? x)
  (symbol? x))

(define (type? t)
  (or (equal? t 'Int) (equal? t 'Bool)))

(define (constant? x)
  (or (number? x) (boolean? x)))

(define (primitive? prim)
  (memq prim '(+ - * / and or)))

;; Verifica que no sea palabra reservada
(define (res? r)
  (equal? r 'let))

(define-parser parse-LF LF)

;;Obtiene el ultimo caracter de una cadena
(define (last_char s)
  (substring s (- (string-length s) 1) (string-length s)))

;;Obtiene el ultimo caracter de un simbolo
(define (last_from_sym s)
  (last_char (symbol->string s)))

;; Obtiene el prefijo de una cadena
(define (pref s)
  (substring s 0 (- (string-length s) 1)))

;; Obtiene el ultimo prefijo de un simbolo
(define (sym_pref s)
  (pref (symbol->string s)))

;; Renombra un simbolo asignadole un nuevo indice
(define (newVar x)
  (cond
    [(primitive? x) x]
    [(res? x) x]
    [else (cond
            [(equal? (last_from_sym x) "0") (string->symbol (~a (sym_pref x) "1"))]
            [(equal? (last_from_sym x) "1") (string->symbol (~a (sym_pref x) "2"))]
            [(equal? (last_from_sym x) "2") (string->symbol (~a (sym_pref x) "3"))]
            [(equal? (last_from_sym x) "3") (string->symbol (~a (sym_pref x) "4"))]
            [(equal? (last_from_sym x) "4") (string->symbol (~a (sym_pref x) "5"))]
            [(equal? (last_from_sym x) "5") (string->symbol (~a (sym_pref x) "6"))]
            [(equal? (last_from_sym x) "6") (string->symbol (~a (sym_pref x) "7"))]
            [(equal? (last_from_sym x) "7") (string->symbol (~a (sym_pref x) "8"))]
            [(equal? (last_from_sym x) "8") (string->symbol (~a (sym_pref x) "9"))]
            [(equal? (last_from_sym x) "9") (string->symbol (~a (newVar (string->symbol (sym_pref x))) "0"))]
            [else (string->symbol (~a x "1"))]
    )]
  )
)

;; Preproceso que renombre las variables
(define-pass rename-var : LF (ir) -> LF ()
  (Expr : Expr (ir) -> Expr ()
        [,x (let* ([newX (newVar x)])
              newX)]
        [(fun ((,x* ,t*) ...) ,t ,[body*] ... ,[body])
         (let* ([newXs (map newVar x*)])
         `(fun ((,newXs ,t*) ...) ,t ,body* ... ,body))]
        [(let ((,x* ,t* ,[e*]) ...) ,[body*] ... ,[body])
         (let* ([newXs (map newVar x*)])
         `(let ((,newXs ,t* ,e*) ...) ,body* ... ,body))]
        [(funF ,x ((,x* ,t*) ...) ,t ,[body*] ... ,[body])
         (let* ([newX (newVar x)] [newXs (map newVar x*)])
         `(funF ,newX ((,newXs ,t*) ...) ,t (begin ,body* ... , body)))]
        )
  )

(printf "Ejercicio4: \n");

;;Ejemplo 1.
(printf "Ejemplo 1 \n");
(rename-var
 (parse-LF
  '(fun ((x1 Int) (x2 Int)) Int (if (and #t #f) (+ x1 x9 1)))
  )
 )

;;Ejemplo 2
(printf "Ejemplo 2 \n");
(rename-var
 (parse-LF
  '(funF sumitaIf ((x1 Int) (x2 Int)) Int
        (if (and #t #f) (+ x1 x9 1) (* x1 0)))
  )
)

;;Ejemplo 3
(printf "Ejemplo 3 \n");
(rename-var
 (parse-LF
  '(funF test ((x Int) (w Int)) Int (+ (funF add ((x Int) (z Int)) Int (+ x y z)) x))
  )
)

(printf "\n");

#|Ejercicio 5|#

; Lenguaje extendido sin IF
(define-language LFNotOneArmed
  (extends LF)
  (Expr (e body)
        (- (if e0 e1))))

; Parser
(define-parser parse-LFNotOneArmed LFNotOneArmed)

; Funcion que quita el IF
(define-pass remove-one-armed-if : LF (ir) -> LFNotOneArmed ()
  (Expr : Expr (ir) -> Expr ()
        [(if ,[e0] ,[e1]) `(if ,e0 ,e1 (void))]))

(printf "Ejercicio5: \n");

;Ejemplo 0

(printf "Ejemplo 0 \n");
(remove-one-armed-if
 (parse-LF
  '(if #t 19)
  )
 )

; Ejemplo 1
(printf "Ejemplo 1 \n");
(remove-one-armed-if
 (parse-LF
  '(if (and #t #f) (+ 42 0))
  )
 )

;; Ejemplo2
(printf "Ejemplo 2 \n");
(remove-one-armed-if
 (parse-LF
  '(funF fun1 ((x1 Int) (x2 Int)) Int (if (and #t #f) (+ x1 x9 1)))
  )
 )

(printf "\n");

#|Ejercicio 6|#

(define-language LNS
  (extends LFNotOneArmed)
  (terminals
   (- (string (s))))
  (Expr (e body)
         (- s)))

(define-parser parse-LNS LNS)

(define-pass remove-strings : LFNotOneArmed (ir) -> LNS ()
  (Expr : Expr (ir) -> Expr()
        [,s (string->list s)]))

;; Ejemplo 1
(printf "Ejercicio 6: \n");
(printf "Ejemplo 1  \n");
(parse-LFNotOneArmed '(+"caca" "huate"))
(remove-strings (parse-LFNotOneArmed '(+ "caca" "huate")))

(printf "Ejemplo 2  \n");
(parse-LFNotOneArmed '(list 1 2 3))

;(remove-strings (parse-LNS '(+ #\c #\h #\o #\l #\o)))