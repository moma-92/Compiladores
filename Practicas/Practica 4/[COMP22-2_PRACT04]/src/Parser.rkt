#lang nanopass

#|
   Compiladores 2022-2
   Practica 3

   Autores:
   Montiel Manriquez Ricardo
   Escamilla Soto Cristopher Alejandro
|#

(require "Lexer.rkt"
         (prefix-in : parser-tools/lex-sre)
         (prefix-in re- parser-tools/lex-sre)
         parser-tools/yacc
         parser-tools/lex)

;;struct
(define-struct var-exp (a) #:transparent) ;Variables
(define-struct num-exp (a) #:transparent) ;Numeros
(define-struct bool-exp (a) #:transparent) ;booleanos

(define-struct typeof-exp (a b) #:transparent) ;Type-of operador ":"
(define-struct int-exp () #:transparent) ;Tipo Int
(define-struct boolean-exp () #:transparent) ;Tipo bool
(define-struct assign-exp (a b) #:transparent) ; Igual "="

;(define-struct func-exp (a b) #:transparent) ;Tipo Func
(define-struct prim-exp (a b c) #:transparent) ;Operaciones aritmeticas

(define-struct if-then-else-exp (a b c) #:transparent) ;if-then-else
(define-struct if-then-exp (a b) #:transparent) ;If-then-else
(define-struct fun-exp (a b) #:transparent) ;Tipo Fun
(define-struct fun-f-exp (a b c) #:transparent) ;Funciones
(define-struct app-exp (a b) #:transparent) ;app
(define-struct app-t-exp (a b) #:transparent) ;appT
(define-struct let-exp (a b) #:transparent) ;Expresiones Let
(define-struct begin-exp (a) #:transparent) ;Begin

(define-struct par-exp (a) #:transparent) ;Parentesis ()
(define-struct key-exp (a) #:transparent) ;llaves {}
(define-struct brack-exp (a) #:transparent) ;corchetes []

;; minHs-parser

(define minHS-parser
  (parser
   (start exp) ;Lee el simbolo inicial de la expresion
   (end EOF) ;Se termina de leer el ultimo simbolo de la expresion
   (error exp) ;Cachamos los errores que puedan surgir
   (tokens a b)

   (grammar ;Escribimos la gramatica de minHS
    (exp ((NUM) (make-num-exp $1)) ;num
         ((BOOL) (make-bool-exp $1)) ;bool
         ((VAR) (make-var-exp $1)) ;var

         ((INT) (make-int-exp)) ;int
         ((BOOLEAN) (make-boolean-exp));bool
         ;((FUNC exp exp) (make-func-exp $2 $3)) ;func

         ((exp + exp) (make-prim-exp + $1 $3)) ;suma
         ((exp - exp) (make-prim-exp - $1 $3)) ;resta
         ((exp * exp) (make-prim-exp * $1 $3)) ;mult
         ((exp / exp) (make-prim-exp / $1 $3)) ;div

         ;logicos
         ((exp AND exp) (make-prim-exp 'and $1 $3)) ;AND
         ((exp OR exp) (make-prim-exp 'or $1 $3)) ;OR
         ((exp = exp) (make-assign-exp $1 $3)) ;assign "="

         ((IF LP exp RP THEN LK exp RK) (make-if-then-exp $3 $7)) ; if-then
         ((IF LP exp RP THEN LK exp RK ELSE LK exp RK) (if-then-else-exp $3 $7 $11)) ;if-then-else
         ((FUN LP exp RP => exp) (fun-exp $3 $6)) ;fun
         ((FUNF LP exp LP exp RP : exp RP => exp) (fun-f-exp (typeof-exp $3 $5) $8 $11)) ;(exp(exp):exp)=>exp
         ((LET LP exp RP IN exp END) (let-exp $3 $6)) ;let
         
         ((exp APP exp) (make-app-exp $1 $3)) ;app
         ((exp APPT exp) (make-app-t-exp $1 $3)) ;( exp exp )
         ((exp : exp) (make-typeof-exp $1 $3)) ;typeof ":"         
         ((BEGIN LB exp RB) (make-begin-exp $3)) ;Begin
         
         ((LP exp RP) (make-par-exp $2)) ;Parentesis ()
         ((LB exp RB) (make-brack-exp $2)) ;Corchetes []
         ((LK exp RK) (make-key-exp $2)) ;Llaves)) {}

         ))))

;Función que almacena nuestro lexer en una función lambda sin argumentos.
(define (lex-this lexer input) (lambda () (lexer input)))

#|Ejercicio 2|#

(define (select-prim prim)
  (cond
    [(equal? prim +) "+"]
    [(equal? prim -) "-"]
    [(equal? prim *) "*"]
    [(equal? prim /) "/"]
    ;[(equal? prim =) "="]
    [(equal? prim 'and) "and"]
    [(equal? prim 'or) "or"]))

(define (expr->string e)
  (match e
    [(num-exp a) (number->string a)]
    [(bool-exp a) (format "~a" a)]
    [(var-exp a) (symbol->string a)]

    [(prim-exp pr a b) (string-append "(" (select-prim pr) " " (expr->string a) " " (expr->string b) ")")]

    [(int-exp) "Int"]
    [(boolean-exp) "Bool"]
    [(assign-exp (typeof-exp a b) c)(string-append "[" (expr->string a) " " (expr->string b) " = " (expr->string c) "]")]
    [(assign-exp a b) (string-append (expr->string a) " = " (expr->string b))]

    [(if-then-exp a b)(string-append "(if " (expr->string a) " " (expr->string b) ")")]
    [(if-then-else-exp a b c)(string-append "(if " (expr->string a) " " (expr->string b) " " (expr->string c) ")")]
    [(fun-exp a b) (string-append "(fun " (expr->string a) " " (expr->string b) ")")]
    [(fun-f-exp a b c)(string-append "(funF " (expr->string a) " " (expr->string b) " " (expr->string c) ")")]
    [(let-exp a b) (string-append "(let (" (expr->string a) ") " (expr->string b) ")")]

    [(app-exp a b) (string-append "(" (expr->string a) " " (expr->string b) ")")]
    [(app-t-exp a b) (string-append (expr->string a) " " (expr->string b))]
    [(app-t-exp (app-t-exp a b) c)(string-append (expr->string a) " " (expr->string b) " " (expr->string c))]
    [(typeof-exp (brack-exp a) b) (string-append "(" (expr->string a) ") " (expr->string b))]
    [(typeof-exp a b) (string-append "[" (expr->string a) " " (expr->string b) "]")]
    [(begin-exp a) (string-append "(begin " (expr->string a) ")")]

    [(par-exp (prim-exp a b c)) (expr->string (prim-exp a b c))]
    [(par-exp a) (string-append "(" (expr->string a) ")")]
    
    [(brack-exp (app-t-exp a b)) (expr->string (app-t-exp a b))]
    [(brack-exp a) (string-append "[" (expr->string a) "]")]

    [(key-exp (prim-exp a b c)) (expr->string (prim-exp a b c))]
    [(key-exp e) (string-append "(" (expr->string e) ")")]
    )
  )

; Expresión concreta:
; (33 + 2)
(expr->string (par-exp (prim-exp + (num-exp 33) (num-exp 2))))
;"(+ 33 2)"

; Expresión concreta:
; 3 - (3 / 6)
(expr->string (prim-exp - (num-exp 3)
                        (par-exp (prim-exp / (num-exp 3) (num-exp 6)))))
;"(- 3 (/ 3 6))"

; Expresión concreta:
; if(#t and #f)then{2}else{3}
(expr->string (if-then-else-exp (prim-exp 'and (bool-exp #t) (bool-exp #f))
                                (num-exp 2) (num-exp 3)))
;"(if (and #t #f) 2 3)"

; Expresión concreta:
; fun ([x:Int]:Int) => x
(expr->string (fun-exp (typeof-exp (brack-exp (typeof-exp (var-exp 'x)
                                                          (int-exp))) (int-exp)) (var-exp 'x)))
;"(fun ((x Int)) Int x)"

; Expresión concreta:
; fun ([x:Int][y:Int]:Int) => x*y
(expr->string (fun-exp
               (typeof-exp (brack-exp (app-t-exp
                                       (typeof-exp (var-exp 'x)
                                                   (int-exp)) (typeof-exp (var-exp 'y) (int-exp)))) (int-exp))
               (prim-exp * (var-exp 'x) (var-exp 'y))))
;"(fun ((x Int) (y Int)) Int (* x y))"

; Expresión concreta:
; funF (sumita ([x:Int][y:Int]):Int) => x+y
(expr->string (fun-f-exp
               (var-exp 'sumita)(typeof-exp (brack-exp
                                             (app-t-exp (typeof-exp (var-exp 'x) (int-exp))
                                                        (typeof-exp (var-exp 'y) (int-exp)))) (int-exp))
               (prim-exp + (var-exp 'x) (var-exp 'y))))
;"(funF sumita ((x Int) (y Int)) Int (+ x y))"

(display "\nExample 1: ((1 + 2) * (3 / 4)) = 2.25\n")
(let ((input (open-input-string "((1 + 2) * (3 / 4)) = 2")))
  (minHS-parser (lex-this minHS-lexer input)))
#|
Desired response:
(assign-exp
 (par-exp (prim-exp #<procedure:*> (par-exp (prim-exp #<procedure:+> (num-exp 1) (num-exp 2))) (par-exp (prim-exp #<procedure:/> (num-exp 3) (num-exp 4)))))
 (num-exp 2))
|#

(display "\nExample 2: if(#t)then{2}else{3}\n")
(let ((input (open-input-string "if(#t)then{2}else{3}")))
  (minHS-parser (lex-this minHS-lexer input)))
#|
Desired response:
(if-exp (bool-exp #t) (num-exp 2) (num-exp 3))
|#

(display "\nExample 3: fun ([x:Int]:Int) => x\n")
(let ((input (open-input-string "fun ([x:Int]:Int) => x")))
  (minHS-parser (lex-this minHS-lexer input)))
#|
Desired response:
(fun-exp (typeof-exp (brack-exp (typeof-exp (var-exp 'x) (int-exp))) (int-exp)) (var-exp 'x))


(display "\nExample 4: fun ([f:Func Int Int]:Int) => f app 1\n")
(let ((input (open-input-string "fun ([f:Func Int Int]:Int) => f app 1")))
  (minHS-parser (lex-this minHS-lexer input)))

Desired response:
(fun-exp (typeof-exp (brack-exp (typeof-exp (var-exp 'f) (func-exp (int-exp) (int-exp)))) (int-exp)) (app-exp (var-exp 'f) (num-exp 1)))


(display "\nExample 5: fun ([f:Func (Func Int Bool) Int]:Bool) => #t\n")
(let ((input (open-input-string "fun ([f:Func (Func Int Bool) Int]:Bool) => #t")))
  (minHS-parser (lex-this minHS-lexer input)))

Desired response:
(fun-exp (typeof-exp (brack-exp (typeof-exp (var-exp 'f) (func-exp (par-exp (func-exp (int-exp) (boole-exp))) (int-exp)))) (boole-exp)) (bool-exp #t))
|#

(display "\nExample 6: funF (sumita ([x:Int][y:Int]):Int) => x+y\n")
(let ((input (open-input-string "funF (sumita ([x:Int][y:Int]):Int) => x+y")))
  (minHS-parser (lex-this minHS-lexer input)))
#|
Desired response:
(fun-f-exp
 (typeof-exp (var-exp 'sumita) (brack-exp (app-t-exp (typeof-exp (var-exp 'x) (int-exp)) (typeof-exp (var-exp 'y) (int-exp)))))
 (int-exp)
 (prim-exp #<procedure:+> (var-exp 'x) (var-exp 'y)))
|#

(display "\nExample 7: let ([x:Int = 1][y:Int = 2]) in x+y end\n")
(let ((input (open-input-string "let ([x:Int = 1][y:Int = 2]) in x+y end")))
  (minHS-parser (lex-this minHS-lexer input)))
#|
Desired response:
(let-exp
 (brack-exp (app-t-exp (assign-exp (typeof-exp (var-exp 'x) (int-exp)) (num-exp 1)) (assign-exp (typeof-exp (var-exp 'y) (int-exp)) (num-exp 2))))
 (prim-exp #<procedure:+> (var-exp 'x) (var-exp 'y)))
|#

(display "\nExample 8: ((funF (sumita ([x:Int][y:Int]):Int) => x+y) app 2) app 4\n")
(let ((input (open-input-string "((funF (sumita ([x:Int][y:Int]):Int) => x+y) app 2) app 4")))
  (minHS-parser (lex-this minHS-lexer input)))
#|
Desired response:
(app-exp
 (par-exp
  (app-exp
   (par-exp
    (fun-f-exp
     (typeof-exp (var-exp 'sumita) (brack-exp (app-t-exp (typeof-exp (var-exp 'x) (int-exp)) (typeof-exp (var-exp 'y) (int-exp)))))
     (int-exp)
     (prim-exp #<procedure:+> (var-exp 'x) (var-exp 'y))))
   (num-exp 2)))
 (num-exp 4))
|#