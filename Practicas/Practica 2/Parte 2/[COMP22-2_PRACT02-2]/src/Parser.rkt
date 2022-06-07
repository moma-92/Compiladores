#lang nanopass
#|
     Compiladores 2022 - 2
    Lab. 16 Marzo 2022|#

;;*********************   Parser    **************************************

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
(define-struct assign-exp (a b) #:transparent) ; Igual "="
(define-struct int-exp () #:transparent) ;Tipo Int
(define-struct boole-exp () #:transparent) ;Tipo bool

(define-struct func-exp (a b) #:transparent) ;Tipo Func
(define-struct funf-exp (a b c) #:transparent) ; Tipo Funf
(define-struct prim-exp (a b c) #:transparent) ;Operaciones aritmeticas

(define-struct if-then-exp (a b c) #:transparent) ;If-then-else
(define-struct fun-exp (a b) #:transparent) ;Tipo Fun
(define-struct fun-f-exp (a b c) #:transparent) ;Funciones
(define-struct app-exp (a b) #:transparent) ;app
(define-struct app-t-exp (a b) #:transparent) ;appT
(define-struct let-exp (a b) #:transparent) ;Expresiones Let

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
         ((BOOLE) (make-boole-exp));bool
         ((FUNC exp exp) (make-func-exp $2 $3)) ;func

         ((exp + exp) (make-prim-exp + $1 $3)) ;suma
         ((exp - exp) (make-prim-exp - $1 $3)) ;resta
         ((exp * exp) (make-prim-exp * $1 $3)) ;mult
         ((exp / exp) (make-prim-exp / $1 $3)) ;div

         ;logicos
         ((exp AND exp) (make-prim-exp 'and $1 $3)) ;AND
         ((exp OR exp) (make-prim-exp 'or $1 $3)) ;OR
         ((exp EQ exp) (make-assign-exp $1 $3)) ;assign "="

         ((IF LP exp RP THEN LK exp RK ELSE LK exp RK) (make-if-then-exp $3 $7 $11)) ;if-then-else
         ((FUN LP exp RP ARR exp) (make-fun-exp $3 $6)) ;fun
         ((FUNF LP exp exp RP ARR exp) (make-funf-exp $3 $4 $7)) ;funf
         ((LET exp IN exp END) (make-let-exp $2 $4)) ;let
         
         ((exp APP exp) (make-app-exp $1 $3)) ;appt
         ((exp TYPEOF exp) (make-typeof-exp $1 $3)) ;typeof ":"
         ((LP exp exp RP ) (make-app-t-exp $2 $3)) ;( exp exp )
         
         ((LP exp RP) (make-par-exp $2)) ;Parentesis ()
         ((LB exp RB) (make-brack-exp $2)) ;Corchetes []
         ((LK exp RK) (make-key-exp $2)) ;Llaves)) {}

         ))))

;Función que almacena nuestro lexer en una función lambda sin argumentos.
(define (lex-this lexer input) (lambda () (lexer input)))

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
|#

(display "\nExample 4: fun ([f:Func Int Int]:Int) => f app 1\n")
(let ((input (open-input-string "fun ([f:Func Int Int]:Int) => f app 1")))
  (minHS-parser (lex-this minHS-lexer input)))
#|
Desired response:
(fun-exp (typeof-exp (brack-exp (typeof-exp (var-exp 'f) (func-exp (int-exp) (int-exp)))) (int-exp)) (app-exp (var-exp 'f) (num-exp 1)))
|#

(display "\nExample 5: fun ([f:Func (Func Int Bool) Int]:Bool) => #t\n")
(let ((input (open-input-string "fun ([f:Func (Func Int Bool) Int]:Bool) => #t")))
  (minHS-parser (lex-this minHS-lexer input)))
#|
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
