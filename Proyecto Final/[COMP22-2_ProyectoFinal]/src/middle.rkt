#lang nanopass

#| Compiladores
   Proyecto Final

   Escamilla Soto Cristopher Alejandro 314309253
   Montiel Manriquez Ricardo 314332662
   Villegas Salvador Kevin Ricardo - 314173739 |#

(require "front.rkt")
(provide (all-defined-out))

;--------------------- Proceso 9 | type-const ---------------------

;; Lenguaje L6
(define-language L6
  (extends L5)
  (Expr (e body)
        (- (quot c))
        (+ (const t c))))

;; Parser para el lenguaje L10
(define-parser parser-L6 L6)

;; Coloca las anotaciones de tipos correspondientes a las constantes
;; type-const : L9 (e) -> L10 (e)
(define-pass type-const : L5 (e) -> L6 ()
  (Expr : Expr (e) -> Expr ()
        [(quot ,c)
         (cond
           [(boolean? c) `(const Bool ,c)]
           [(number? c) `(const Int ,c)]
           [(char? c) `(const Char ,c)])]))

;--------------------- Proceso 10 | type-infer ---------------------

;; Elimina la anotación de tipo Lambda y se sustituye por el tipo `(T → T)
;; que corresponde a la definición de la función además, sustituye las anotaciones
;; de tipo List por el tipo (List of T) 
;; type-infer: L10 (e) -> L10 (e)
(define-pass type-infer : L6 (e) -> L6 ()
  (Expr : Expr (e) -> Expr ()
        [(let ([,x ,t ,e]) ,body)
         (if (equal? t 'List)
             (let ([t0 (J e '())])
               `(let ([,x ,t0 ,e]) ,body))
             `(let ([,x ,t ,e]) ,body))]
        [(letrec ([,x ,t ,e]) ,body)
         (let ([t0 (J e '())])
           `(letrec ([,x ,t0 ,e]) ,body))]
        [(letfun ([,x ,t ,e]) ,body)
         (let ([t0 (J e '())])
           `(letfun ([,x ,t0 ,e]) ,body))]))

;; Regresa el tipo correspondiente a la expresión
;; J : L10 (e) -> listof(ctx) -> typeof(e)
(define (J expr ctx)
  (nanopass-case (L6 Expr) expr
                 [,x (get x ctx)]
                 [(const ,t ,c) t]
                 [(begin ,e* ... ,e) (J e ctx)]
                 [(primapp ,pr ,e* ...)
                  (let* ([args (map (λ (x) (J x ctx)) e*)]
                         [correct?
                          (case pr
                            [(+ - / * < > ++ -- iszero?) (andmap (λ (x) (equal? x 'Int)) args)]
                            [(or and not) (andmap (λ (x) (equal? x 'Bool)) args)]
                            [(length car cdr) (andmap (λ (x) (and (list? x) (equal? (car x) 'List))) args)])])
                    (if correct?
                        (case pr
                          [(+ - / * length < > ++ -- iszero?) 'Int]
                          [(and or not) 'Bool]
                          [(car) (caddr (car args))]
                          [(cdr) (car args)])
                        (error "Los tipos de los argumentos no corresponden a la primitiva dada")))]
                 [(if ,e0 ,e1 ,e2)
                  (let ([t0 (J e0 ctx)]
                        [t1 (J e1 ctx)]
                        [t2 (J e2 ctx)])
                    (if (and (unify t0 'Bool) (unify t1 t2))
                        t1
                        (error "F: Las ramas del if no tienen el mismo tipo y/o la condición no es booleana")))]
                 [(while [,[e0]] ,e1)
                  (let ([t0 (J e0 ctx)]
                        [t1 (J e1 ctx)])
                    (if (unify t0 'Bool)
                        t1
                        (error "F: No hay una condición booleana")))]
                 [(lambda ([,x ,t]) ,body)
                  (let* ([ctxN (set-add ctx (cons x t))]
                         [type (J body ctxN)])
                    `(,t → ,type))]
                 [(let ([,x ,t ,e]) ,body)
                  (let ([ctxN (set-add ctx (cons x t))]
                        [t0 (J e ctx)])
                    (if (unify t t0)
                        (J body ctxN)
                        (error "F: Los tipos no coinciden en 'let'")))]
                 [(letrec ([,x ,t ,e]) ,body)
                  (let* ([ctxN (set-add ctx (cons x t))]
                         [t0 (J e ctxN)])
                    (if (unify t t0)
                        (J body ctxN)
                        (error "F: Los tipos no coinciden en 'letrec'")))]
                 [(letfun ([,x ,t ,e]) ,body)
                  (if (and (list? t) (equal? (second t) '→))
                      (let ([ctxN (set-add ctx (cons x t))]
                            [t0 (J e ctx)])
                        (if (unify t t0)
                            (J body ctxN)
                            (error "F: Los tipos no coinciden en 'letfun'")))
                      (error "F: El tipo recibido o letfun no es una función"))]
                 [(list ,e* ...)
                  (if (empty? e*)
                      'List
                      (let ([t (J (car e*) ctx)]
                            [t1 (map (λ (x) (J x ctx)) (cdr e*))])
                        (if (or (null? t1) (check-types t t1))
                            `(List of ,t)
                            (error "F: La lista no admite elementos con diferentes tipos"))))]
                 [(,e0 ,e1)
                  (let* ([t0 (J e0 ctx)]
                         [t1 (J e1 ctx)])
                    (if (list? t0)
                        (if (unify (car t0) t1)
                            (third t0)
                            (error "F: El dominio y la entrada de la función son "))
                        (error "F: El primer parámetro no es una función")))]
                 [(for [,x ,[e0]] ,e1) (let ([ctxN (set-add ctx (cons x (third e0)))])
                                         (J e1 ctxN))]))

;; Regresa el tipo de una variable dado un contexto
;; get : var -> listof (listof (var type)) -> typeof(var)
(define (get var ctx)
  (cond
    [(empty? ctx) (error "Var without type")]
    [(equal? var (caar ctx)) (cdar ctx)]
    [else (get var (cdr ctx))]))

;; Verifica que una lista sea de un mismo tipo
;; check-types : typeof(T) -> listof(typeof) -> Bool
(define (check-types t t1)
  (let* ([t0 (unify t (car t1))])
    (if (null? (cdr t1))
        t0
        (if t0
            (check-types t (cdr t1))
            #f))))

;; Verifica si t1 es unificable con t2 sin regresar el unificador
;; unify : typeof(T) -> typeof(T)  -> Bool
(define (unify t1 t2)
  (if (and (type? t1) (type? t2))
      (cond
        [(equal? t1 t2) #t]
        [(and (equal? 'List t1) (list? t2)) (equal? (car t2) 'List)]
        [(and (equal? 'List t2) (list? t1)) (equal? (car t1) 'List)]
        [(and (list? t1) (list? t2))
         (and (unify (car t1) (car t2)) (unify (caddr t1) (caddr t2)))]
        [else #f])
      (error "Se esperaban 2 tipos")))

;--------------------- Proceso 11 | uncurry ---------------------

;; Lenguaje L11
(define-language L7
  (extends L6)
  (Expr (e body)
        (- (lambda ([x t]) body* ... body))
        (+ (lambda ([x* t*] ...) body))))

;; Parser para el lenguaje L11
(define-parser parser-L7 L7)

;; Función para obtener los parámetros así como el cuerpo de lambda
;; params : L7 (e) -> list -> (listof expr '((x y)))
(define (params expr acc)
  (nanopass-case (L7 Expr) expr
                 [(lambda ([,x ,t]) ,body)
                  (params body (append acc (list (list x t))))]
                 [else (list expr acc)]))

;; Descurrifica las expresiones lambda
;; uncurry : L10 (e) -> L11 (e)
(define-pass uncurry : L6 (e) -> L7 ()
  (Expr : Expr (e) -> Expr ()
        [(lambda ([,x ,t]) ,body) (let* ([param (params e '())]
                                         [b-body (first param)]
                                         [par (second param)]
                                         [x* (map car par)]
                                         [t* (map second par)])
                                    `(lambda ([,x* ,t*] ...) ,(uncurry b-body)))]
        [(const ,t ,c)
         (with-output-language (L7 Expr) `(const ,t ,c))]
        [(begin ,[e*] ... ,[e])
         (with-output-language (L7 Expr) `(begin ,e* ... ,e))]
        [(primapp ,pr ,[e*] ...)
         (with-output-language (L7 Expr) `(primapp ,pr ,e* ...))]
        [(if ,[e0] ,[e1] ,[e2])
         (with-output-language (L7 Expr) `(if ,e0 ,e1 ,e2))]
        [(let ([,x ,t ,[e]]) ,[body])
         (with-output-language (L7 Expr) `(let ([,x ,t ,e]) ,body))]
        [(letrec ([,x ,t ,[e]]) ,[body])
         (with-output-language (L7 Expr) `(letrec ([,x ,t ,e]) ,body))]
        [(letfun ([,x ,t ,[e]]) ,[body])
         (with-output-language (L7 Expr) `(letfun ([,x ,t ,e]) ,body))]
        [(list ,[e*] ...)
         (with-output-language (L7 Expr) `(list ,e* ...))]
        [(while [,[e0]] ,e1)
         (with-output-language (L7 Expr) `(while [,e0] e1))]
        [(,[e0] ,[e1])
         (with-output-language (L7 Expr) `(,e0 ,e1))]
        [else (parser-L7 (unparse-L6 e))]))

;--------------------- Middle-end ---------------------

(define (middle expr)
  (uncurry
   (type-infer
    (type-const
     (front expr)))))