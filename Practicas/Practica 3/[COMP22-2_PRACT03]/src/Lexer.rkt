#lang nanopass

#|
   Compiladores 2022-2
   Practica 3

   Autores:
   Montiel Manriquez Ricardo
   Escamilla Soto Cristopher Alejandro
|#

;; Bibliotecas chidas para lexear
(require parser-tools/lex
         parser-tools/lex-plt-v200
         (prefix-in : parser-tools/lex-sre);Operadores
         (prefix-in re- parser-tools/lex-sre)
         parser-tools/yacc)

(provide (all-defined-out));Exporta todos los identificadores que están definidos en el  nivel
;de fase relevante dentro del módulo de exportación, y que tienen el mismo contexto léxico

; NUM = Valores numericos
; VAR = Variables
; BOOL = Valores booleanos
(define-tokens a (NUM VAR BOOL))

; LP = Parentesis izquierdos
; RP = Parentesis derechos
; LCB = Llaves izquierdas
; RCB = Llaves derechar
; LRB = Corchetes izquierdos
; LRB = Corchetes derechos
; EOF = Fin del documento
(define-empty-tokens b (LP RP LK RK LB RB + - * / : => = IF THEN ELSE FUN FUNC FUNF LET IN END APP APPT AND OR EOF INT BOOLEAN BEGIN))

; sre : S-regular expressions
(define calc-lexer
           (lexer

            ["if"
              ; =>
              (token-IF)]

            ["then"
              ; =>
              (token-THEN)]

            ["else"
              ; =>
              (token-ELSE)]

            ["fun"
              ; =>
              (token-FUN)]

            ["Func"
              ; =>
              (token-FUNC)]

            ["funF"
              ; =>
              (token-FUNF)]

            ["let"
              ; =>
              (token-LET)]

            ["in"
              ; =>
              (token-IN)]

            ["end"
              ; =>
              (token-END)]
            
            ["app"
              ; =>
              (token-APP)]

            [(:: #\] #\[)
             ; =>
             (token-APPT)]

            ["and"
              ; =>
              (token-AND)]
            
            ["or"
              ; =>
              (token-OR)]

            ["#t"
              ; =>
              (token-BOOL (equal? lexeme "#t"))]

             ["#f"
              ; =>
              (token-BOOL (equal? lexeme "#t"))]

             ["Int"
              ; =>
              (token-INT)]

             ["Bool"
              ; =>
              (token-BOOLEAN)]

             ["=>"
              ;=>
              (token-=>)]

             [#\:
              ;=>
              (token-:)]

             [#\=
              ;=>
              (token-=)]
            
             [(:+ (:or (char-range #\a #\z) (char-range #\A #\Z))) ; (a-z | A-Z)^+
              ; =>
              (token-VAR (string->symbol lexeme))]

             [(::  (:or #\- (epsilon)) (:: (:* (char-range #\0 #\9)) (:: (:or (:: #\. (char-range #\0 #\9)) (:: (char-range #\0 #\9)) #\.) (:* (char-range #\0 #\9)))))
              ; =>
              (if (integer? (string->number lexeme))
              (token-NUM (string->number lexeme))
              (error "No acepta flotantes"))]

             [#\+
              ; =>
              (token-+)]

             [#\-
              ; =>
              (token--)]

             [#\*
              ; =>
              (token-*)]

             [#\/
              ; =>
              (token-/)]

             ["Begin"
              ; =>
              (token-BEGIN)]

             [#\(
              ; =>
              (token-LP)]

             [#\)
              ; =>
              (token-RP)]

             [#\{
              ; =>
              (token-LK)]
             
             [#\}
              ; =>
              (token-RK)]

             [#\[
              ; =>
              (token-LB)]

             [#\]
              ; =>
              (token-RB)]

             [whitespace
              ; =>
              (calc-lexer input-port)]

             [(eof)
              (token-EOF)]

             ))

; Llamada a MinHS
(define (minHS-lexer l)
  (calc-lexer l))

(define-struct arith-exp (op e1 e2) #:transparent)
(define-struct num-exp (n) #:transparent)
(define-struct var-exp (i) #:transparent)

