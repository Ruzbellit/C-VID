#lang eopl

;******************************************************************************************
;;;;; Interpretador para lenguaje con condicionales, ligadura local y procedimientos

;; La definición BNF para las expresiones del lenguaje:
;;
;;  <program>       ::= <expression>
;;                      <c-vid-program (exp)>
;;  <expression>    ::= <int>
;;                      <int-exp (datum)>
;;                  ::= <float>
;;                      <float-exp (datum)>
;;                  ::= <identifier>
;;                      <var-exp (id)>
;;                  ::= <char>
;;                      <char-exp (datum)>
;;                  ::= <string>
;;                      <string-exp (datum)>
;;                  ::= <exp-bool>
;;                      <bool-exp (datum)>
;;
;;                  ::= <list>
;;                      ({<expression>}*(,))
;;                  ::= <vector>
;;                      [{<expression>}*(,)]
;;                  ::= <record>
;;                      [{<expression>}*(,)]
;;
;;                  ::= <primitive> ({<expression>}*(,))
;;                      <primapp-exp (prim rands)>
;;                  ::= if <expresion> then <expresion> else <expression>
;;                      <if-exp (exp1 exp2 exp23)>
;;                  ::= let {identifier = <expression>}* in <expression>
;;                      <let-exp (ids rands body)>
;;                  ::= proc({<identificador>}*(,)) <expression>
;;                      <proc-exp (ids body)>
;;                  ::= (<expression> {<expression>}*)
;;                      <app-exp proc rands>
;;  <primitive-op>  ::= + | - | * | / | > | >= | < | <= | == | <>
;;  <exp-bool>      ::= <bool>
;;                  ::= <compare-exp>
;;                      compare(<expression> <primitive-op> <expression>)
;;                  ::= <bin-bool-exp>
;;                      <bool-op> (<exp-bool>, <exp-bool>)
;;                  ::= <neg-bool-exp>
;;                      <neg-bool> <exp-bool>
;;  <bool-op>       ::= AND | OR | XOR 
;;  <bool-neg>      ::= NOT

;******************************************************************************************

;Especificación Léxica

(define scanner-spec-simple-interpreter
'((white-sp
   (whitespace) skip)
  (comment
   ("%" (arbno (not #\newline))) skip)
  (identifier
   (letter (arbno (or letter digit "?"))) symbol)
  (int
   (digit (arbno digit)) number)
  (int
   ("-" digit (arbno digit)) number)
  (float
   (digit (arbno digit) "." digit (arbno digit)) number)
  (float
   ("-" digit (arbno digit) "." digit (arbno digit)) number)
  (char
   ("'" letter "'") string)
  (str
   ("\"" (arbno any) "\"") string)
  (bool
   ((or "true" "false")) string)
  ))

;Especificación Sintáctica (gramática)

(define grammar-simple-interpreter
  '((program (expression) c-vid-program)

    (expression (identifier) var-exp)
    
    ;;Datos
    (expression (int) int-exp)
    (expression (float) float-exp)
    (expression (char) char-exp)
    (expression (str) string-exp)
    (expression (exp-bool) bool-exp)
;    (expression (bool) bool-exp)

    ;;Constructores de Datos Predefinidos
    (expression ("(" expression (arbno "," expression) ")") list-exp)
    (expression ("[" expression (arbno "," expression ) "]") vector-exp)
    (expression ("{" identifier "=" expression (arbno ";" identifier "=" expression) "}") record-exp)

    (exp-bool (bool) bool-val)
    (exp-bool ("compare(" expression primitive-op expression ")") compare-exp)
    (exp-bool (bool-op "(" exp-bool "," exp-bool ")") bin-bool-exp)
    (exp-bool (bool-neg exp-bool) neg-bool-exp)

    (primitive-op ("<") less_than-op)
    (primitive-op (">") greater_than-op)
    (primitive-op ("<=") less_equal-op)
    (primitive-op (">=") greater_equal-op)
    (primitive-op ("==") equal)
    (primitive-op ("<>") not_equal)
    (bool-op ("AND") and-op)
    (bool-op ("OR") and-op)
    (bool-op ("XOR") and-op)
    (bool-neg ("NOT") neg-op)

    ;;Definiciones
    (expression ("global" "(" identifier "=" expression (arbno "," identifier "=" expression) ")") global-def)
    (expression ("var" "(" identifier "=" expression (arbno "," identifier "=" expression) ")" "in" expression) var-def)
    (expression ("cons" "(" identifier "=" expression (arbno "," identifier "=" expression) ")" "in" expression) cons-def)
    (expression ("rec" "(" identifier "=" expression(arbno "," identifier "=" expression) ")" "in" expression) rec-def)
    (expression ("unic" "(" identifier "=" expression (arbno "," identifier "=" expression) ")" "in" expression) unic-def)

    ;;Estructuras de Control
    (expression ("sequence" expression (arbno ";" expression) "end") seq-exp)
    (expression ("if" exp-bool "then" expression "else" expression "end") if-exp)
    (expression ("cond" "[" expression expression "]" (arbno ";" "[" expression expression "]") "else" expression) cond-exp)
    (expression ("while" exp-bool "do" expression "done") while-exp)
    (expression ("for" identifier "=" expression for-type expression "do" expression "done") for-exp)

    (for-type ("to") for-to)
    (for-type ("downto") for-downto)

    )
  )


;Construidos automáticamente:

(sllgen:make-define-datatypes scanner-spec-simple-interpreter grammar-simple-interpreter)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes scanner-spec-simple-interpreter grammar-simple-interpreter)))

;*******************************************************************************************
;Parser, Scanner, Interfaz

;El FrontEnd (Análisis léxico (scanner) y sintáctico (parser) integrados)

(define scan&parse
  (sllgen:make-string-parser scanner-spec-simple-interpreter grammar-simple-interpreter))

;El Analizador Léxico (Scanner)

(define just-scan
  (sllgen:make-string-scanner scanner-spec-simple-interpreter grammar-simple-interpreter))


;;PRUEBAS
(scan&parse "0")
(scan&parse "1")
(scan&parse "-1")
(scan&parse "3.14")
(scan&parse "-1.5")
(scan&parse "'R'")
(scan&parse "\"hola mundo 7\"")
(scan&parse "true")
(scan&parse "false")
(scan&parse "AND (true, false)")
(scan&parse "compare(5>2)")

(scan&parse "sequence 123; 'R'; -5; true end")
(scan&parse "if compare(5>2) then 10 else 0 end")