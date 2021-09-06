#lang eopl

;******************************************************************************************
;;;;; Interpretador para el lenguaje C-VID

;; La definición BNF para las expresiones del lenguaje:
;;
;;  <program>       ::= <globals expression>
;;  <globals>       ::= global( {<identifier> = expression}*(,) )
;;                      <global (ids rands)>
;;  <expression>    ::= <int>
;;                      <int-exp (datum)>
;;                  ::= <float>
;;                      <float-exp (datum)>
;;                  ::= <octal>
;;                      <octal-exp (datum)>
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
;;                      '({<expression>}*(,))
;;                  ::= <vector>
;;                      vector[{<expression>}*(,)]
;;                  ::= <record>
;;                      [{<expression>}*(,)]
;;
;;                  ::= var {<identifier> = <expression>}*(,) in <expression>
;;                  ::= const {<identifier> = <expression>}*(,) in <expression>
;;                  ::= rec {<identifier> ( {<identifier>}*(,) ) = <expression>}* in <expression>
;;                  ::= unic {<identifier> = <expression>}*(,) in <expression>
;;
;;                  ::= secuence {<expresion>}+(;) end
;;                      <secuence-exp (exps)>
;;                  ::= if <exp-bool> then <expresion> else <expression> end
;;                      <if-exp (exp1 exp2 exp3)>
;;                  ::= cond { [ <expression> <expression> ] }* else <expression> end
;;                      <cond-exp (exp1 exp2 exp3)>
;;                  ::= while <exp-bool> do <expression> done
;;                      <while-exp (pred body)>
;;                  ::= for <identifier> = <expression> <for-type> <expression> do <expression> done
;;                      <cond-exp (exp1 exp2 exp3)>
;;
;;                  ::= (<expression> {<expression>}*)
;;                      <app-exp proc rands>
;;  <exp-bool>      ::= <bool>
;;                  ::= <compare-exp>
;;                      compare(<expression> <primitive-op> <expression>)
;;                  ::= <bin-bool-exp>
;;                      <bool-op> (<exp-bool>, <exp-bool>)
;;                  ::= <neg-bool-exp>
;;                      <neg-bool> <exp-bool>
;;  <primitive-op>  ::= > | >= | < | <= | == | <>
;;  <bool-op>       ::= AND | OR | XOR 
;;  <bool-neg>      ::= NOT
;;  <arith-prim>    ::= + | - | * | / | % | ++ | --
;;  <for-type>      ::= to | downto

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
   ("\"" (arbno (not #\")) "\"") string)
  ))

;Especificación Sintáctica (gramática)

(define grammar-simple-interpreter
  '((program (globals expression) c-vid-program)

    (globals ("global" "(" (separated-list identifier "=" expression ",") ")") global)
    (expression (identifier) var-exp)
    
    ;;Datos
    (expression (int) int-exp)
    (expression (float) float-exp)
    (expression (char) char-exp)
    (expression (str) string-exp)
    (expression (exp-bool) bool-exp)
    (expression ("x8(" (arbno int) ")") octal-exp)
    (exp-bool ("true") true-exp)
    (exp-bool ("false") false-exp)

    ;;Constructores de Datos Predefinidos
    (expression ("'(" expression (arbno "," expression) ")") list-exp)
    (expression ("vector[" expression (arbno "," expression ) "]") vector-exp)
    (expression ("{" identifier "=" expression (arbno ";" identifier "=" expression) "}") record-exp)

    ;;Expresiones booleanas
    ;(exp-bool (expression) bool-val)
    (exp-bool ("compare(" expression primitive-op expression ")") compare-exp)
    (exp-bool (bool-op "(" exp-bool "," exp-bool ")") bin-bool-exp)
    (exp-bool (bool-neg exp-bool) neg-bool-exp)

    ;;Primitivas
    (primitive-op ("<") less_than-op)
    (primitive-op (">") greater_than-op)
    (primitive-op ("<=") less_equal-op)
    (primitive-op (">=") greater_equal-op)
    (primitive-op ("==") equal)
    (primitive-op ("<>") not_equal)
    (bool-op ("AND") and-op)
    (bool-op ("OR") or-op)
    (bool-op ("XOR") xor-op)
    (bool-neg ("NOT") neg-op)
    (arith-prim ("+") add-prim)
    (arith-prim ("-") substract-prim)
    (arith-prim ("*") mult-prim)
    (arith-prim ("%") percent-prim)
    (arith-prim ("/") div-prim)
    (arith-prim ("++") incr-prim)
    (arith-prim ("--") decr-prim)
    (concat-op ("concat") concat-prim)
    (length-op ("length") length-prim)

    (expression ("(" expression arith-prim expression ")") arith-exp)
    (expression (length-op "(" expression ")") length-exp)
    (expression (concat-op "(" expression ";" expression ")") concat-exp)

    ;;Definiciones
    (expression ("var" (separated-list identifier "=" expression ",") "in" expression) var-def)
    (expression ("const" (separated-list identifier "=" expression ",") "in" expression) const-def)
    (expression ("rec" (arbno identifier "(" (separated-list identifier ",") ")" "=" expression) "in" expression) rec-def)
    (expression ("unic" (separated-list identifier "=" expression ",") "in" expression) unic-def)

    ;;Estructuras de Control
    (expression ("sequence" expression (arbno ";" expression) "end") seq-exp)
    (expression ("if" exp-bool "then" expression "else" expression "end") if-exp)
    ;;(expression ("cond" "[" expression expression "]" (arbno ";" "[" expression expression "]") "else" expression) cond-exp)
    (expression ("cond" (arbno "[" expression expression "]") "else" expression) cond-exp)
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
(scan&parse "global(x=1, y=2) (x+y)")
(scan&parse "global() 0")
(scan&parse "global() 1")
(scan&parse "global() -1")
(scan&parse "global() 3.14")
(scan&parse "global() -1.5")
(scan&parse "global() 'R'")
(scan&parse "global() \"hola mundo 7\"")
(scan&parse "global() true")
(scan&parse "global() false")
(scan&parse "global() AND (true, false)")
(scan&parse "global() NOT true")
(scan&parse "global() compare(5>2)")

;;lista, vector y registro
(scan&parse "global() '(1, true, 'X')")
(scan&parse "global() vector[10, true, 'X']")
(scan&parse "global() {x = 5; y = 7}")

;;estructuras de control
(scan&parse "global() sequence 123; \"Hola\" ; 'R'; -5; true end")
(scan&parse "global() if compare(5>2) then 10 else 0 end")
(scan&parse "global() cond [ compare(5>0) true] else 0")
(scan&parse "global() cond [ compare(5>0) \"paso FLP\"] [compare(5>3) \"arrastrado pero lo paso\"] else 0")
(scan&parse "global() while true do (i+1) done")
(scan&parse "global() for x = 0 to 5 do (y+1) done")
(scan&parse "global() for x = 10 downto 5 do (y+1) done")

;; operaciones sobre cadenas
(scan&parse "global() length(\"hola\")")
(scan&parse "global() concat(\"hola\" ; \" mundo\")")

;;definiciones
(scan&parse "global() var x=1, y=2 in (x+y)")
(scan&parse "global() const x=1, y=2 in (x+y)")
(scan&parse "global() unic x=1, y=2 in (x+y)")

(scan&parse "global() x8(1 5 7)")