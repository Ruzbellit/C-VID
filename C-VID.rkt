#lang eopl

;******************************************************************************************
;;;;; Interpretador para lenguaje con condicionales, ligadura local y procedimientos

;; La definición BNF para las expresiones del lenguaje:
;;
;;  <program>       ::= <expression>
;;                      <a-program (exp)>
;;  <expression>    ::= <integer>
;;                      <int-exp (datum)>
;;                  ::= <identifier>
;;                      <var-exp (id)>
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
;;  <primitive>     ::= + | - | * | add1 | sub1 

;******************************************************************************************

;Especificación Léxica

(define scanner-spec-simple-interpreter
'((white-sp
   (whitespace) skip)
  (comment
   ("%" (arbno (not #\newline))) skip)
  (identifier
   (letter (arbno (or letter digit "?"))) symbol)
  (integer
   (digit (arbno digit)) number)
  (integer
   ("-" digit (arbno digit)) number)
  (float
   (digit (arbno digit) "," digit (arbno digit)) float)
  (float
   ("-" digit (arbno digit) "," digit (arbno digit)) float)))

;Especificación Sintáctica (gramática)

(define grammar-simple-interpreter
  '((program (global_def expression) C-VID_program)

    ;;Datos
    (expression (integer) int-exp)
    (expression (float) float-exp)
    (expression (identifier) var-exp)
    (expression (or (eqv? identifier "true") (eqv? identifier "false")) bool)

    ;;Constructores de Datos Predefinidos
    (expression ("(" expression (arbno "," expression) ")") exp_list)
    (expression ("[" expression (arbno "," expression ) "]") exp_vector)
    (expression ("{" expression (arbno ";" expression) "}") exp_record)
    
    (expression_bool (expression prim-bool expression) exp_bool_prim)
    (expression_bool (oper-bool expression_bool expression_bool) exp_bool_oper)
    (expression_bool (bool) exp_bool)
    (expression_bool (neg_bool expression_bool) exp_bool_neg)

    (prim-bool ("<") less_oper?)
    (prim-bool (">") greater_oper?)
    (prim-bool ("<=") less_equal_oper?)
    (prim-bool (">=") greater_equal?)
    (prim-bool ("==") equal?)
    (prim-bool ("<>") not_equal?)

    (oper-bool ("and") and_oper)
    (oper-bool ("or") or_oper)
    (oper-bool ("xor") xor_oper)

    (neg-bool ("not") neg_bool)

    ;;Definiciones
    (expression ("global" "(" identifier "=" expression (arbno "," identifier "=" expression) ")") global_def)
    (expression ("var" "(" identifier "=" expression (arbno "," identifier "=" expression) ")" "in" expression) var_def)
    (expression ("cons" "(" identifier "=" expression (arbno "," identifier "=" expression) ")" "in" expression) cons_def)
    (expression ("rec" "(" identifier "=" expression(arbno "," identifier "=" expression) ")" "in" expression) rec_def)
    (expression ("unic" "(" identifier "=" expression (arbno "," identifier "=" expression) ")" "in" expression) unic_def)

    ;;Estructuras de Control
    (expression ("sequence" expression (arbno ";" expression)) exp_seq)
    (expression ("if" exp_expression_bool "then" expression "else" expression) exp_if)
    (expression ("cond" "{" "[" expression expression "]" (arbno ";" "[" expression expression "]")) exp_cond)
    (expression ("while" expression_bool "do" expression) exp_while)
    (expression ("for" identifier "=" expression "do" expression))
    )
  )