#lang eopl

;Especificacion Lexica

(define lexica
'((white-sp
   (whitespace) skip)
  (comment
   ("%" (arbno (not #\newline))) skip)
  (identifier
   (letter (arbno (or letter digit "?"))) symbol)
  (number
   (digit (arbno digit)) number)
  (number
   ("-" digit (arbno digit)) number)
  (float
   (digit (arbno digit) "," digit (arbno digit)) float)
  (float
   ("-" digit (arbno digit) "," digit (arbno digit)) float)
  )
  )

;Especificacion Sintactica(Gramatica)

(define gramatica
  '((program (global_def expression) C-VID_program)
    
    (expression (number) numb)
    (expression (float) float)
    (expression ("#\'" letter "#\'") char)
    (expression ("#\"" identifier "#\"") string)
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
    (expression ("var" "(" identifier "=" expression (arbno "," identifier "=" expression) ")" "in") var_def)
    (expression ("cons" "(" identifier "=" expression (arbno "," identifier "=" expression) ")" "in") cons_def)
    (expression ("rec" "(" identifier "=" expression(arbno "," identifier "=" expression) ")" "in") rec_def)
    (expression ("global" "(" identifier "=" expression (arbno "," identifier "=" expression) ")" "in") global_def)
    (expression ("unic" "(" identifier "=" expression (arbno "," identifier "=" expression) ")" "in") unic_def)

    ;;Estructuras de Control
    (expression ("sequence" expression (arbno ";" expression)) exp_seq)
    (expression ("if" exp_expression_bool "then" expression "else" expression) exp_if)
    (expression ("cond" "{" "[" expression expression "]" (arbno ";" "[" expression expression "]")) exp_cond)
    (expression ("while" expression_bool "do" expression) exp_while)
    (expression ("for" identifier "=" expression "do" expression))
    )
  )