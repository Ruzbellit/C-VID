#lang eopl
;;Ruzbellit Rossy Romero Ramirez - 1925456
;;Christian Villanueva Paez - 1924546


;******************************************************************************************
;;;;; Interpretador para el lenguaje C-VID

;; La definición BNF para las expresiones del lenguaje:
;;
;;  <program>       ::= <globals expression>
;;  <globals>       ::= global( {<identifier> = expression}*(,) )
;;                      <global-env (ids rands)>
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
;;                      compare(<expression> <bool-prim> <expression>)
;;                  ::= <bin-bool-exp>
;;                      <bool-op> (<exp-bool>, <exp-bool>)
;;                  ::= <neg-bool-exp>
;;                      <neg-bool> <exp-bool>
;;  <bool-prim>  ::= > | >= | < | <= | == | <>
;;  <bool-op>       ::= AND | OR | XOR 
;;  <simple-bool-op>::= NOT
;;  <arith-prim>    ::= + | - | * | / | % | ++ | --
;;  <for-type>      ::= to | downto

;******************************************************************************************

;Especificación Léxica

(define scanner-spec-simple-interpreter
'((white-sp
   (whitespace) skip)
  (comment
   ("//" (arbno (not #\newline))) skip)
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
  '(
    (program (globals expression) c-vid-program)
    (globals ("global" "(" (separated-list identifier "=" expression ",") ")") global-env)
    
    (expression (identifier) var-exp)
    (expression ("set" identifier "=" expression) set-exp)

    ;;Definiciones
    (expression ("var" (separated-list identifier "=" expression ",") "in" expression) var-def)
    (expression ("const" (separated-list identifier "=" expression ",") "in" expression) const-def)
    (expression ("rec" (arbno identifier "(" (separated-list identifier ",") ")" "=" expression) "in" expression) rec-def)
    (expression ("unic" (separated-list identifier "=" expression ",") "in" expression) unic-def)
    
    ;;Datos
    (expression (int) int-exp)
    (expression (float) float-exp)
    (expression (char) char-exp)
    (expression (str) string-exp)
    (expression (exp-bool) bool-exp)
    (expression ("x8(" (arbno int) ")") octal-exp)
    (expression ("print" "(" expression ")") print-exp)

    ;;Constructores de Datos Predefinidos
    (expression ("'(" (arbno expression (arbno "," expression)) ")") list-exp)
    (expression ("vector" "[" expression (arbno "," expression ) "]") vector-exp)
    (expression ("{" identifier "=" expression (arbno ";" identifier "=" expression) "}") record-exp)

    ;;Expresiones booleanas
    (exp-bool ("true") true-exp)
    (exp-bool ("false") false-exp)
    (exp-bool ("compare(" expression bool-prim expression ")") compare-exp)
    (exp-bool (bool-op "(" exp-bool "," exp-bool ")") bin-bool-exp)
    (exp-bool (simple-bool-op exp-bool) neg-bool-exp)
    (bool-prim ("<") less-than-prim)
    (bool-prim (">") greater-than-prim)
    (bool-prim ("<=") less-equal-prim)
    (bool-prim (">=") greater-equal-prim)
    (bool-prim ("==") equal-prim)
    (bool-prim ("<>") not-equal-prim)
    (bool-op ("AND") and-prim)
    (bool-op ("OR") or-prim)
    (bool-op ("XOR") xor-prim)
    (simple-bool-op ("NOT") neg-prim)

    ;;Estructuras de Control
    (expression ("sequence" expression (arbno ";" expression) "end") seq-exp)
    (expression ("if" exp-bool "then" expression "else" expression "end") if-exp)
    (expression ("cond" (arbno "[" expression expression "]") "else" expression) cond-exp)
    (expression ("while" exp-bool "do" expression "done") while-exp)
    (expression ("for" identifier "=" expression for-type expression "do" expression "done") for-exp)
    (for-type ("to") for-to)
    (for-type ("downto") for-downto)
    
    ;;Primitivas aritmeticas para enteros
    (expression ("(" expression arith-prim expression ")") arith-exp)
    (expression (simple-arith-prim expression) simple-arith-exp)
    (arith-prim ("+") add-prim)
    (arith-prim ("-") subtract-prim)
    (arith-prim ("*") mult-prim)
    (arith-prim ("/") div-prim)
    (arith-prim ("%") remainder-prim)
    (simple-arith-prim ("++") incr-prim)
    (simple-arith-prim ("--") decr-prim)

    ;;Primitivas aritmeticas para octales
    (expression ("x8-op" "(" expression arith-prim-octal expression ")") arith-octal-exp)
    (expression (simple-arith-prim-octal expression) simple-arith-octal-exp)
    (arith-prim-octal ("+") add-prim-octal)
    (arith-prim-octal ("-") subtract-prim-octal)
    (arith-prim-octal ("*") mult-prim-octal)
    (simple-arith-prim-octal ("+") incr-prim-octal)
    (simple-arith-prim-octal ("-") decr-prim-octal)
    
    ;;Primitivas sobre cadenas
    (expression ("length" "(" expression ")") length-exp)
    (expression ("concat" "(" expression "," expression ")") concat-exp)

    ;;Primitivas sobre listas
    (expression ("is-empty-list?" expression) is-empty-list)
    (expression ("empty-list") empty-list)
    (expression ("create-list" "(" (arbno expression (arbno "," expression)) ")") create-list)
    (expression ("is-list?" expression) is-list)
    (expression ("head" expression) head-list)
    (expression ("bottom" expression) bottom-list)
    (expression ("append" "(" expression expression ")") append-list)

    ;;Primitivas sobre vectores
    (expression ("is-vector?" expression) is-vector)
    (expression ("create-vector" "[" expression (arbno "," expression ) "]") create-vector)
    (expression ("ref-vector" expression expression) ref-vector) ;;ref-vector (x, y) x->indice  y->vector
    (expression ("set-vector" expression expression expression) set-vector) ;;set-vector (x, y, z) x->valor y->indice z->

    ;;Primitivas sobre registros
    (expression ("is-record?" expression) is-record)
    (expression ("create-record" "{" identifier "=" expression (arbno ";" identifier "=" expression) "}") create-record)
    (expression ("ref-record" expression expression) ref-record) ;;ref-record (x, y) x-> y->registro
    (expression ("set-registro" expression expression expression) set-registro) ;;set-registro (x, y, z) x->valor y->clave z->registro
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

;El Interpretador (FrontEnd + Evaluación + señal para lectura )

(define interpretador
  (sllgen:make-rep-loop  "--> "
    (lambda (pgm) (eval-program  pgm))
    (sllgen:make-stream-parser
      scanner-spec-simple-interpreter
      grammar-simple-interpreter)))

;*******************************************************************************************
;El Interprete

;eval-program: <programa> -> numero
; función que evalúa un programa teniendo en cuenta un ambiente dado (se inicializa dentro del programa)

(define eval-program
  (lambda (pgm)
    (cases program pgm
      (c-vid-program (globals expression)
                 (eval-expression expression (init-globals globals))))))

; inicializa el ambiente de globals
(define init-globals
   (lambda (glb)
    (cases globals glb
       (global-env (ids rands)
                   (extend-env ids (eval-rands rands empty-env) (empty-env))))))

(define eval-expression
  (lambda (exp env)
    (cases expression exp
      (int-exp (datum) datum)
      (float-exp (datum) datum)
      (string-exp (datum) datum)
      (var-exp (id) (apply-env env id))
      (arith-exp (rand1 prim rand2)
                   (apply-arith-prim (eval-expression rand1 env) prim (eval-expression rand2 env)))
      (var-def (ids rands body)
               (let ((args (eval-rands rands env)))
                 (eval-expression body
                                  (extend-env ids args env))))
      (set-exp (id rhs-exp)
               (begin
                 (setref!
                  (apply-env-ref env id)
                  (eval-expression rhs-exp env))
                 1))
      (bool-exp (datum)
                (eval-boolean-exp datum env))
      (print-exp (datum)
                 (eopl:printf ">> ~a~%" (eval-expression datum env)))
      (else 0)
      )))

; funciones auxiliares para aplicar eval-expression a cada elemento de una 
; lista de operandos (expresiones)
(define eval-rands
  (lambda (rands env)
    (map (lambda (x) (eval-rand x env)) rands)))

(define eval-rand
  (lambda (rand env)
    (eval-expression rand env)))

;eval-boolean-exp: <boolean-exp> -> boolean
(define eval-boolean-exp
  (lambda (exp env)
    (cases exp-bool exp
      (true-exp () #t)
      (false-exp () #f)
      (compare-exp (rand1 prim rand2)
                   (apply-bool-primitive prim (eval-expression rand1 env) (eval-expression rand2 env)))
      (bin-bool-exp (bool-op rand1 rand2)
                    (apply-bin-bool-primitive bool-op (eval-boolean-exp rand1 env) (eval-boolean-exp rand2 env)))
      (neg-bool-exp (bool-neg datum) (not (eval-boolean-exp datum env)))
      )))

;apply-arith-primitive: numero * <arith-prim> * numero -> numero
(define apply-arith-prim
  (lambda (rand1 prim rand2)
    (cases arith-prim prim
      (add-prim () (+ rand1 rand2))
      (subtract-prim () (- rand1 rand2))
      (mult-prim () (* rand1 rand2))
      (div-prim () (/ rand1 rand2))
      (remainder-prim () (remainder rand1 rand2)))))

;apply-bool-primitive: <bool-prim> * <bool> * <bool> -> <bool>
(define apply-bool-primitive
  (lambda (prim rand1 rand2)
    (cases bool-prim prim
      (less-than-prim () (< rand1 rand2))
      (greater-than-prim () (> rand1 rand2))
      (less-equal-prim () (<= rand1 rand2))
      (greater-equal-prim () (>= rand1 rand2))
      (equal-prim () (equal? rand1 rand2))
      (not-equal-prim () (not (equal? rand1 rand2))))))

;apply-bin-bool-primitive: <bool-op> * <bool> * <bool> -> <bool>
(define apply-bin-bool-primitive
  (lambda (prim rand1 rand2)
    (cases bool-op prim
      (and-prim () (and rand1 rand2))
      (or-prim () (or rand1 rand2))
      (xor-prim () (or (and rand1 (not rand2)) (and (not rand1) rand2))))))

;*******************************************************************************************
;Procedimientos
(define-datatype procval procval?
  (closure
   (ids (list-of symbol?))
   (body expression?)
   (env environment?)))

;apply-procedure: evalua el cuerpo de un procedimientos en el ambiente extendido correspondiente
(define apply-procedure
  (lambda (proc args)
    (cases procval proc
      (closure (ids body env)
               (eval-expression body (extend-env ids args env))))))
;*******************************************************************************************
;Ambientes

;definición del tipo de dato ambiente
(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record
   (syms (list-of symbol?))
   (vec vector?)
   (env environment?)))

(define scheme-value? (lambda (v) #t))

;empty-env:      -> enviroment
;función que crea un ambiente vacío
(define empty-env
  (lambda ()
    (empty-env-record)))       ;llamado al constructor de ambiente vacío


;extend-env: <list-of symbols> <list-of numbers> enviroment -> enviroment
;función que crea un ambiente extendido
(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms (list->vector vals) env)))

;extend-env-recursively: <list-of symbols> <list-of <list-of symbols>> <list-of expressions> environment -> environment
;función que crea un ambiente extendido para procedimientos recursivos
(define extend-env-recursively
  (lambda (proc-names idss bodies old-env)
    (let ((len (length proc-names)))
      (let ((vec (make-vector len)))
        (let ((env (extended-env-record proc-names vec old-env)))
          (for-each
            (lambda (pos ids body)
              (vector-set! vec pos (closure ids body env)))
            (iota len) idss bodies)
          env)))))

;iota: number -> list
;función que retorna una lista de los números desde 0 hasta end
(define iota
  (lambda (end)
    (let loop ((next 0))
      (if (>= next end) '()
        (cons next (loop (+ 1 next)))))))

;(define iota
;  (lambda (end)
;    (iota-aux 0 end)))
;
;(define iota-aux
;  (lambda (ini fin)
;    (if (>= ini fin)
;        ()
;        (cons ini (iota-aux (+ 1 ini) fin)))))

;función que busca un símbolo en un ambiente
(define apply-env
  (lambda (env sym)
    (deref (apply-env-ref env sym))))
     ;(apply-env-ref env sym)))
    ;env))
(define apply-env-ref
  (lambda (env sym)
    (cases environment env
      (empty-env-record ()
                        (eopl:error 'apply-env-ref "No binding for ~s" sym))
      (extended-env-record (syms vals env)
                           (let ((pos (rib-find-position sym syms)))
                             (if (number? pos)
                                 (a-ref pos vals)
                                 (apply-env-ref env sym)))))))


;*******************************************************************************************
;Referencias

(define-datatype reference reference?
  (a-ref (position integer?)
         (vec vector?)))

(define deref
  (lambda (ref)
    (primitive-deref ref)))

(define primitive-deref
  (lambda (ref)
    (cases reference ref
      (a-ref (pos vec)
             (vector-ref vec pos)))))

(define setref!
  (lambda (ref val)
    (primitive-setref! ref val)))

(define primitive-setref!
  (lambda (ref val)
    (cases reference ref
      (a-ref (pos vec)
             (vector-set! vec pos val)))))


;****************************************************************************************
;Funciones Auxiliares

; funciones auxiliares para encontrar la posición de un símbolo
; en la lista de símbolos de un ambiente

(define rib-find-position
  (lambda (sym los)
    (list-find-position sym los)))

(define list-find-position
  (lambda (sym los)
    (list-index (lambda (sym1) (eqv? sym1 sym)) los)))

(define list-index
  (lambda (pred ls)
    (cond
      ((null? ls) #f)
      ((pred (car ls)) 0)
      (else (let ((list-index-r (list-index pred (cdr ls))))
              (if (number? list-index-r)
                (+ list-index-r 1)
                #f))))))

;******************************************************************************************

;;PRUEBAS
(scan&parse "global(x=5, y=2) (x*y)")  ;; retorna 10
(scan&parse "global(nombre=\"Victor\") nombre")
(scan&parse "global(nombre=\"Victor\") set nombre=\"Sarah\"")

;;Definiciones
(scan&parse "global(x=5, y=2) var x=10, z=0 in (x*y)") ;; retorna 20
(scan&parse "global() var x=1, y=2 in (x+y)") ;; retorna 3
(scan&parse "global() const x=1, y=2 in (x+y)")
(scan&parse "global() unic x=1, y=2 in (x+y)")
(scan&parse "global() var x=1, y=2 in  sequence set x = 10; (x * y) end")

;;Datos
(scan&parse "global() 0")
(scan&parse "global() 1")
(scan&parse "global() -1")
(scan&parse "global() 3.14")
(scan&parse "global() -1.5")
(scan&parse "global() 'R'")
(scan&parse "global() \"hola mundo 7\"")
(scan&parse "global() x8(1 5 7)")

;;Expresiones booleanas
(scan&parse "global() true")  ; -> #t
(scan&parse "global() false")  ; -> #f
(scan&parse "global() AND (true, false)") ; -> #f
(scan&parse "global() AND (true, true)") ; -> #t
(scan&parse "global() NOT true")  ; -> #f
(scan&parse "global() XOR (false, false)") ; -> #f
(scan&parse "global() XOR (true, true)") ; -> #f
(scan&parse "global() XOR (true, false)") ; -> #t
(scan&parse "global() XOR (false, true)") ; -> #t
(scan&parse "global() NOT false")  ; -> #t
(scan&parse "global() compare(5>2)") ; -> #t
(scan&parse "global() compare(5<2)") ; -> #f
(scan&parse "global() compare(5>=2)") ; -> #t
(scan&parse "global() compare(5<=2)") ; -> #f
(scan&parse "global() compare(5<>2)") ; -> #t
(scan&parse "global() compare(5==2)") ; -> #f
(scan&parse "global() compare(5==5)") ; -> #t


;;Constructores de datos predefinidos
(scan&parse "global() '(1, true, 'X')")
(scan&parse "global() vector[10, true, 'X']")
(scan&parse "global() {x = 5; y = 7}")

;;Estructuras de control
(scan&parse "global() sequence 123; \"Hola\" ; 'R'; -5; true end")
(scan&parse "global() if compare(5>2) then 10 else 0 end")
(scan&parse "global() cond [ compare(5>0) true] else 0")
(scan&parse "global() cond [ compare(5>0) \"paso FLP\"] [compare(5>3) \"arrastrado pero lo paso\"] else 0")
(scan&parse "global() while true do (i+1) done")
(scan&parse "global() for x = 0 to 5 do (y+1) done")
(scan&parse "global() for x = 10 downto 5 do (y+1) done")

;;Primitivas aritmeticas para enteros
(scan&parse "global() (12 + 7)")
(scan&parse "global() (7 - 27)")
(scan&parse "global() ((1 + 5) * 3)")
(scan&parse "global() (15 / 3)")
(scan&parse "global() (100 % 50)")
(scan&parse "global() ++ 0")
(scan&parse "global() -- 13")

;;Primitivas aritmeticas para octales
(scan&parse "global() x8-op(x8(1 5 7) + x8(8 0 1))")
(scan&parse "global() x8-op(x8(8 8 8) - x8(8 8 0))")
(scan&parse "global() x8-op(x8(0 0 0) * x8(1 1 1))")
(scan&parse "global() + x8(4 5 7)")
(scan&parse "global() + x8(8 0 1)")

;;Primitivas sobre cadenas
(scan&parse "global() length(\"Hola\")")
(scan&parse "global(nombre=\"Carlos\") concat(nombre, \"Andrade\")")

;;Primitivas sobre listas
;;arreglar implementacion de las listas

;;Primitivas sobre vectores
(scan&parse "global() is-vector? vector[10, false, 'R']")
(scan&parse "global() create-vector[true, 11, 2]")
(scan&parse "global() ref-vector 1 vector[0, 1, 2, 3]")
(scan&parse "global() set-vector true 2 vector[0, 1, 2, 3, 4, 5]")

;;;Primitivas sobre registros
(scan&parse "global() is-record? {nombre=\"Emily\"; apellido=\"Cardona\"}")
(scan&parse "global() create-record {celular=315496 ; fijo=3654}")
(scan&parse "global() ref-record nombre {nombre=\"Emily\"; apellido=\"Cardona\"}")
(scan&parse "global() set-registro 20 edad {edad=0}")

;;Imprimir por salida estandar
(scan&parse "global() var x=1, y=2 in print((5+y))") ;; imprime 7

(interpretador)