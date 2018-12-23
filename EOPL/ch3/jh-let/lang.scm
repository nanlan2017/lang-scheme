(module lang (lib "eopl.ss" "eopl")
  (provide (all-defined-out))
  ;;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  (define the-lexical-spec
    '((whitespace (whitespace)
                  skip)
      
      (comment ("%" (arbno (not #\newline)))
               skip)
      
      (identifier (letter (arbno (or letter digit "_" "-" "?")))
                  symbol)
      
      (number (digit (arbno digit))
              number)
      (number ("-" digit (arbno digit))
              number)
      ))

  ;; 6种表达式： const-exp , zero? , diff-exp ,  if-exp , var-exp, let-exp
  (define the-grammar
    '(
      ;; Program
      (program (expression)
               a-program)
      ;; Expression
      (expression (number)   ; 4
                  const-exp)
      (expression (identifier)  ; x
                  var-exp)
      (expression ("-" "(" expression "," expression ")") ; -(e1,e2)
                  diff-exp)
      (expression ("zero?" "(" expression ")")            ; zero? (e1)
                  zero?-exp)
      (expression ("if" expression "then" expression "else" expression)  ; if e1 then e2 else e3
                  if-exp)
      (expression ("let" identifier "=" expression "in" expression)  ; let i = e1 in e2
                  let-exp)
      ))
      
      

  
  ;;======================================================== SLLGEN
  (sllgen:make-define-datatypes the-lexical-spec the-grammar)
  
  (define show-the-datatypes
    (lambda () (sllgen:list-define-datatypes the-lexical-spec the-grammar)))
  
  (define scan&parse
    (sllgen:make-string-parser the-lexical-spec the-grammar))
  
  (define just-scan
    (sllgen:make-string-scanner the-lexical-spec the-grammar))
  )
