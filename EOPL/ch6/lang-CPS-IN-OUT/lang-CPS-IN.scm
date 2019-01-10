(module lang-CPS-IN (lib "eopl.ss" "eopl")
  (provide (all-defined-out))


  (define the-lexical-spec
    '((whitespace (whitespace) skip)
      (comment ("%" (arbno (not #\newline))) skip)
      (identifier (letter (arbno (or letter digit "_" "-" "?"))) symbol)
      (number (digit (arbno digit)) number)
      (number ("-" digit (arbno digit)) number)
      ))

  (define the-grammar
    '((program (expression) a-program)
      ; ----------- expression -------------------
      (expression (number) const-exp)
      (expression (identifier) var-exp)
      (expression ("-" "(" expression "," expression ")") diff-exp)      
      (expression ("+" "(" (separated-list expression ",") ")") sum-exp)
      (expression ("zero?" "(" expression ")") zero?-exp)
      (expression ("if" expression "then" expression "else" expression) if-exp)
      (expression ("let" identifier "=" expression "in" expression) let-exp)
      ; multi-letrec
      (expression ("letrec" (arbno identifier "(" (arbno identifier) ")" "=" expression) "in"expression) letrec-exp)
      ; multi-args-procedure
      (expression ("proc" "(" (arbno identifier) ")" expression) proc-exp)
      (expression ("(" expression (arbno expression) ")") call-exp)      
      ))

  ; ====================================================================
  
  (sllgen:make-define-datatypes the-lexical-spec the-grammar)
  
  (define show-the-datatypes
    (lambda () (sllgen:list-define-datatypes the-lexical-spec the-grammar)))
  
  (define scan&parse
    (sllgen:make-string-parser the-lexical-spec the-grammar))
  
  (define just-scan
    (sllgen:make-string-scanner the-lexical-spec the-grammar))
  
  )
