(module lang (lib "eopl.ss" "eopl")
  (provide (all-defined-out))
  
  (define the-lexical-spec
    '((whitespace (whitespace) skip)      
      (comment ("%" (arbno (not #\newline)))skip)
      (identifier (letter (arbno (or letter digit "_" "-" "?"))) symbol)
      (number (digit (arbno digit)) number)
      (number ("-" digit (arbno digit)) number)
      ))

  (define the-grammar
    '((program (expression) a-program)
      ;------------------------- type -------------------------------------
      (type ("int") int-type)      
      (type ("bool") bool-type)      
      (type ("(" type "->" type ")") proc-type)
      ;------------------------- Expression -------------------------------
      (expression (number) const-exp)
      (expression (identifier) var-exp)
      (expression ("-" "(" expression "," expression ")")  diff-exp)
      (expression ("zero?" "(" expression ")") zero?-exp)
      (expression ("if" expression "then" expression "else" expression)  if-exp)
      (expression ("let" identifier "=" expression "in" expression) let-exp)
      ; single-argument procedure
      (expression ("proc" "(" identifier ":" type ")" expression) proc-exp)
      (expression ("(" expression expression ")") call-exp)
      ; letrec int fact(x:int) = ...   // fact::int->int
      (expression ("letrec" type identifier "(" identifier ":" type ")" "=" expression "in" expression) letrec-exp)      
      ))
      
  ; ================================================================== SLLGEN
  (sllgen:make-define-datatypes the-lexical-spec the-grammar)  
  (define show-the-datatypes
    (lambda () (sllgen:list-define-datatypes the-lexical-spec the-grammar)))
  (define just-scan
    (sllgen:make-string-scanner the-lexical-spec the-grammar))  
  (define scan&parse
    (sllgen:make-string-parser the-lexical-spec the-grammar)) 
  
  )
