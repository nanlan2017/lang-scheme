(module lang (lib "eopl.ss" "eopl")  

  (require "drscheme-init.scm")
  
  (provide (all-defined-out))

  ;;;;;;;;;;;;;;;; grammatical specification ;;;;;;;;;;;;;;;;
  
  ;; 词法规则
  (define the-lexical-spec
    '(
      ;; 空白
      (whitespace (whitespace) skip)
      ;; 注释
      (comment ("%" (arbno (not #\newline))) skip)
      ;; 标识符
      (identifier
       (letter (arbno (or letter digit "_" "-" "?")))
       symbol)
      ;; 数字
      (number (digit (arbno digit)) number)
      (number ("-" digit (arbno digit)) number)
      ))

  
  ;; 语法规则 （BNF文法)
  
  ;; 其中： identifier是终结符token
  ;;        program, expression是非终结符
  
  ;;;  于此处定义了BNF文法, 并定义了语法树的节点类型
  (define the-grammar
    '(
      (program (expression)                   
        a-program)

      (expression (number)
        const-exp)
      
      (expression
        ("-" "(" expression "," expression ")")
        diff-exp)
      
      (expression
       ("zero?" "(" expression ")")
       zero?-exp)

      (expression
       ("if" expression "then" expression "else" expression)
       if-exp)

      (expression (identifier)
       var-exp)

      (expression
       ("let" identifier "=" expression "in" expression)
       let-exp)   

      ))
  
  ;;;;;;;;;;;;;;;; sllgen boilerplate ;;;;;;;;;;;;;;;;
  ; sllgen:make-define-datatypes
  (sllgen:make-define-datatypes the-lexical-spec the-grammar)
  ; sllgen:list-define-datatypes
  (define show-the-datatypes
    (lambda () (sllgen:list-define-datatypes the-lexical-spec the-grammar)))
  
  ; sllgen:make-string-parser
  (define scan&parse
    (sllgen:make-string-parser the-lexical-spec the-grammar))
  ; sllgen:make-string-scanner
  (define just-scan
    (sllgen:make-string-scanner the-lexical-spec the-grammar))
  
  )
