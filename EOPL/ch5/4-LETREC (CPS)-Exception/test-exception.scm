(module test-exception (lib "eopl.ss" "eopl")
  (provide (all-defined-out))  
  (require "lang.scm")
  (require "data-structures.scm")
  (require "interp.scm")
  ; ==========================================================

  (define src-0
  "raise 1"
    )

  (define src-1
"
try
  let y = 4
  in raise 101
catch(e)
  e
")

  (define src-2
"
try
  let y = 4
  in raise -(101,y)
catch(e)
  e
")


  )
