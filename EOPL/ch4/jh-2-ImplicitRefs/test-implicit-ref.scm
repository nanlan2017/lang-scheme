(module test-implicit-ref (lib "eopl.ss" "eopl")
  (provide (all-defined-out))

  (require "lang.scm")
  (require "data-structures.scm")
  (require "interp.scm")
  ;;========================================================

  (define src-2
    "let g = let count = 0
             in proc (dummy)
                   begin
                       set count = -(count,-9);
                       count
                   end
     in let a = (g 11)
        in let b = (g 11)
           in -(a,b)"
    )

  ;; 测试 call-by-value
  (define src-3
    "let dec = proc (x) -(x,1)
     in  let y = 10
         in begin
              (dec y) ;
              y                  % 此处y值为10，则call-by-value ; 为9, 则call-by-reference
            end
     ")



  )