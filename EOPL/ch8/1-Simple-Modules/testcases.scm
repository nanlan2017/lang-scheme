(module testcases (lib "eopl.ss" "eopl")
  (require "lang.scm")
  (require "interp.scm")
  (require "type-checker.scm")

  ;====================================================================

  (define src-0
"
  module m1
    interface
      [
        a : int
        b : int
        c : int
      ]
    body
      [
        a = 33
        x = -(a,1)    % = 32
        b = -(a,x)    % = 1
        % c = -(x,b)   % = 31
      ]

  let a = 10
  in -(-(from m1 take a, from m1 take b),a)     %%%%%%% m1::a - m1::b - 10
"
    )
  )
