(module data-structures (lib "eopl.ss" "eopl")

  (provide (all-defined-out))

;*****************************************************************************************************************  

  (define-datatype expval expval?
    (num-val
      (value number?))
    (bool-val
      (boolean boolean?)))

;;; extractors:

  ;; expval->num : ExpVal -> Int
  ;; Page: 70
  (define expval->num
    (lambda (v)
      (cases expval v
	(num-val (num) num)
	(else (expval-extractor-error 'num v)))))

  ;; expval->bool : ExpVal -> Bool
  ;; Page: 70
  (define expval->bool
    (lambda (v)
      (cases expval v
	(bool-val (bool) bool)
	(else (expval-extractor-error 'bool v)))))

  (define expval-extractor-error
    (lambda (variant value)
      (eopl:error 'expval-extractors
                  "Looking for a ~s, found ~s" variant value)))

)
