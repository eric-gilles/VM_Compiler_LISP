;; fact.lisp
(defun fact (n) 
  (if (= n 1) 
    1 
    (* n (fact (- n 1)))
  )
) 

;; Test factorielle de 10
(fact 10)