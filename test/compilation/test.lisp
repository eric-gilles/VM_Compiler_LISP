(defun testFact (vm n r)
    (vm_CL vm '(defun fact (n) 
                (if (<= n 1) 
                  1 
                  (* n (fact (- n 1)))
                )
              )
    )
    (vm_CL vm `(fact ,n))
    (vm_CL vm '(halt))
    (vm_exec vm)
    ;; (= r (vm_get_reg vm :R0))

    (format t "Factorielle =>~% Rang: ~a~% Résulat: ~a~% Résulat attendu: ~a~%" n r (vm_get_reg vm :R0))
)


         
(defun testFibo (vm n r)
  (vm_CL vm '(defun fibo (n) 
               (if (= 0 n) 
                 0 
                 (if (= 1 n) 
                   1 
                   (+ (fibo (- n 1))(fibo (- n 2)))
                 )
               )
            )
  )
  (vm_CL vm `(fibo ,n))
  (vm_CL vm '(halt))
  (vm_exec vm)
  (format t "Fibonacci =>~% Rang: ~a~% Résulat: ~a~% Résulat attendu: ~a~%" n r (vm_get_reg vm :R0))
)