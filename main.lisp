;; (require "compilation/compiler.lisp")

(require "src/VM/vm.lisp")
(require "src/VM/instructions.lisp")
(require "src/test/compilation/test.lisp")



;; Appel au Test de Factorielle de 12
(let ((vm (vm_make)))
  (testFact vm 12 479001600)
)

;; Appel au Test de Fibonacci de 12
(let ((vm (vm_make)))
  (testFibo vm 10 55)
)

;; Affichage de la Compilation de la fonction Fibonacci
(format t "~%Affichage de la Compilation de la fonction Fibonacci :")
(print (compile '(defun fibo (n) 
             (if (= 0 n) 
               0 
               (if (= 1 n) 
                 1 
                 (+ (fibo (- n 1))(fibo (- n 2)))
               )
             )
          )
        )
)

