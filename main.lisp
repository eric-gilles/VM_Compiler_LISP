;; (require "compilation/compiler.lisp")

(require "VM/vm.lisp")
(require "VM/instructions.lisp")
(require "test/compilation/test.lisp")




(let ((vm (vm_make)))
  (testFact vm 12 479001600)
  
)

(let ((vm (vm_make)))
  (testFibo vm 10 55)
)

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

