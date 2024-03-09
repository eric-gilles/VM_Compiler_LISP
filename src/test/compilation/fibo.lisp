;; fibo.lisp
;; Code de la fonction Fibonacci
(defun fibo (n) 
  (if (= 0 n) 
    0 
    (if (= 1 n) 
      1 
      (+ (fibo (- n 1))(fibo (- n 2)))
    )
  )
)

;; Test fibo de 10
(fibo 10)