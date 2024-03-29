
(require "cas.lisp")
;; Compilateur LISP -> ASM

(defun compile (exp &optional (env ()) (fenv ())  (nomf ()))
  (let ((arg (if (atom exp) () (cdr exp))))
    (cond
      ((atom exp) (compile-literral exp env fenv nomf))
      ((member (car exp) '(+ - * /)) (compile-operator exp env fenv nomf))
      ((equal (car exp) 'if) (compile-if (cdr exp) env fenv nomf))
      ((equal (car exp) 'cond) (compile-cond (cdr exp) env fenv nomf))
      ((equal (car exp) 'loop) (compile-loop (cdr exp) env fenv nomf))
      ((equal (car exp) 'let) (compile-let (cdr exp) env fenv nomf))
      ((equal (car exp) 'setf) (compile-setf (cdr exp) env fenv nomf))
      ((equal (car exp) 'defun) (compile-defun (cdr exp) env fenv nomf))
      ((equal (car exp) 'halt) (append `((HALT))))
      ((equal (car exp) 'nop) (append `((NOP))))
      ((equal (car exp) 'and) (compile-and arg (gensym "finAnd") env fenv nomf))
      ((equal (car exp) 'or) (compile-or arg (gensym "finOr") env fenv nomf))
      ((member (car exp) '(< > = >= <=)) (compile-comp exp env fenv nomf))
      ((equal (car exp) 'progn) (compile-progn arg env fenv nomf))
      (t  (compile-appel exp env fenv nomf))
    )
  )
)