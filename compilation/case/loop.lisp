;; Compilation des boucles
(require "conditions.lisp")
(defun compile-loop (exp env fenv nomf)
  (cond
    ((equal (car exp) 'while) (compile-while (cdr exp) env fenv nomf))
    ((equal (car exp) 'for) (compile-for (cdr exp) env fenv nomf))
    (t (error "Boucle non reconnue"))
  )
)

;; WHILE
(defun compile-while (exp env fenv nomf)
  (let ((finwhile (gensym "finwhile")) (while (gensym "while")))
    (append
      `((LABEL ,while))
      (compile (car exp) env fenv nomf)
      `((CMP :R0 (:LIT 0)))  ;; Si :R0 == 0, alors on sort du while
      `((JEQ ,finwhile))
      (compile (caddr exp) env fenv nomf)
      `((JMP ,while))
      `((LABEL ,finwhile))
    )
  )
)

;; FOR
(defun compile-for (exp env fenv nomf)
  (let ((finfor (gensym "finfor")) (for (gensym "for")))
    (append
      `((MOVE ::R0 ,(cadr exp)))
      `((MOVE :R1 ,(car exp)))
      `((MOVE :R2 ,(caddr exp)))
      `((LABEL ,for))
      `((CMP :R1 :R2)) ;; si R1 == R2, on sort du for
      `((JGT ,finfor))
      (compile (cddddr exp) env fenv nomf)
      `((INCR :R1))
      `((JMP ,for))
      `((LABEL ,finfor))
    )
  )
)