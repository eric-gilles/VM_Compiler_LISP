;; Compilation if et cond

;; IF
(defun compile-if (exp env fenv nomf)
  (let ((sinon (gensym "sinon"))(finSi (gensym "finSi")))
    (append (compile (car exp) env fenv nomf)  
      '((CMP :R0 (:LIT 0)))
      `((JEQ ,sinon))
      (compile (cadr exp) env fenv nomf) 
      `((JMP ,finSi))
      `((LABEL ,sinon))  
      (compile (caddr exp) env fenv nomf)
      `((LABEL ,finSi))
    )
  )
)

;; COND
(defun compile-cond (exp env fenv nomf)
  (if (null exp)
    ()
    (let ((fincond (gensym "fincond")))
      (append
        (compile (caar exp) env fenv nomf)
        `((CMP :R1 (:LIT 0)))
        `((JEQ ,fincond))
        (compile (cdar exp) env fenv nomf)
        `((JMP ,fincond))
        `((LABEL ,fincond))
        (compile (cdr exp) env fenv nomf)
      )
    )
  )
)