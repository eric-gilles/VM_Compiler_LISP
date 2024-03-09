;; Compilation des fonctions

(defun compile-defun (exp env fenv nomf)
  (let ((nivem (assoc nomf fenv)) (out (gensym "notCalled")))
    (append
      `((JMP ,out))
      `((LABEL ,(car exp)))
      (compile-progn (cddr exp)
        (param-env (cadr exp) env 1 (if nivem (+ 1 (cadr nivem)) 0)) 
        (fun-env (list exp) fenv (if nivem (+ 1 (cadr nivem)) 0)) 
        (car exp)
      )
      '((RTN))
      `((LABEL ,out))
    )
  )
)

(defun compile-progn (exp env fenv nomf)
  (if (null exp) 
    ()
    (append 
       (compile (car exp) env fenv nomf)
       (compile-progn (cdr exp) env fenv nomf)
    )
  )
)


(defun param-env (exp env dep nivem)   
  (if (atom exp) 
    env
    (param-env (cdr exp) (cons (cons (car exp) `(LOC ,(- 0 dep) ,nivem)) env) (+ 1 dep) nivem)
  )
)


(defun fun-env (exp fenv nivem)
  (if (atom exp) 
    fenv
    (fun-env (cdr exp) (cons `(,(caar exp) ,nivem) fenv) nivem)
  )
)


(defun compile-appel (exp env fenv nomf)
  (let ((n (length (cdr exp))) (nivem (assoc (car exp) fenv)))
    (append (compile-param (cdr exp) env fenv nomf)
      `((PUSH (:LIT ,n)))
      `((MOVE :FP :R1))
      `((MOVE :SP :FP))
      `((MOVE :SP :R2))
      `((SUB  (:LIT ,n) :R2))
      `((SUB  (:LIT 1) :R2));;old SP
      `((PUSH :R2)) 
      `((PUSH :R1))
      '((INCR :SP))
      ;; (if nivem  `((PUSH (:LIT ,(cadr nivem))))  `((PUSH (:LIT ,0))))
      `((JSR ,(car exp)))
    )
  )
)



(defun compile-param (exp env fenv nomf)
(if (atom exp) 
  ()
  (append (compile (car exp) env fenv nomf)
    `((PUSH :R0))
    (compile-param (cdr exp) env fenv nomf))
  )
)