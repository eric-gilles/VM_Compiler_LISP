;; Compilation Variables (setf, let, ...)

(defun compile-setf (exp env fenv nomf)
  (if (null exp) ()
    (append 
     (compile (cadr exp) env fenv nomf)
      (let ((var (assoc (car exp) env)))
        (if var
          (progn
            `((MOVE :FP :R1))
            `((ADD (:LIT ,(caddr var)) :R1))
            `((STORE :R0 :R1))
          )
        )
      )
      (compile-setf (cddr exp) env fenv nomf)
    )
  )
)

(defun iterative-max-dep (lst)
  (let((max 0))
    (if (not (null lst))
      (loop for e in lst
        do (if (< max  (caddr e))
          (setf max (caddr e))
        )
      )
    )
    (if (= max 0)
        nil
        max
    )
  )
)


(defun compile-let (exp env fenv nomf)
  (let* ((nivem (assoc nomf fenv))
         (max-dep-value (iterative-max-dep env)))
    (append 
      (compile-local (car exp) env fenv nomf)
      (if (null max-dep-value) 
          (compile (cadr exp) (local-env (car exp) env 4 (cadr nivem)) fenv nomf)
          (compile (cadr exp) (local-env (car exp) env (+ 1 max-dep-value) (cadr nivem)) fenv nomf)
      )
      (depile-local (car exp) env fenv nomf)
    )
  )
)


(defun compile-local (exp env fenv nomf)
  (if (null exp)
    ()
    (append (compile (cadar exp) env fenv nomf)
      '((PUSH :R0))
      (compile-local (cdr exp) env fenv nomf )
    )
  )
)

(defun depile-local (exp env fenv nomf)
  (if (null exp) 
    ()
    (append '((POP :R1)) (depile-local (cdr exp) env fenv nomf))
  )
)

(defun local-env (exp env dep nivem)
  (if (atom exp) 
    env
    (local-env (cdr exp) (cons (cons (caar exp) `(LOC ,dep ,nivem)) env) (+ 1 dep) nivem)
  )
)

(defun iterative-max (lst)
  (if (null lst)
      nil
      (let ((current-max (car lst)))
        (dolist (element (cdr lst) current-max)
          (if (> element current-max)
              (setq current-max element)
          )
        )
      )
  )
)
