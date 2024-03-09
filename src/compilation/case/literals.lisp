;; Compilation des litteraux

(defun compile-const (const)
  `((MOVE (:LIT ,const) :R0))
)


(defun compile-varg (exp)
  `((MOVE (:* :@ ,exp) ::R0))
)


(defun compile-literral (exp env fenv nomf)
  (let ((var (assoc exp env)))
    (cond
      ((not (null var)) 
        (if (eql (cadr var) 'LOC) ;; variable présent dans l'environnement
          (append '((MOVE :FP :R0))
                  `((ADD (:LIT,(car (cdr (cdr var)))) :R0))
                  `((LOAD :R0 :R0))
          ) 
          (if (numberp (cadr var)) 
              (compile-const (cdr var))
          )
        )
      )
      ((and (symbolp exp) (not (null exp))) (compile-varg exp env fenv nomf))

      (t (compile-const exp))
    )
  )
)
