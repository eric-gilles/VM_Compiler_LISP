;; Compilation des comparateurs

;; Comparaison
(defun compile-comp (exp env fenv nomf)
  (let ((op (car exp))
        (fin (gensym "finTest")))
    (append
      (compile (cadr exp) env fenv nomf)
      '((PUSH :R0))
     (compile (caddr exp) env fenv nomf)
      '((PUSH :R0))
      '((POP :R1))
      '((POP :R0))
      '((CMP :R0 :R1))
     '((MOVE (:LIT 1) :R0)) ;; vrai
      (cond   ;; si opérateurs sont vérifiés on saute la 
        ((eq op '=) `((JEQ ,fin)))
        ((eq op '<) `((JLT ,fin)))
        ((eq op '>) `((JGT ,fin)))
        ((eq op '<=) `((JLE ,fin)))
        ((eq op '>=) `((JGE ,fin)))
      )
     '((MOVE (:LIT 0) :R0)) ;; remise à 0 (cas faux)
      `((LABEL ,fin))
    )
  )
)


;; AND
;; ok
(defun compile-and(exp etiqFin env fenv nomf)
  (if (null exp)
    (append '((MOVE (:LIT 1) :R0)) `((LABEL ,etiqFin)))
    (append 
      (compile (car exp) env fenv nomf)
      '((CMP :R0 (:LIT 0))) 
      `((JEQ ,etiqFin)) ;; si :R0 = 0 alors faux donc on arrête
      (compile-and (cdr exp) etiqFin env fenv nomf)
    )
  )
)


;; OR
(defun compile-or(exp etiqFin env fenv nomf)
  (if (null exp)
    (append '((MOVE (:LIT 0) :R0)) `((LABEL ,etiqFin)))
    (append 
      (compile (car exp) env fenv nomf)
      '((CMP :R0 (:LIT 1)))
      '((MOVE (:LIT 1) :R0))
      `((JEQ ,etiqFin)) ;; Si :R0 = 1 alors vrai donc on arrête
      (compile-or (cdr exp) etiqFin env fenv nomf)
    )
  )
)