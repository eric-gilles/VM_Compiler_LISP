
;; Memory Management
(defun vm_get_mem (vm_name adr)
  (if (numberp adr)
    (if (>= adr (array-total-size (get vm_name :MEMORY)))
      (error "accès à l'adresse ~s hors de la memoire" adr)
      (if (null (aref (get vm_name :MEMORY) adr))
        0
        (aref (get vm_name :MEMORY) adr)
      )
    )
  )
)


(defun vm_set_mem (vm_name adr val)
  (if (numberp adr)
    (if (>= adr (array-total-size (get vm_name :MEMORY)))
      (error "accès à l'adresse ~s hors de la memoire" adr)
      (setf (aref (get vm_name :MEMORY) adr) val)
    )
  )
)



;; Registry Management
(defun vm_get_reg(vm_name reg)
  (get vm_name reg)
)

(defun vm_set_reg(vm_name reg val)
  (setf (get vm_name reg) val)
)

;;(vm_get_low vm_name 'LAST_CODE)
(defun vm_get_low_variable(vm_name key)
  (cdr(assoc  key (vm_get_mem vm_name 0)))
)

;; (vm_get_low vm_name 'LAST_CODE 1)
(defun vm_set_low_variable(vm_name key val)
  (let ((mem (vm_get_mem vm_name 0)))
    (vm_set_mem vm_name (cdr (assoc key mem)) val)
  )
)

(defun vm_get_label(vm_name label)
  (gethash label (vm_get_mem vm_name (vm_get_low_variable vm_name 'LABELS)))
)
(defun vm_get_labels(vm_name)
  (vm_get_mem vm_name (vm_get_low_variable vm_name 'LABELS))
)
(defun vm_set_label(vm_name label adr)
  (setf (gethash label (vm_get_mem vm_name (vm_get_low_variable vm_name 'LABELS))) adr)
)