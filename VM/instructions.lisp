(require "function.lisp")

;; Fonctionnalité d'après LEC.pdf 'Mathieu LAFOURCADE'

;; INSTRUCTIONS :

;; INSTRUCTIONS ACCES MEMOIRE :
;; (LOAD <src> <dest>) chargement de mémoire à registre
;; (STORE <src> <dest> chargement de registre à mémoire
;; (MOVE <src> <dest>) mouvement de registre à registre

;; INSTRUCTIONS ARITHMÉTIQUES
;; (ADD <src> <dest>) addition
;; (SUB <src> <dest>) soustraction
;; (MUL <src> <dest>) multiplication
;; (DIV <src> <dest>) division
;; (INCR <dest>) incrément
;; (DECR <dest>) décrément

;; INSTRUCTIONS GESTION DE LA PILE
;; (PUSH <src> empiler
;; (POP <dest> dépiler

;; INSTRUCTIONS DE SAUT
;; (LABEL <label>) déclaration d’étiquette
;; (JMP <label>) saut inconditionnel à une étiquette
;; (JSR <label>) saut avec retour
;; (RTN) retour

;; INSTRUCTIONS DE SAUT CONDITIONNEL
;; (CMP <src1> <src2>) comparaison
;; (JGT <label>) saut si plus grand
;; (JGE <label>) saut si plus grand ou égal
;; (JLT <label>) saut si plus petit
;; (JLE <label>) saut si plus petit ou égal
;; (JEQ <label> saut si égal
;; (JNE <label> saut si différent
;; (TEST <src>) comparaison à NIL
;; (JTRUE <label>) saut si non-NIL
;; (JNIL <label>) saut si NIL

;; INSTRUCTIONS DIVERSES
;; (NOP) rien
;; (HALT) arrêt

;; INSTRUCTIONS ACCES MEMOIRE :

;; LOAD
(defun vm-load(vm src dest) 
  (if (vm_get_reg vm src)
      (vm_set_reg vm dest (vm_get_mem vm (vm_get_reg vm src))) ;; je prend le contenue de l'adresse contenue dans un registre 
    (vm_set_reg vm dest (vm_get_mem vm src)) ;; je prend le contenue de src et je le mets dans dest 
  )

)

;; MOVE
(defun vm-move(vm src dest)
  (if (vm-is-litteral src)
    (vm_set_reg vm dest (cadr src))
	  (vm_set_reg vm dest (vm_get_reg vm src))
  )
)

;; STORE (STORE R0 SP)
(defun vm-store(vm src dest)
  (if (vm-is-litteral src)
    (if (vm_get_reg vm dest)
      (vm_set_mem vm (vm_get_reg vm dest) (cadr src))
      (vm_set_mem vm dest (cadr src))
    )
  )
  (if (and (not (vm_get_reg vm dest)) (not(vm-is-litteral src)))
    (vm_set_mem vm dest (vm_get_reg vm src)) ;; contenue de src je le mets en mémoire à l'adresse "dest" 
  )
  (if (and (vm_get_reg vm dest) (not(vm-is-litteral src)))
      (vm_set_mem vm (vm_get_reg vm dest) (vm_get_reg vm src)) ;; contenue de src je le met dans l'adresse contenue dans un registre 
  ) 
)

;; INSTRUCTIONS ARITHMÉTIQUES
(defun vm-is-litteral(var) 
  (and (listp var) (eql (car var) :LIT))
)

;; ADD
(defun vm-add(vm src dest)  

  (if (vm-is-litteral src)
    (vm_set_reg vm dest (+ (vm_get_reg vm dest) (cadr src)))
  )
  (if (and (not (vm_get_reg vm src)) (not (vm-is-litteral src)))
    (vm_set_reg vm dest (+ (vm_get_mem vm src) (vm_get_reg vm dest)))
  )
  (if (and (vm_get_reg vm src) (not (vm-is-litteral src)))
    (vm_set_reg vm dest (+ (vm_get_reg vm src) (vm_get_reg vm dest)))
  )
)

;; SUB
(defun vm-sub (vm src dest) 
  (if (vm-is-litteral src)
    (vm_set_reg vm dest (- (vm_get_reg vm dest) (cadr src)))

  )
  (if (and (not (vm_get_reg vm src)) (not (vm-is-litteral src)))
    (vm_set_reg vm dest (- (vm_get_reg vm dest) (vm_get_mem vm src)))
  )
  (if (and (vm_get_reg vm src) (not (vm-is-litteral src)))
    (vm_set_reg vm dest (- (vm_get_reg vm dest) (vm_get_reg vm src)))
  )
)


;; INCR
(defun vm-incr(vm dest) 
  (vm_set_reg vm dest (+ (vm_get_reg vm dest) 1))
)

;; DECR
(defun vm-decr(vm dest) 
  (vm_set_reg vm dest (- (vm_get_reg vm dest) 1))
)

;; MUL
(defun vm-mul(vm src dest) 
  (if (vm-is-litteral src)
     (vm_set_reg vm dest (* (vm_get_reg vm dest) (cadr src)))
  )
  (if (not (vm_get_reg vm src))
    (vm_set_reg vm dest (* (vm_get_reg vm dest) (vm_get_mem vm src)))
  )

  (if (vm_get_reg vm src)
    (vm_set_reg vm dest (* (vm_get_reg vm dest) (vm_get_reg vm src)))
  )
)

;; DIV
(defun vm-div(vm src dest) 
  (if (vm-is-litteral src)
    (vm_set_reg vm dest (/ (vm_get_reg vm dest) (cadr src)))
  )
  (if (not (vm_get_reg vm src))
    (vm_set_reg vm dest (/ (vm_get_reg vm dest) (vm_get_mem vm src)))
  )
  (if (vm_get_reg vm src)
    (vm_set_reg vm dest (/ (vm_get_reg vm dest) (vm_get_reg vm src)))
  )
)


;; INSTRUCTIONS GESTION DE LA PILE
;; PUSH
(defun vm-push(vm src) 
  (vm-incr vm :SP)
  
  (if (or (vm_get_reg vm src) (vm-is-litteral src))
    (vm-store vm src :SP) ;; peut être effectué comme un store
    (vm_set_mem vm (vm_get_reg vm :SP) (vm_get_mem vm src)) ;; si src est une adr mem
  )
)

;; POP
(defun vm-pop(vm dest);;ok

	(vm-load vm :SP dest)
	(vm-decr vm :SP)
)

;; INSTRUCTIONS DE SAUT


(defun vm-exec-label(vm_name)
	;; nothing
)
  
;; JUMP
(defun vm-jump (vm label)   
  (let ((adr (vm_get_label vm label)))
    (if (not (null adr))
      (vm_set_reg vm :PC adr)
      (if (vm_get_reg vm label)
        (vm_set_reg vm :PC (vm_get_reg vm label)) ;; label est un registre
        (vm_set_reg vm :PC (vm_get_mem vm label)) ;; label est une adr mémoire
      )
    )
  )
)

;; JSR
(defun vm-jump-retour(vm label) 

  (let ((adr (vm_get_label vm label)))
    (vm-push vm `(:LIT ,(vm_get_reg vm :PC))) ;; met dans SP le contenu de PC+1 en mémoire
    (vm-jump vm label)
  )
)

 
;; RTN 
(defun vm-rtn(vm) 

  (vm-load vm :SP :R1)
  (vm-decr vm :SP)
  (vm-load vm (+ 1 (vm_get_reg vm :FP)) :SP)
  (vm-load vm (+ 2 (vm_get_reg vm :FP)) :FP)
  (vm-jump vm :R1)
)




;; INSTRUCTIONS DE SAUT CONDITIONNEL FLT, FEQ, FGT
;; CMP
(defun vm-cmp-val(vm n1 n2)
  (if (= n1 n2)
    (and (vm_set_reg vm :FLT 0)
      (vm_set_reg vm :FEQ 1)
      (vm_set_reg vm :FGT 0)
    )
  )
  (if (< n1 n2)
    (and (vm_set_reg vm :FLT 1)
      (vm_set_reg vm :FEQ 0)
      (vm_set_reg vm :FGT 0)
    )
  )
  (if (> n1 n2)
    (and (vm_set_reg vm :FLT 0)
      (vm_set_reg vm :FEQ 0)
      (vm_set_reg vm :FGT 1)
    )
  )
)
(defun vm-cmp (vm n1 n2) 
  (if (and (vm-is-litteral n1) (vm-is-litteral n2))
  	(vm-cmp-val vm (cadr n1) (cadr n2))
    (if (and (vm-is-litteral n1) (not (vm-is-litteral n2)))
      (vm-cmp-val vm (cadr n1) (vm_get_reg vm n2))
      (if (and (not(vm-is-litteral n1)) (vm-is-litteral n2))
        (vm-cmp-val vm (vm_get_reg vm n1) (cadr n2))
        (vm-cmp-val vm (vm_get_reg vm n1) (vm_get_reg vm n2))
      )
  	)
  )
)
  

;; JGT
(defun vm-jgt (vm label) 
  (if (and (= (get vm :FGT) 1) (= (get vm :FEQ) 0))
    (vm-jump vm label)
  )
)

;; JGE
(defun vm-jge (vm label) 
  (if (or (= (get vm :FGT) 1) (= (get vm :FEQ) 1))
    (vm-jump vm label)
  )
)

;; JLT
(defun vm-jlt (vm label) 
  (if (and (= (get vm :FLT) 1) (= (get vm :FEQ) 0))
    (vm-jump vm label)
  )
)

;; ;; JLE
(defun vm-jle (vm label) 
  (if (or (= (get vm :FLT) 1) (= (get vm :FEQ) 1))
    (vm-jump vm label)
  )
)

;; ;; JEQ
(defun vm-jeq (vm label) 
  (if (and (= (get vm :FLT) 0) (= (get vm :FGT) 0))
    (vm-jump vm label)
  )
)

;; ;; JNE
(defun vm-jne (vm label) 
  (if (or (= (get vm :FLT) 1) (= (get vm :FGT) 1))
    (vm-jump vm label)
  )
)
  
;; ;; INSTRUCTIONS DIVERSES

(defun vm-nop (vm)
		;; nothing
)

(defun vm-halt (vm)
	(vm_set_reg vm :exitVM 1)
)