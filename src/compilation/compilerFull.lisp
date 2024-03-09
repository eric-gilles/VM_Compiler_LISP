;; Compilateur Complet


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

;; Compilation des opérateurs arithmétiques (+, -, *, /)
(defun compile-operator (exp env fenv nomf)

  (let ((op (car exp)) (arg (cdr exp)))
    (append
       (compile (car arg) env fenv nomf)
      '((PUSH :R0))
      (compile (cadr arg) env fenv nomf)
      '((PUSH :R0))
      '((POP :R1))
      '((POP :R0))    
      (cond
        ((eq op '+) '((ADD :R1 :R0)))
        ((eq op '-) '((SUB :R1 :R0)))
        ((eq op '*) '((MULT :R1 :R0)))
        ((eq op '/) '((DIV :R1 :R0)))
      )
    )
  )
)


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
