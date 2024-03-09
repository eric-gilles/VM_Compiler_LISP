(MOVE (:LIT 120) R0)     ; N = 10 (ou la valeur que vous souhaitez)
(MOVE (:LIT 0) R1)      ; F(0) = 0
(MOVE (:LIT 1) R2)      ; F(1) = 1
(PUSH R1)
(PUSH R2)

(LABEL FIBO)         
  (CMP R0 (:LIT 0))     ; Comparaison avec 0
  (JEQ BASE) 
  (POP R1)              ; R1 = F(n-1)
  (POP R2)              ; R2 = F(n-2)
  (ADD R1 R2)           ; R1 = F(n-1) + F(n-2)
  (PUSH R1)
  (PUSH R2)
  (DECR R0)             ; Décrémentation de N
  (JMP FIBO)
(LABEL BASE)

(HALT)
