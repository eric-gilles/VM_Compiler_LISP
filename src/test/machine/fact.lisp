(MOVE (:LIT 6) R0)
(MOVE (:LIT 1) R1)

(PUSH R1)
(MOVE (:LIT 0) R1)
(LABEL FACT)
  (CMP R0 (:LIT 0))      ; Comparaison avec 0
  (JEQ BASE)          ; Saut à la fin si N == 0
  (POP R1)
  (MULT R0 R1)            ; Multiplication de R0 par R1
  (PUSH R1)
  (DECR R0)              ; Décrémentation de N
  (JMP FACT)

(LABEL BASE)  
(HALT)