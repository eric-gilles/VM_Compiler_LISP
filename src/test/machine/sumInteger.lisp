(MOVE (:LIT 5) R0)  ; Remplacez 5 par la valeur de N que vous souhaitez

; Initialisation de la somme à zéro
(MOVE (:LIT 0) R1)

; Boucle pour calculer la somme des N premiers entiers
(LABEL SUM_LOOP)
    (CMP R0 (:LIT 0))      ; Comparaison avec 0
    (JEQ END_SUM)          ; Saut à la fin si N == 0
    (ADD R0 R1)            ; Ajout de R0 à la somme actuelle (R1)
    (DECR R0)              ; Décrémentation de N
    (JMP SUM_LOOP)         ; Saut au début de la boucle

(LABEL END_SUM)
    ; À ce stade, R1 contient la somme des N premiers entiers
    ; Vous pouvez utiliser la valeur de R1 comme nécessaire
(HALT)