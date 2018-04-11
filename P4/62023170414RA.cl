; RA1UZCELP6
; Severus

(defun mi-f-ev (estado)
    ;mis fichas menos las del contrario
  (let ((tablero (estado-tablero estado)) (p0 (estado-lado-sgte-jugador estado))
        (p1 (lado-contrario (estado-lado-sgte-jugador estado))))
  (+ (- 19 (get-fichas tablero p1 6))
     ;las fichas de mi kahala menos las del contrario
     (* (- (get-fichas tablero p0 6) (get-fichas tablero p1 6)) 10)
     ;las fichas en la posicion 5 del contrario menos las mias
     ;consideramos que es malo acumular demasiadas fichas en esta posicion
     (*(- (get-fichas tablero p1 5) (get-fichas tablero p0 5)) 6)
     ;las fichas en la posicion 4 del contrario menos las mias
     ;consideramos que es malo acumular demasiadas fichas en esta posicion
     (*(- (get-fichas tablero p1 4) (get-fichas tablero p0 4)) 5))))
