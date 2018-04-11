; RA1UZCELP6
; Sirius

(defun mi-f-ev (estado)
    ;mis fichas menos las del contrario
  (let ((tablero (estado-tablero estado)) (p0 (estado-lado-sgte-jugador estado))
        (p1 (lado-contrario (estado-lado-sgte-jugador estado))))
    (+      
     ;distancia del rival a ganar
     (*(- 19 (get-fichas tablero p1 6)) 1.5)
     ;las fichas de mi kahala menos las del contrario
     (*(- (get-fichas tablero p0 6) (get-fichas tablero p1 6)) 12)
     ;las fichas en la posicion 5 del contrario menos las mias
     ;consideramos que es malo acumular demasiadas fichas en esta posicion
     (*(- (get-fichas tablero p1 5) (get-fichas tablero p0 5))10)
     ;las fichas en la posicion 4 del contrario menos las mias
     ;consideramos que es malo acumular demasiadas fichas en esta posicion
     (*(- (get-fichas tablero p1 4) (get-fichas tablero p0 4))8)
   
     (*(- (get-fichas tablero p1 3) (get-fichas tablero p0 3))7)
     (*(- (get-fichas tablero p1 2) (get-fichas tablero p0 2))5)
     (*(- (get-fichas tablero p1 1) (get-fichas tablero p0 1))3)
     (*(- (get-fichas tablero p1 0) (get-fichas tablero p0 0))2)
     
     ;diferencia de ceros
     (* (- (if (= 0 (get-fichas tablero p1 0)) 2 0) (if (= 0 (get-fichas tablero p0 5)) 1 0)) 4)
     (* (- (if (= 0 (get-fichas tablero p1 1)) 2 0) (if (= 0 (get-fichas tablero p0 4)) 1 0)) 4)
     (* (- (if (= 0 (get-fichas tablero p1 2)) 2 0) (if (= 0 (get-fichas tablero p0 3)) 1 0)) 4)
     (* (- (if (= 0 (get-fichas tablero p1 3)) 2 0) (if (= 0 (get-fichas tablero p0 2)) 1 0)) 4)
     (* (- (if (= 0 (get-fichas tablero p1 4)) 2 0) (if (= 0 (get-fichas tablero p0 1)) 1 0)) 4)
     (* (- (if (= 0 (get-fichas tablero p1 5)) 2 0) (if (= 0 (get-fichas tablero p0 0)) 1 0)) 4))))