; RA1UZCELP6
; Severus

(defun mi-f-ev (estado)
  (let* ((tablero (estado-tablero estado)) 
         (p0 (estado-lado-sgte-jugador estado))
         (p1 (lado-contrario (estado-lado-sgte-jugador estado)))
         (fichas-p1-0 (get-fichas tablero p1 0)) (fichas-p0-0 (get-fichas tablero p0 0))
         (fichas-p1-1 (get-fichas tablero p1 1)) (fichas-p0-1 (get-fichas tablero p0 1))
         (fichas-p1-2 (get-fichas tablero p1 2)) (fichas-p0-2 (get-fichas tablero p0 2))
         (fichas-p1-3 (get-fichas tablero p1 3)) (fichas-p0-3 (get-fichas tablero p0 3))
         (fichas-p1-4 (get-fichas tablero p1 4)) (fichas-p0-4 (get-fichas tablero p0 4))
         (fichas-p1-5 (get-fichas tablero p1 5)) (fichas-p0-5 (get-fichas tablero p0 5))
         (kahala-p1 (get-fichas tablero p1 6)) (kahala-p0 (get-fichas tablero p0 6)))
    (+      
     ;distancia del rival a ganar
     (*(- 19 kahala-p1) 5)

     ;las fichas de mi kahala menos las del contrario
     (*(- kahala-p0 kahala-p1) 3)
     ;las fichas en la posicion 5 del contrario menos las mias
     ;consideramos que es malo acumular demasiadas fichas en esta posicion
     (*(- fichas-p1-5 fichas-p0-5)10)
     ;las fichas en la posicion 4 del contrario menos las mias
     ;consideramos que es malo acumular demasiadas fichas en esta posicion
     (*(- fichas-p1-4 fichas-p0-4) 8)
     (*(- fichas-p1-3 fichas-p0-3)7)
     (*(- fichas-p1-2 fichas-p0-2)5)
     (*(- fichas-p1-1 fichas-p0-1)3)
     (*(- fichas-p1-0 fichas-p0-0)2)
     
     ;diferencia de ceros
     (* (- (if (= 0 fichas-p1-0) 2 0) (if (= 0 fichas-p0-5) 1 0)) 3)
     (* (- (if (= 0 fichas-p1-1) 2 0) (if (= 0 fichas-p0-4) 1 0)) 3)
     (* (- (if (= 0 fichas-p1-2) 2 0) (if (= 0 fichas-p0-3) 1 0)) 3)
     (* (- (if (= 0 fichas-p1-3) 2 0) (if (= 0 fichas-p0-2) 1 0)) 3)
     (* (- (if (= 0 fichas-p1-4) 2 0) (if (= 0 fichas-p0-1) 1 0)) 3)
     (* (- (if (= 0 fichas-p1-5) 2 0) (if (= 0 fichas-p0-0) 1 0)) 3))))