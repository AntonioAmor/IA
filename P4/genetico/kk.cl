;;JUGADORES KK

(defun mi-f-ev-KK1 (estado)
    ;mis fichas menos las del contrario
  (let ((tablero (estado-tablero estado)) (p0 (estado-lado-sgte-jugador estado))
        (p1 (lado-contrario (estado-lado-sgte-jugador estado))))
    (+      
     ;distancia del rival a ganar
     (*(- 19 (get-fichas tablero p1 6)) 0)
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
     (* (- (if (= 0 (get-fichas tablero p1 0)) 2 0) (if (= 0 (get-fichas tablero p0 5)) 1 0)) 3)
     (* (- (if (= 0 (get-fichas tablero p1 1)) 2 0) (if (= 0 (get-fichas tablero p0 4)) 1 0)) 3)
     (* (- (if (= 0 (get-fichas tablero p1 2)) 2 0) (if (= 0 (get-fichas tablero p0 3)) 1 0)) 3)
     (* (- (if (= 0 (get-fichas tablero p1 3)) 2 0) (if (= 0 (get-fichas tablero p0 2)) 1 0)) 3)
     (* (- (if (= 0 (get-fichas tablero p1 4)) 2 0) (if (= 0 (get-fichas tablero p0 1)) 1 0)) 3)
     (* (- (if (= 0 (get-fichas tablero p1 5)) 2 0) (if (= 0 (get-fichas tablero p0 0)) 1 0)) 3))))


(defun mi-f-ev-KK2 (estado)
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
     (* (- (if (= 0 (get-fichas tablero p1 0)) 2 0) (if (= 0 (get-fichas tablero p0 5)) 1 0)) 5)
     (* (- (if (= 0 (get-fichas tablero p1 1)) 2 0) (if (= 0 (get-fichas tablero p0 4)) 1 0)) 5)
     (* (- (if (= 0 (get-fichas tablero p1 2)) 2 0) (if (= 0 (get-fichas tablero p0 3)) 1 0)) 5)
     (* (- (if (= 0 (get-fichas tablero p1 3)) 2 0) (if (= 0 (get-fichas tablero p0 2)) 1 0)) 5)
     (* (- (if (= 0 (get-fichas tablero p1 4)) 2 0) (if (= 0 (get-fichas tablero p0 1)) 1 0)) 5)
     (* (- (if (= 0 (get-fichas tablero p1 5)) 2 0) (if (= 0 (get-fichas tablero p0 0)) 1 0)) 5))))


(defun mi-f-ev-KK3 (estado)
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
     (* (- (if (= 0 (get-fichas tablero p1 0)) 2 0) (if (= 0 (get-fichas tablero p0 5)) 1 0)) 2)
     (* (- (if (= 0 (get-fichas tablero p1 1)) 2 0) (if (= 0 (get-fichas tablero p0 4)) 1 0)) 2)
     (* (- (if (= 0 (get-fichas tablero p1 2)) 2 0) (if (= 0 (get-fichas tablero p0 3)) 1 0)) 2)
     (* (- (if (= 0 (get-fichas tablero p1 3)) 2 0) (if (= 0 (get-fichas tablero p0 2)) 1 0)) 2)
     (* (- (if (= 0 (get-fichas tablero p1 4)) 2 0) (if (= 0 (get-fichas tablero p0 1)) 1 0)) 2)
     (* (- (if (= 0 (get-fichas tablero p1 5)) 2 0) (if (= 0 (get-fichas tablero p0 0)) 1 0)) 2))))


(defun mi-f-ev-KK4 (estado)
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
     (* (- (if (= 0 (get-fichas tablero p1 0)) 2 0) (if (= 0 (get-fichas tablero p0 5)) 1 0)) 7)
     (* (- (if (= 0 (get-fichas tablero p1 1)) 2 0) (if (= 0 (get-fichas tablero p0 4)) 1 0)) 6)
     (* (- (if (= 0 (get-fichas tablero p1 2)) 2 0) (if (= 0 (get-fichas tablero p0 3)) 1 0)) 2)
     (* (- (if (= 0 (get-fichas tablero p1 3)) 2 0) (if (= 0 (get-fichas tablero p0 2)) 1 0)) 1)
     (* (- (if (= 0 (get-fichas tablero p1 4)) 2 0) (if (= 0 (get-fichas tablero p0 1)) 1 0)) 4)
     (* (- (if (= 0 (get-fichas tablero p1 5)) 2 0) (if (= 0 (get-fichas tablero p0 0)) 1 0)) 8))))


(defun mi-f-ev-KK5 (estado)
    ;mis fichas menos las del contrario
  (let ((tablero (estado-tablero estado)) (p0 (estado-lado-sgte-jugador estado))
        (p1 (lado-contrario (estado-lado-sgte-jugador estado))))
    (+      
     ;distancia del rival a ganar
     (*(- 19 (get-fichas tablero p1 6)) 15)
     ;las fichas de mi kahala menos las del contrario
     (*(- (get-fichas tablero p0 6) (get-fichas tablero p1 6)) 120)
     ;las fichas en la posicion 5 del contrario menos las mias
     ;consideramos que es malo acumular demasiadas fichas en esta posicion
     (*(- (get-fichas tablero p1 5) (get-fichas tablero p0 5))100)
     ;las fichas en la posicion 4 del contrario menos las mias
     ;consideramos que es malo acumular demasiadas fichas en esta posicion
     (*(- (get-fichas tablero p1 4) (get-fichas tablero p0 4))80)
   
     (*(- (get-fichas tablero p1 3) (get-fichas tablero p0 3))70)
     (*(- (get-fichas tablero p1 2) (get-fichas tablero p0 2))50)
     (*(- (get-fichas tablero p1 1) (get-fichas tablero p0 1))30)
     (*(- (get-fichas tablero p1 0) (get-fichas tablero p0 0))20)
     
     ;diferencia de ceros
     (* (- (if (= 0 (get-fichas tablero p1 0)) 20 0) (if (= 0 (get-fichas tablero p0 5)) 10 0)) 30)
     (* (- (if (= 0 (get-fichas tablero p1 1)) 20 0) (if (= 0 (get-fichas tablero p0 4)) 10 0)) 30)
     (* (- (if (= 0 (get-fichas tablero p1 2)) 20 0) (if (= 0 (get-fichas tablero p0 3)) 10 0)) 30)
     (* (- (if (= 0 (get-fichas tablero p1 3)) 20 0) (if (= 0 (get-fichas tablero p0 2)) 10 0)) 30)
     (* (- (if (= 0 (get-fichas tablero p1 4)) 20 0) (if (= 0 (get-fichas tablero p0 1)) 10 0)) 30)
     (* (- (if (= 0 (get-fichas tablero p1 5)) 20 0) (if (= 0 (get-fichas tablero p0 0)) 10 0)) 30))))


(defun mi-f-ev-KK6 (estado)
    ;mis fichas menos las del contrario
  (let ((tablero (estado-tablero estado)) (p0 (estado-lado-sgte-jugador estado))
        (p1 (lado-contrario (estado-lado-sgte-jugador estado))))
    (+      
     ;distancia del rival a ganar
     (*(- 19 (get-fichas tablero p1 6)) 0.15)
     ;las fichas de mi kahala menos las del contrario
     (*(- (get-fichas tablero p0 6) (get-fichas tablero p1 6)) 1.20)
     ;las fichas en la posicion 5 del contrario menos las mias
     ;consideramos que es malo acumular demasiadas fichas en esta posicion
     (*(- (get-fichas tablero p1 5) (get-fichas tablero p0 5))1.00)
     ;las fichas en la posicion 4 del contrario menos las mias
     ;consideramos que es malo acumular demasiadas fichas en esta posicion
     (*(- (get-fichas tablero p1 4) (get-fichas tablero p0 4))0.80)
   
     (*(- (get-fichas tablero p1 3) (get-fichas tablero p0 3))0.70)
     (*(- (get-fichas tablero p1 2) (get-fichas tablero p0 2))0.50)
     (*(- (get-fichas tablero p1 1) (get-fichas tablero p0 1))0.30)
     (*(- (get-fichas tablero p1 0) (get-fichas tablero p0 0))0.20)
     
     ;diferencia de ceros
     (* (- (if (= 0 (get-fichas tablero p1 0)) 0.20 0) (if (= 0 (get-fichas tablero p0 5)) 0.10 0)) 0.30)
     (* (- (if (= 0 (get-fichas tablero p1 1)) 0.20 0) (if (= 0 (get-fichas tablero p0 4)) 0.10 0)) 0.30)
     (* (- (if (= 0 (get-fichas tablero p1 2)) 0.20 0) (if (= 0 (get-fichas tablero p0 3)) 0.10 0)) 0.30)
     (* (- (if (= 0 (get-fichas tablero p1 3)) 0.20 0) (if (= 0 (get-fichas tablero p0 2)) 0.10 0)) 0.30)
     (* (- (if (= 0 (get-fichas tablero p1 4)) 0.20 0) (if (= 0 (get-fichas tablero p0 1)) 0.10 0)) 0.30)
     (* (- (if (= 0 (get-fichas tablero p1 5)) 0.20 0) (if (= 0 (get-fichas tablero p0 0)) 0.10 0)) 0.30))))




(setf *kk1* 
  (make-jugador
   :nombre 'kk1
   :f-juego #'f-j-nmx
   :f-eval #'mi-f-ev-kk1))
(setf *kk2* 
  (make-jugador
   :nombre 'kk2
   :f-juego #'f-j-nmx
   :f-eval #'mi-f-ev-kk2))
(setf *kk3* 
  (make-jugador
   :nombre 'kk3
   :f-juego #'f-j-nmx
   :f-eval #'mi-f-ev-kk3))
(setf *kk4* 
  (make-jugador
   :nombre 'kk4
   :f-juego #'f-j-nmx
   :f-eval #'mi-f-ev-kk4))
(setf *kk5*
  (make-jugador
   :nombre 'kk5
   :f-juego #'f-j-nmx
   :f-eval #'mi-f-ev-kk5))
(setf *kk6* 
  (make-jugador
   :nombre 'kk6
   :f-juego #'f-j-nmx
   :f-eval #'mi-f-ev-kk6))

