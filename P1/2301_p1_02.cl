;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; AUTORES:  Antonio Amor Mourelle
;;;           Esther Lopez Ramos
;;; GRUPO: 2301
;;; PAREJA: 02

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;FUNCION AUXLIAR
;;;lp-rec-aux
;;;Funcion que realiza el sumatorio de los elementos del vector
;;;elevados al exponente p
;;;
;;;INPUT : x: vector, en forma de lista
;;;p: orden de la norma que se quiere calcular
;;;
;;;OUTPUT:sumatorio interior de la norma
(defun lp-rec-aux(x p)
  (if (null x)
      0
    (+ (expt (abs (first x)) p) (lp-rec-aux (rest x) p))))

;;;EJEMPLOS
;;;(lp-rec-aux nil 2); -> 0 caso especial
;;;(lp-rec-aux '(3 4) 2);-> 25 caso general

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; lp-rec (x p)
;;; Calcula la norma Lp de un vector de forma recursiva
;;;
;;; INPUT: x: vector, representado como una lista
;;; p: orden de la norma que se quiere calcular
;;;
;;; OUTPUT: norma Lp de x
;;;
(defun lp-rec (x p)  
  (expt (lp-rec-aux x p) (/ 1 p)))

;;;EJEMPLOS
;;;(lp-rec '() 2); -> 0 caso especial
;;;(lp-rec '(3 4) 2);-> 5.0 caso general

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; lp-mapcar (x p)
;;; Calcula la norma Lp de un vector usando mapcar
;;;
;;; INPUT: x: vector, representado como una lista
;;; p: orden de la norma que se quiere calcular
;;;
;;; OUTPUT: norma Lp de x
;;;
(defun lp-mapcar (x p) 
  (expt (apply #'+ (mapcar #'(lambda (n) (expt (abs n) p)) x)) (/ 1 p)))

;;;EJEMPLOS
;;;(lp-mapcar '() 2); -> 0 caso especial
;;;(lp-mapcar '(3 4) 2);-> 5.0 caso general
;;;
;;;COMENTARIOS
;;;al hacer apply #'+ con un vector es nil da 0

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; l2-rec (x)
;;; Calcula la norma L2 de un vector de forma recursiva
;;;
;;; INPUT: x: vector
;;;
;;; OUTPUT: norma L2 de x
;;;
(defun l2-rec (x)
  (lp-rec x 2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; l2-mapcar (x)
;;; Calcula la norma L2 de un vector usando mapcar
;;;
;;; INPUT: x: vector
;;;
;;; OUTPUT: norma L2 de x
;;;
(defun l2-mapcar (x)
  (lp-mapcar x 2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; l1-rec (x)
;;; Calcula la norma L1 de un vector de forma recursiva
;;;
;;; INPUT: x: vector
;;;
;;; OUTPUT: norma L1 de x
;;;
(defun l1-rec (x) 
  (lp-rec x 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; l1-mapcar (x)
;;; Calcula la norma L1 de un vector usando mapcar
;;;
;;; INPUT: x: vector
;;;
;;; OUTPUT: norma L1 de x
;;;
(defun l1-mapcar (x) 
  (lp-mapcar x 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; linf-mapcar(x)
;;; Calcula la norma infinito de un vector usando mapcar
;;;
;;; INPUT: x: vector
;;;
;;; OUTPUT: norma infinito de x
;;;
(defun linf-mapcar(x)
  (unless (null x)
    (apply #'max (mapcar #'abs x))))

;;;EJEMPLOS
;;;(linf-mapcar '())-> NIL caso particular

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;FUNCION AUXILIAR
;;; linf-rec-aux(x)
;;; Funcion auxiliar para la norma infinito de un vector usando 
;;; recursion
;;;
;;; INPUT: x: vector
;;;
;;; OUTPUT: norma infinito de x
;;;
(defun linf-rec-aux(x)
  (unless (null x)
    (cons (abs (first x)) (linf-rec-aux (rest x)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; linf-rec(x)
;;; Calcula la norma infinito de un vector usando recursion
;;;
;;; INPUT: x: vector
;;;
;;; OUTPUT: norma infinito de x
;;;
(defun linf-rec(x)
  (unless (null x)
    (apply #'max (linf-rec-aux x))))

;;;EJEMPLOS
;;;(linf-rec '())-> NIL caso particular

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;FUNCION AUXILIAR
;;;eval-dist
;;;evalua la distancia entre el vector ref y el vectro vec mediente
;;;la funcion fn-dist
;;;INPUT : ref ; primer vector
;;;vec: segundo vector
;;;fn-dist: funcion con la que se consigue la distancia entre los vectores
;;;
;;;OUTPUT: distancia entre los vectores
(defun eval-dist(ref vec fn-dist)
  (funcall fn-dist (mapcar #'- ref vec)))

;;;EJEMPLOS
;;;(eval-dist '()'(1 0) #'l2-mapcar)-> 0 caso particular
;;;(eval-dist '(1 0)'(1 0) #'l2-mapcar)-> 0 caso general
;;;(eval-dist '(0 0)'(1 0) #'l2-mapcar)-> 1 caso general

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; nearest (lst-vectors vector fn-dist)
;;; Calcula de una lista de vectores el vector más cercano a uno dado,
;;; usando la función de distancia especificada
;;;
;;; INPUT: lst-vectors: lista de vectores para los que calcular la distancia
;;; vector: vector referencia, representado como una lista
;;; fn-dist: referencia a función para medir distancias
;;;
;;; OUTPUT: vector de entre los de lst-vectors más cercano al de referencia
;;;
(defun nearest (lst-vectors vector fn-dist) 
  (unless (null vector)
  (let ((distancias (mapcar #'(lambda(x) (eval-dist vector x fn-dist)) 
                      lst-vectors)))    
    (first (nthcdr (position (apply #'min distancias)
                             distancias) lst-vectors)))))

;;;EJEMPLOS
;;;(nearest '((2 3) (1 2) ()) '(0 0) #'l2-mapcar)-> NIL caso particular
;;;(nearest '(()) '(0 0) #'l2-mapcar)-> NIL caso particular
;;;(nearest '((2 3)(1 2) (5 7)) '() #'l2-mapcar)-> NIL caso particular
;;;(nearest '((2 3) (1 2)) '(0 0) #'l2-mapcar)-> (1 2) caso general
;;;
;;;COMENTARIOS
;;;la funcion espera una lista de listas y esto no se comprueba,
;;;se sigue el principio RTFM





;;;EJERCICIO2


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; secante (f tol-abs max-iter par-semillas)
;;; Estima el cero de una función mediante el método de la secante
;;;
;;; INPUT: f: función cuyo cero se desea encontrar
;;; tol-abs: tolerancia para convergencia
;;; max-iter: máximo número de iteraciones
;;; par-semillas: estimaciones iniciales del cero (x0 x1)
;;;
;;; OUTPUT: estimación del cero de f, o NIL si no converge
;;;
(defun  secante (f tol-abs max-iter par-semillas)
  (unless (or (< max-iter 0) (null par-semillas))
    (let* ((x0 (first par-semillas)) (x1 (second par-semillas)) 
           (fx0 (funcall f x0)) (fx1 (funcall f x1)))
      (if (< (abs (apply #'- par-semillas)) tol-abs)
          x1
        (secante f tol-abs (- max-iter 1) 
                 (list x1 (- x1 (* fx1 (/ (- x1 x0) 
                                          (- fx1 fx0))))))))))
;;;EJEMPLOS
;;;caso de no convergencia
;;;(setf funcion (lambda (x) (+ (sin x) 1.5)))
;;;(secante funcion tol iters semillas)->NIL  
;;;
;;;En el caso general nos remitimos al ejemplo proporcionado en el enunciado

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; un-cero-secante (f tol-abs max-iter pares-semillas)
;;; Prueba con distintos pares de semillas iniciales hasta que
;;; la secante converge
;;;
;;; INPUT: f: función de la que se desea encontrar un cero
;;; tol-abs: tolerancia para convergencia
;;; max-iter: máximo número de iteraciones
;;; pares-semillas: pares de semillas con las que invocar a secante
;;;
;;; OUTPUT: el primer cero de f que se encuentre, o NIL si se diverge
;;; para todos los pares de semillas
;;;
(defun un-cero-secante (f tol-abs max-iter pares-semillas)
  (unless (null pares-semillas)
    (let ((par1 (secante f tol-abs max-iter (first pares-semillas))))
      (if par1
          par1
        (un-cero-secante f tol-abs max-iter (rest pares-semillas))))))

;;;EJEMPLOS
;;;caso de no convergencia
;;;(setf funcion (lambda (x) (+ (sin x) 1.5)))
;;;(un-cero-secante funcion tol iters semillas)->NIL  
;;;
;;;En el caso general nos remitimos al ejemplo proporcionado en el enunciado
;;;
;;;COMENTARIOS
;;;
;;;la funcion espera una lista de listas y esto no se comprueba,
;;;se sigue el principio RTFM

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; todos-ceros-secante (f tol-abs max-iter pares-semillas)
;;; Prueba con distintas pares de semillas iniciales y devuelve
;;; las raíces encontradas por la secante para dichos pares
;;;
;;; INPUT: f: función de la que se desea encontrar un cero
;;; tol-abs: tolerancia para convergencia
;;; max-iter: máximo número de iteraciones
;;; pares-semillas: pares de semillas con las que invocar a secante
;;;
;;; OUTPUT: todas las raíces que se encuentren, o NIL si se diverge
;;; para todos los pares de semillas
;;;
(defun todos-ceros-secante (f tol-abs max-iter pares-semillas)
  (unless (null pares-semillas)
    (cons (secante f tol-abs max-iter (first pares-semillas))
          (todos-ceros-secante f tol-abs max-iter 
                               (rest pares-semillas)))))

;;;EJEMPLOS
;;;caso de no convergencia
;;;(setf funcion (lambda (x) (+ (sin x) 1.5)))
;;;(todos-ceros-secante funcion tol iters semillas)->(NIL)
;;;
;;;En el caso general nos remitimos al ejemplo proporcionado en el enunciado



;;;EJERCICIO3

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; combine-elt-lst (elt lst) 
;;; Combina un elemento con una lista devolviendo una lista que
;;; contiene listas con el formato ((elt first)(elt second)...)
;;; donde first y second son respectivamente los 2 primeros elementos
;;; de la lista
;;;
;;; INPUT: elt: elemento
;;; lst: lista
;;;
;;; OUTPUT: lista que combina el elemto con la lista introducida
(defun combine-elt-lst (elt lst) 
  (unless (null lst)
    (cons ( list elt (first lst)) (combine-elt-lst elt (rest lst)))))

;;;EJEMPLOS
;;;(combine-elt-lst 'a '())-> NIL caso particular
;;;(combine-elt-lst 5 '(a b))-> ((5 A)(5 B)) caso general


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; combine-lst-lst (lst1 lst2)
;;; Realiza el producto cartesiano de las dos listas
;;;
;;; INPUT: lst1: lista1
;;; lst2: lista2
;;; OUTPUT: lista con las tuplas con las combinaciones producidas por el producto cartesiano
(defun combine-lst-lst (lst1 lst2)
  (unless (or(null lst1) (null lst2))
    (append (combine-elt-lst (first lst1) lst2) (combine-lst-lst (rest lst1) lst2))))

;;;EJEMPLOS
;;;(combine-lst-lst '(1 2)'())-> NIL caso particular
;;;(combine-lst-lst '()'(1 2))-> NIL caso particular
;;;(combine-lst-lst '(1 2)'( a b))->((1 A) (1 B) (2 A) (2 B)) caso general

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;FUNCION AUXLIAR
;;;check-lists-okp(listoflsts)
;;;Funcion que comprueba que la list-of-lsts no contiene listas vacias
;;;
;;;INPUT : listoflsts: lista de listas
;;;
;;;OUTPUT:T si ok, NIL si alguna lista vacia
(defun check-lists-okp(listoflsts)
  (if (null listoflsts)
      T
    (unless(null (first listoflsts))
      (check-lists-okp (rest listoflsts)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; combine-list-of-lsts (lstolsts)
;;; Representa todas las posibles disposiciones de elementos
;;; pertenecientes a
;;; N listas de forma que en cada disposición aparezca únicamente 
;;; un elemento de cada lista
;;;
;;; INPUT: lstolsts: lista de listas con las que realizaremos las disposiciones
;;;
;;; OUTPUT: lista con las diferentes disposiciones producidas por la funcion
(defun combine-list-of-lsts (lstolsts)
  (unless (null (check-lists-okp lstolsts))
    (let ((first-lst (first lstolsts)) (other-lsts (rest lstolsts)))
      (if (null first-lst)
          '(NIL)
        (mapcan #'(lambda (x)
                    (mapcar #'(lambda (y) (cons x y))
                      (combine-list-of-lsts other-lsts)))
          first-lst)))))

;;;EJEMPLOS
;;;(combine-list-of-lsts '((1 2)(+)(A B C)))-> NIL caso particular
;;;(combine-list-of-lsts '(())-> (NIL) caso particular
;;;(combine-list-of-lsts '((1 2))-> ((1) (2)) caso particular
;;;(combine-list-of-lsts '((1 2)(+)(A B C)))->
;;;                        ((1 + A) (1 + B) (1 + C) (2 + A) (2 + B) (2 + C)) caso general
