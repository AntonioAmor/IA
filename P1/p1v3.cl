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
;;;(lp-rec-aux nil 2) ; -> 0 caso especial
;;;(lp-rec-aux '(3 4) 2) ;-> 25 caso general

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
  (when x
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
  (when X
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
  (when x
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
  (when vector
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
;;;(secante funcion 1e-3 50 '(1 2))->NIL  
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
  (when pares-semillas
    (let ((par1 (secante f tol-abs max-iter (first pares-semillas))))
      (if par1
          par1
        (un-cero-secante f tol-abs max-iter (rest pares-semillas))))))

;;;EJEMPLOS
;;;caso de no convergencia
;;;(setf funcion (lambda (x) (+ (sin x) 1.5)))
;;;(un-cero-secante funcion 1e-3 50 '((1 2)(4 5)))->NIL  
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
  (when pares-semillas
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
  (when lst
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
    (when (first listoflsts)
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
  (when (check-lists-okp lstolsts)
    (let ((first-lst (first lstolsts)) (other-lsts (rest lstolsts)))
      (if (null first-lst)
          '(NIL)
        (mapcan #'(lambda (x)
                    (mapcar #'(lambda (y) (cons x y))
                      (combine-list-of-lsts other-lsts)))
          first-lst)))))

;;;EJEMPLOS
;;;(combine-list-of-lsts '(()))-> (NIL) caso particular
;;;(combine-list-of-lsts '((1 2)))-> ((1) (2)) caso particular
;;;(combine-list-of-lsts '((1 2)(+)(A B C)))->
;;;                        ((1 + A) (1 + B) (1 + C) (2 + A) (2 + B) (2 + C)) caso general



;;;EJERCICIO4

(defconstant +bicond+ '<=>)
(defconstant +cond+ '=>)
(defconstant +and+ '^)
(defconstant +or+ 'v)
(defconstant +not+ '¬)

(defun truth-value-p (x)
  (or (eql x t) (eql x nil)))

(defun unary-connector-p (x) ; toma 1 argumento
  (eql x +not+))

(defun binary-connector-p (x) ; toma 2 argumentos
  (or (eql x +bicond+) (eql x +cond+)))

(defun n-ary-connector-p (x) ; toma n = 0,1,2,… argumentos
  (or (eql x +and+) (eql x +or+)))

(defun connector-p (x)
 (or (unary-connector-p x)
 (binary-connector-p x)
     (n-ary-connector-p x)))

(defun comprobar-unario-p (expresion)
  (if (null (rest expresion))
      (let ((primero (first expresion)))
      (if (listp primero)
          (proposicion-p primero)
        T))))

(defun comprobar-binario-p (expresion)
  (when (null (third expresion))
    (and (proposicion-p (first expresion)) 
         (proposicion-p (second expresion)))))

(defun comprobar-n-ario-p (expresion)
  (when expresion
    (mapcan #'proposicion-p expresion)))

(defun proposicion-p(expresion)
  (let ((atomop (atom expresion)) (conectorp (connector-p expresion)))
    (cond
     ((and atomop (not conectorp)) t)
     ((and atomop conectorp) nil)
     (t 
      (let ((op (first expresion)) (expr (rest expresion)))
     (cond
     ((unary-connector-p op) (comprobar-unario-p expr))
     ((binary-connector-p op) (comprobar-binario-p expr))
     ((n-ary-connector-p op) (comprobar-n-ario-p expr))
      (t nil)))))))

(defun base-p(lista-exp)
  (when (listp lista-exp)
    (mapcan #'proposicion-p lista-exp)))


;;;   4.2

(defun extrae-simbolos-aux (kb)
  (if (listp kb)
      (extrae-simbolos 
       (remove-if #'connector-p kb) )
    (list kb)))

(defun extrae-simbolos (kb)
  (when kb
    (remove-duplicates 
     (mapcan #'extrae-simbolos-aux (remove-if #'connector-p kb)))))


;;;   4.3
(defun genera-lista-interpretaciones (lst)
  (when lst
    (combine-list-of-lsts 
     (mapcar #'(lambda (x) (combine-elt-lst x '(T NIL))) lst))))




;;;   4.4
;;;
;;;

;;;definimos las operaciones que se tendran que realizar

(defun mynot (lst)
  (if (listp lst)
      (not (first lst))
  (not lst)))

(defun implica (lst)
  (or (not (first lst))
      (first(rest lst)))) ;el rest es una lista (de un elemento),
                          ;cogemos el elemento

(defun bicondicional (lst)
  (and (implica lst)
       (implica (reverse lst))))

(defun myor (r)
  (reduce #'(lambda (x y) (or x y)) r))

(defun myand (r)
  (reduce #'(lambda (x y) (and x y)) r))


;;;mapeamos las operaciones que se tendran que realizar

(defun conseguir-op (op)
  (cond 
    ((eql op +not+) #'mynot)
    ((eql op +cond+) #'implica)
    ((eql op +bicond+) #'bicondicional)
    ((eql op +and+) #'myand)
    ((eql op +or+) #'myor)
    (t nil)))

;;funcion auxiliar que sistituye las variables por los valores de verdad
(defun sustituir-val (int kb) 
  (if (not (null int))
       
      (sustituir-val( rest int)
                         (subst 
                          ; cogemos el elemento a sustituir y
                          ;ademas le quitamos los parentesis
                          (first(rest (first int))) 
                          ; el elemento que va a ser sustituido
                          (first (first int))
                          kb))
    kb))

;;;funcion auxiliar que evalua una proposicion
(defun eval-prop-p (prop)
  (cond
   ((null (listp prop)) prop)
   ((connector-p(first prop))  (funcall (conseguir-op(first prop)) ;funcall 
                                        (mapcar #'eval-prop-p
                                          (rest prop))))
   (t (first prop))))

;;;funcion que comprueba si una interpretacion
;;;es modelo de la base de conocimiento
(defun interpretacion-modelo-p (interp kb)
   (when (base-p kb)
    (reduce #'(lambda(x y) (and (eval-prop-p x) (eval-prop-p y))) 
            (append (sustituir-val interp kb) '(T)))))



;;; 4.5

;;;funcion que encuentra los modelos de entre 
;;;todas las posibles interpretaciones de kb
;;;(defun encuentra-modelos (kb)
;;; (when (base-p kb)
;;;    (mapcar #'(lambda (x) (interpretacion-modelo-p (print x) kb))
;;;      (genera-lista-interpretaciones
;;;       (extrae-simbolos kb)))))

(defun encuentra-modelos-aux (interps kb)
  (when interps
  (if (interpretacion-modelo-p (first interps) kb)
      (cons (first interps) (encuentra-modelos-aux (rest interps) kb))
    (encuentra-modelos-aux (rest interps) kb))))                              
      
(defun encuentra-modelos (kb)
  (when (base-p kb)
    (encuentra-modelos-aux (genera-lista-interpretaciones 
                            (extrae-simbolos kb)) kb)))




;;; 4.6

(defun consecuencia-p (prop kb)  
  (mapcan #'(lambda(x) (interpretacion-modelo-p x (list prop))) 
    (encuentra-modelos kb)))

;;Error interpretacion-modelo-p append


;;; 5
;;;
;;; 5.3 5.4
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;Breadth-first-search in graphs
;;;
(defun bfs (end queue net)
  (if (null queue) nil ; queue es la lista de abiertos
    (let ((path (car queue)))
      (let ((node (car path)))
        (if (eql node end) (reverse path) ; Si el nodo satisface el 
                                          ; test objetivo devolvemos 
                                          ; el camino hasta el
          
          (bfs end (append
                    (cdr queue) ; eliminamos el nodo de la lista abierta
                    (new-paths path node net)) ;añadimos los nodos
                                               ;resultantes de la expasion
               net))))))

(defun new-paths (path node net) ;expansion del nodo
  (mapcar #'(lambda(n)
              (cons n path))
    (cdr (assoc node net)))) ;conseguimos los hijos
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun shortest-path (start end net)
  (bfs end (list (list start)) net))

(setf net '((a b c d e) (b a d e f) (c a g) (d a b g h)
            (e a b g h) (f b h) (g c d e h) (h d e f g)))

(shortest-path 'f 'c net)

(defun bfs-improved (end queue net) 
