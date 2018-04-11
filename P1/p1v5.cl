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

;;; 4.1 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;FUNCION AUXILIAR
;;;permite saber si una expresion tiene el 
;;;formato de una expresion unaria
(defun comprobar-unario-p (expresion)
  (if (null (rest expresion))
      (let ((primero (first expresion)))
      (if (listp primero)
          (proposicion-p primero)
        T))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;FUNCION AUXILIAR
;;;permite comprobar si una expresion tiene el 
;;;formato de una expresion binaria
(defun comprobar-binario-p (expresion)
  (when (null (third expresion))
    (and (proposicion-p (first expresion)) 
         (proposicion-p (second expresion)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;FUNCION AUXILIAR
;;;comprueba si una expresion tiene el 
;;;formato de una expresion binaria
(defun comprobar-n-ario-p (expresion)
  (when expresion
    (mapcan #'proposicion-p expresion)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;FUNCION PROPOSICON_P
;;;muestra si una proposicion tiene el formato correcto para ser
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

;;;EJEMPLOS
;;; (proposicion-p '()) -> T caso particular, el vacio esta bien
;;;                        formado
;;; (proposicion-p 'A) -> T caso general
;;; (proposicion-p '(H <=> (¬ H))) -> NIL caso general
;;; (proposicion-p '(<=> H (¬ H))) -> T caso general
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;FUNCION BASE-P
;;; permite comprobar si una base de conocimiento
;;; tiene el formato correcto de una base
(defun base-p(lista-exp)
  (unless (proposicion-p lista-exp)
    (mapcan #'proposicion-p lista-exp)))

;;;EJEMPLOS
;;; (base-p '(())) -> T caso particular, consdieramos de nuevo el
;;;                   vacío como una proposición ;;; (base-p 'A) -> NIL caso particular, una proposicion no es una base
;;; (base-p '((<=> H (¬ H)))) -> T caso general, base con una proposicion
;;; (base-p '((<=> A (¬ H)) (<=> P (^ A H)) (<=> H P)))-> T caso general,
;;; base con varias proposiciones


;;;   4.2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;FUNCION AUXILIAR
;;; Extrae los simbolos de una proposicion. Como una proposicion
;;; puede contener proposiciones dentro necesitamos una llamada a 
;;; la funcion principal
(defun extrae-simbolos-aux (prop)
  (if (listp prop)
      (extrae-simbolos 
       (remove-if #'connector-p prop) )
    (list prop)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;FUNCION EXTRAE-SIMBOLOS
;;; funcion que extrae los simbolos de una base de conocimiento 
;;; sin repeticiones
(defun extrae-simbolos (kb)
  (when kb
    (remove-duplicates 
     (mapcan #'extrae-simbolos-aux (remove-if #'connector-p kb)))))

;;;EJEMPLOS
;;; (extrae-simbolos '()) -> NIL Caso particular, la lista vacia no tiene
;;; simbolos
;;; (extrae-simbolos '(A)) -> A Caso general
;;; (extrae-simbolos '((v (¬ A) A B (¬ B)))) -> (A B) caso general

;;;   4.3

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;FUNCION GENERA-LISTA-INTERPRETACIONES
;;; coge los atomos de la lista pasada por argumento y
;;; devuelve todas las posibles interpretaciones
(defun genera-lista-interpretaciones (lst)
  (when lst
    (combine-list-of-lsts 
     (mapcar #'(lambda (x) (combine-elt-lst x '(T NIL))) lst))))

;;;EJEMPLOS
;;; (genera-lista-interpretaciones nil)-> NIL caso particular
;;; (genera-lista-interpretaciones '(P))-> (((P T) ((P NIL))) Caso 
;;; general
;;; (genera-lista-interpretaciones '(P I))-> 
;;; (((P T) (I T)) ((P T) (I NIL)) ((P NIL) (I T)) ((P NIL) (I NIL)))
;;; Caso general

;;;   4.4
;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Definimos las operaciones que se tendran que realizar
;;; Necesitamos, debido a nuestra implementacion, que las funciones
;;; and, or, not se puedan aplicar sobre una lista

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;mapeamos las operaciones que se tendran que realizar

(defun conseguir-op (op)
  (cond 
    ((eql op +not+) #'mynot)
    ((eql op +cond+) #'implica)
    ((eql op +bicond+) #'bicondicional)
    ((eql op +and+) #'myand)
    ((eql op +or+) #'myor)
    (t nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;FUNCION AUXILIAR
;;; Cambia el nombre de un atomo por su valor de verdad.

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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;FUNCION AUXILIAR
;;; Una vez que se ha sustituido el valor de verdad en los atomos
;;; de una proposicion, esta funcion la evalua haciendo uso de las
;;; funciones definidas anteriormente
(defun eval-prop-p (prop)
  (cond
   ((null (listp prop)) prop)
   ((connector-p(first prop))  (funcall (conseguir-op(first prop)) ;funcall 
                                        (mapcar #'eval-prop-p
                                          (rest prop))))
   (t (first prop))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;FUNCION INTERPRETACION-MODELO-P
;;; funcion que comprueba si una interpretacion
;;; es modelo de la base de conocimiento
(defun interpretacion-modelo-p (interp kb)
   (when (base-p kb)
    (reduce #'(lambda(x y) (and (eval-prop-p x) (eval-prop-p y))) 
            (append (sustituir-val interp kb) '(T)))))
;;;EJEMPLOS
;;;(interpretacion-modelo-p '() lista) -> NIL caso particular
;;;
;;;(interpretacion-modelo-p interp '()) -> NIL caso particular
;;;
;;;(interpretacion-modelo-p '((A nil) (P nil) (H t)) '((<=> A (¬ H)) 
;;;                                                    (<=> P (^ A H))(=> H P))) -> NIL
;;;(interpretacion-modelo-p '((A t) (P nil) (H nil)) '((<=> A (¬ H)) 
;;;                                                    (<=> P (^ A H))(=> H P))) -> T




;;; 4.5
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FUNCION AUXILIAR
;;; comprueba de manera recursiba si las interpretaciones 
;;; pasadas pro argumento son modelo
(defun encuentra-modelos-aux (interps kb)
  (when interps
  (if (interpretacion-modelo-p (first interps) kb)
      (cons (first interps) (encuentra-modelos-aux (rest interps) kb))
    (encuentra-modelos-aux (rest interps) kb)))) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FUNCION ENCUENTRA-MODELOS
;;; funcion que encuentra los modelos de entre 
;;; todas las posibles interpretaciones de kb    
(defun encuentra-modelos (kb)
  (when (base-p kb)
    (encuentra-modelos-aux (genera-lista-interpretaciones 
                            (extrae-simbolos kb)) kb)))

;;;EJEMPLOS
;;;(encuentra-modelos '()) -> NIL caso particular
;;;
;;;(encuentra-modelos '((=> A (¬ H)) (<=> P (^ A H)) (=> H P))) ->
;;; -> (((A T) (H NIL) (P NIL)) ((A NIL) (H NIL) (P NIL))) caso general
;;;
;;;(encuentra-modelos '((=> (^ P I) L) (=> (¬ P) (¬ L)) (¬ P) L)) -> NIL caso general


;;; 4.6
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;FUNCION ENCUENTRA-MODELOS
;;;comprueba si la proposicion pasada como argumento 
;;;es consecuencia logica de la kb
(defun consecuencia-p (prop kb)
  (when (base-p kb)
    (mapcan #'(lambda(x) (interpretacion-modelo-p x (list prop))) 
      (encuentra-modelos kb))))

;;;EJEMPLOS
;;;(consecuencia-p 'A '()) -> NIL caso particular
;;;
;;;(consecuencia-p '() '((=> A (¬ H)) (<=> P (^ A H)) (=> H P))) -> NIL caso particular
;;;
;;;(consecuencia-p 'A '(A)) -> T caso general
;;;
;;;(consecuencia-p 'A '(¬ A)) -> NIL caso general
;;;
;;;(consecuencia-p '(¬ H) '((=> A (¬ H)) (<=> P (^ A H)) (=> H P))) -> T caso general

;;; 5
;;;
;;; 5.3 5.4
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;FUNCION BUSQUEDA EN ANCHURA COMENTADA
;;;Breadth-first-search in graphs
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;FUNCION SHORTEST-PATH
;;;funcion que encuentra mediante el algoritmo de busqueda en anchura,
;;:implementado en la funciuon anterior, el camino mas corto entre 2 nodos
(defun shortest-path (start end net)
  (bfs end (list (list start)) net))




;;llamadas realizadas 5.7

;;;(setf net '((a d c b e) (b f d e a) (c a g) (d a b g h)
;;;            (e a b g h) (f b h) (g c d e h) (h d e f g)))
;;;
;;;(shortest-path 'f 'c net)



;;; 5.8
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FUNCION BFS-IMPROVED
;;; Breadth-first-search in graphs sin revisitar nodos
(defun bfs-improved (end queue net)
    (if (null queue) nil ; queue es la lista de abiertos
    (let ((path (car queue)))
      (let ((node (car path)))
        (if (eql node end) (reverse path) ; Si el nodo satisface el 
                                          ; test objetivo devolvemos 
                                          ; el camino hasta el
          
          (bfs end (append
                    (cdr queue) ; eliminamos el nodo de la lista abierta
                    (new-paths path node net)) ; añadimos los nodos
                                               ; resultantes de la expasion
               (remove nil (mapcar #'(lambda (x) (remove-node node x))net)))))))); quitamos el nodo actual 
                                                                                 ; para evitar repeticiones

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FUNCION AUXILIAR
;;; permite la eliminacion de los nodos ya visitados
(defun remove-node(node lst)
  (unless (eql (first lst) node)
    (remove node lst)))
      
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Funcion que llama a bfs-improved para hallar el camino minimo sin tener 
;;; en cuenta los nodos ya explorados
(defun shortest-path-improved (start end net)
  (bfs-improved end (list (list start)) net))


;;;CASO DEL ERROR EN BFS NORMAL:
;;; (setf net '((a b)(b a)(c b)))
;;; (shortest-path 'a 'c net)
;;; Esto causa un bucle que se ve resuelto al utilizar bfs-improved




