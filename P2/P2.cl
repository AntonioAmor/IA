;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; VARIABLES GLOBALES 
(setf *planets* 
'(Avalon Davion Manory Kentares Katril Proserpina Sirtis))

(setf *white-holes*
'((Avalon Mallory 2) (Avalon Proserpina 12) 
  (Mallory Katril 6) (Mallory Proserpina 17)
  (Katril Mallory 6) (Katril Davion 2)
  (Davion Proserpina 14) (Davion Sirtis 1) 
  (Sirtis Proserpina 10) (Sirtis Davion 1)
  (Proserpina Mallory 17) (Proserpina Avalon 12) (Proserpina Davion 14) (Proserpina Sirtis 10)
  (Kentares Avalon 3) (Kentares Katril 12) (Kentares Proserpina 10)))

(setf *worm-holes*
  '((Avalon Kentares 4) (Kentares Avalon 4) 
    (Avalon Mallory 7) (Mallory Avalon 7)
    (Davion Katril 1) (Katril Davion 1) 
    (Davion Sirtis 8) (Sirtis Davion 8)
    (Mallory Katril 5) (Katril  Mallory 5) 
    (Mallory Proserpina 16) (Proserpina  Mallory 16) 
    (Katril Sirtis 10) (Sirtis Katril 10)
    (Sirtis Proserpina 7) (Proserpina Sirtis 7) 
    (Proserpina Kentares 21) (Kentares Proserpina 21)))

(setf *sensors*
'((Avalon 5) (Mallory 7) (Kentares 4) (Davion 1) (Proserpina 4) (Katril 3) (Sirtis 0)))

(setf *planet-origin* 'Kentares) 
(setf *planets-destination* '(Sirtis))

 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Problem definition
;;
(defstruct problem
 states ; List of states
 initial-state ; Initial state
 f-goal-test ; reference to a function that determines whether
 ; a state fulfills the goal
 f-h ; reference to a function that evaluates to the
 ; value of the heuristic of a state
 operators) ; list of operators (references to functions)
 ; to generate succesors
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Node in search tree
;;
(defstruct node
  state ; state label
  parent ; parent node
  action ; action that generated the current node from its parent
  (depth 0) ; depth in the search tree
  (g 0) ; cost of the path from the initial state to this node
  (h 0) ; value of the heuristic
  (f 0))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Actions
;;
(defstruct action
  name ; Name of the operator that generated the action
  origin ; State on which the action is applied
  final ; State that results from the application of the action
  cost ) ; Cost of the action
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Search strategies
;;
(defstruct strategy
  name ; Name of the search strategy
  node-compare-p) ; boolean comparison
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; FUNCION: f-goal-test-galaxy
;; DESCRIPCION: Comprueba si el navegante ha alcanzado su objetivo
;; INPUT: state: estado actual del navegante
;;        planets-destination: lista de objetivos
;; OUTPUT: t o nil      
(defun f-goal-test-galaxy (state planets-destination)
  (member state planets-destination))

;;EJEMPLOS
;;(f-goal-test-galaxy 'Sirtis *planets-destination*) ;-> (SIRTIS)
;;(f-goal-test-galaxy 'Avalon *planets-destination*) ;-> NIL
;;(f-goal-test-galaxy 'Urano *planets-destination*) ;-> NIL
;;COMENTARIOS
;;
;;si quisiesemos conseguir el planeta en vez de evaluar en lugar de when 
;;usariamos first
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; FUNCION: f-h-galaxy
;; DESCRIPCION: calcula la heuristica del problema
;; INPUT: state: estado actual del navegante
;;        sensors: lista de sensores
;; OUTPUT: heuristica del state
(defun f-h-galaxy (state sensors)
  (when sensors
    (if (eql (first (first sensors)) state)
      (second (first sensors))
      (f-h-galaxy state (rest sensors))))) 
;;EJEMPLOS
;;(f-h-galaxy 'Sirtis *sensors*) ;-> 0
;;(f-h-galaxy 'Avalon *sensors*) ;-> 5
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; FUNCION AUXILIAR navigate 
;; DESCRIPCION: funcion de navegacion worn-hole
;; INPUT: name: nombre de la forma de navegacion
;;        state: estado actual del navegante
;;        worn-holes: lista de sensores
;; OUTPUT: heuristica del state


(defun navigate (name state net)
  (mapcan #'(lambda(x)
              (when (eql (first x) state)
               (list (make-action :name name
                             :origin state
                             :final (second x)
                             :cost (third x)))))net))

;;COMENTARIOS
;;en vez de crear una lista se podria realizar sobre el resultado un remove NIL

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; FUNCION navigate-worm-hole ;; DESCRIPCION: funcion de navegacion worm-hole
;; INPUT: state: estado actual del navegante
;;        worm-holes: lista de sensores
;; OUTPUT: heuristica del state
(defun navigate-worm-hole (state worm-holes)
  (navigate "NAVIGATE-WORM-HOLE" state worm-holes))


;; EJEMPLOS
;;(navigate-worm-hole 'Katril *worm-holes*);->
;;(#S(ACTION :NAME NAVIGATE-WORM-HOLE :ORIGIN KATRIL :FINAL DAVION :COST 1)
;; #S(ACTION :NAME NAVIGATE-WORM-HOLE :ORIGIN KATRIL :FINAL MALLORY :COST 5)
;; #S(ACTION :NAME NAVIGATE-WORM-HOLE :ORIGIN KATRIL :FINAL SIRTIS :COST 10))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; FUNCION navigate-white-hole 
;; DESCRIPCION: funcion de navegacion white-hole
;; INPUT: state: estado actual del navegante
;;        white-holes: lista de sensores
;; OUTPUT: heuristica del state
(defun navigate-white-hole (state white-holes)
  (navigate "NAVIGATE-WHITE-HOLE" state white-holes))


;; EJEMPLOS
;;(navigate-white-hole 'Urano *white-holes*);-> NIL
;;(navigate-white-hole 'avalon *white-holes*)
;;(#S(ACTION :NAME "NAVIGATE-WHITE-HOLE" :ORIGIN AVALON :FINAL MALLORY
;;           :COST 2)
;; #S(ACTION :NAME "NAVIGATE-WHITE-HOLE" :ORIGIN AVALON :FINAL PROSERPINA
;;           :COST 12))
;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; VARIABLES GLOBALES
;; EJERCICIO 4


(defun node-compare-a* (n1 n2)
  (<= (node-f n1) (node-f n2)))

(setf *A-star*
  (make-strategy 
   :name 'A-star
   :node-compare-p 'node-compare-a*))

(defun node-g-<= (node-1 node-2)
 (<= (node-g node-1) (node-g node-2)))

(setf *uniform-cost*
  (make-strategy
   :name 'uniform-cost
   :node-compare-p 'node-g-<=))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; EJERCICIO 5

(setf *galaxy-M35*
 (make-problem
 :states *planets*
 :initial-state *planet-origin*
 :f-goal-test #'(lambda (state) (f-goal-test-galaxy state *planets-destination*))
 :f-h #'(lambda(x) (f-h-galaxy x *sensors*))
 :operators (list #'(lambda(state) (navigate-worm-hole state *worm-holes*))
                       #'(lambda(state) (navigate-white-hole state *white-holes*)))))

;;(setf node-00 (make-node :state 'Proserpina :depth 12 :g 10 :f 20))
;;(setf node-01
;; (make-node :state 'Avalon :depth 0 :g 0 :f 0) )
;;(setf node-02
;; (make-node :state 'Kentares :depth 2 :g 50 :f 50) )
   


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; EJERCICIO 6
;; FUNCION expand-node
;; DESCRIPCION: funcion de navegacion white-hole
;; INPUT: node: nodo que se pretende expandir
;;        problem: problema
;; OUTPUT: lista de nodos resultantes de expandir node

(defun expand-node (node problem)  
  (mapcar #'(lambda(x) (let ((g (+ (node-g node) (action-cost x)))
        (h (funcall (problem-f-h problem) (action-final x))))
                        (make-node
                        :state (action-final x)
                        :parent node
                        :action x
                        :depth (+ 1 (node-depth node))
                        :g g
                        :h h
                        :f (+ g h))))
    (mapcan #'(lambda (x) (funcall x (node-state node)))
                            (problem-operators problem))))
              
                        

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; EJERCICIO 7
;; Insert a list of nodes into another list of nodes

;; FUNCION AUXILIAR
;; FUNCION insert-node-strategy
;; DESCRIPCION: inserta un unico nodo dentro de una lista ordenada en la posicion correcta
;; INPUT: node: nodo que se pretende insertar
;;        lst-nodes: lista ordenada en la que se insertara node
;;        strategy: estrategia seguida (comparador) para saber donde se tiene que insertar
;;        comp-lst: lista de nodos ya comparados, esta ordenada
;; OUTPUT: lista de nodos con el insertado en la posicion correcta
(defun insert-node-strategy (node lst-nodes strategy comp-lst)
  (cond 
   ((null lst-nodes) (append comp-lst (list node)))                                    ;;Caso insercion al final de la lista
  ((funcall (strategy-node-compare-p strategy) node (first lst-nodes))
      (append comp-lst (cons node lst-nodes)))                                         ;;Caso insercion en la posicion correspondiente
    (T (insert-node-strategy node (rest lst-nodes) strategy                            ;;No se ha encontrado la posicion correcta, seguimos buscando
                          (append comp-lst (list (first lst-nodes)))))))               


;;Siguiendo el mismo principio que la funcion auxiliar esta funcion la usamos para inicializar las listas
(defun insert-nodes-strategy (nodes lst-nodes strategy)
  (if (null nodes)
      lst-nodes
    (insert-nodes-strategy (rest nodes) 
                           (insert-node-strategy (first nodes) lst-nodes strategy nil)
                           strategy)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; EJERCICIO 8
;; open: lista de nodos generados, pero no explorados
;; closed: lista de nodos generados y explorados
;; strategy: estrategia de búsqueda implementada como una ordenación de la
;; lista open-nodes
;; goal-test: test objetivo (predicado que evalúa a T si un nodo cumple la
;;  condición de ser meta)

; Realiza la búsqueda para el problema dado utilizando una estrategia
; Evalúa:
; Si no hay solución: NIL
; Si hay solución: un nodo que cumple el test objetivo
;
;; Inicializar la lista de nodos open-nodes con el estado inicial
;; inicializar la lista de nodos closed-nodes con la lista vacía
;; recursión:
;;       si la lista open-nodes está vacía, terminar[no se han encontrado
;;                                                    solución]
;;       extraer el primer nodo de la lista open-nodes
;;       si dicho nodo cumple el test objetivo evaluar a la solución y terminar.
;;         en caso contrario
;;         si el nodo considerado no está en closed-nodes o, estando en
;;               dicha lista, tiene un coste g inferior al del que está en
;;               closed-nodes
;;               * expandir el nodo e insertar los nodos generados en
;;                    la lista open-nodes de acuerdo con la estrategia strategy.
;;               * incluir el nodo recién expandido al comienzo de la lista closed-nodes.
;;       Continuar la búsqueda eliminando el nodo considerado de la lista open-nodes.

(defun graph-search-aux (problem strategy open-nodes closed-nodes)
  (when open-nodes
    (let* ((actual-node (first open-nodes)) (dup-node (member actual-node closed-nodes)))
      (if (funcall (problem-f-goal-test problem) (node-state actual-node))
          actual-node
        (if (or (null dup-node) ;el nodo considerado no esta en la lista de abiertos
                (funcall (strategy-node-compare-p strategy) actual-node (first dup-node)))
            ;estando en la lista de abiertos el coste del nodo es menor
            (graph-search-aux problem strategy (remove actual-node 
                                                       (insert-nodes-strategy 
                                                        (expand-node actual-node problem)
                                                        open-nodes strategy))
                              (append closed-nodes (list actual-node))))))))
                              ;expandimos el nodo y los metemos en la lista de abiertos
        
                                 

          
    
  
;; Dado que el algoritmo se implementa en la funcion auxiliar aqui solo es necesario inicializar
;; las variables
(defun graph-search (problem strategy)
  (let ((h (funcall (problem-f-h problem) (problem-initial-state problem))))
  (graph-search-aux problem strategy (list (make-node
                                            :state (problem-initial-state problem)
                                            :parent NIL
                                            :action NIL
                                            :depth 0
                                            :g 0
                                            :h h
                                            :f h))
                                           nil)))
  ;Inicializa la lista de nodos abiertos con el estado inicial y la de cerrados a vacio





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; EJERCICIO 9
;;
;;
(defun a-star-search (problem)
  (graph-search problem *A-star*))

;;EJEMPLO
;;(a-star-search *galaxy-M35*)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; EJERCICIO 10
;;
;; FUNCION AUXILIAR
;; FUNCION tree-path-aux
;; DESCRIPCION: funcion que lleva la lista de nodos visitados, se consiguen desde 
;; el nodo objetivo hacia los padres generados
;; INPUT: node: nodo que se pretende insertar
;;        lst-states: lista ordenada de los estados que se han seguido hasta llegar a node
;; OUTPUT: lst-states completa
(defun tree-path-aux (node lst-states)
  (if (null (node-parent node))
      lst-states
    (tree-path-aux (node-parent node) (append (list (node-state (node-parent node))) lst-states)))) 
               

(defun tree-path (node)
  (when node
    (tree-path-aux node (list (node-state node)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; EJERCICIO 11
;;
;; FUNCION AUXILIAR
;; FUNCION action-secuence-aux
;; DESCRIPCION: funcion que lleva la lista de las acciones seguidas
;; INPUT: node: nodo que se pretende insertar
;;        lst-actions: lista ordenada de las acciones que se han seguido hasta llegar a node
;; OUTPUT: lst-actions completa
(defun action-secuence-aux (node lst-actions)
  (if (null (node-action (node-parent node)))
      lst-actions
    (action-secuence-aux (node-parent node) 
                         (append (list (node-action (node-parent node))) lst-actions))))
               

(defun action-secuence (node)
  (when node
    (action-secuence-aux node (list (node-action node)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; EJERCICIO 12
;;
;;
(setf *depth-first*
  (make-strategy
   :name 'depth-first
   :node-compare-p 'depth-first-node-compare-p))
(defun depth-first-node-compare-p (node-1 node-2)
  (<= (node-depth node-1) (node-depth node-2)))

(tree-path (graph-search *galaxy-M35* *depth-first*))


(setf *breadth-first*
 (make-strategy
  :name 'breadth-first
  :node-compare-p 'breadth-first-node-compare-p))

(defun breadth-first-node-compare-p (node-1 node-2)
  (>= (node-depth node-1) (node-depth node-2)))


(tree-path (graph-search *galaxy-M35* *breadth-first*))



