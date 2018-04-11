;;; ==========================================================================================
;;; PRACTICAS IA. PRACTICA 4 (JUEGOS)
;;;
;;; MANCALA - v.10.0 P2P 2017
;;; Implementa una version del juego Mancala con las variantes expuestas en la P4 de 2017
;;;
;;; NUEVO EN LA VERSION 10.
;;; - Acumulador revisado para que solo cuente fichas capturadas, ignorando las obtenidas de otros modos.
;;; - Se incluye mecanismo de fuga de fichas de Kalaha propio a hoyos contrarios.
;;;
;;; NUEVO EN LA VERSION 9.
;;; - Adaptado para Modern Lisp
;;;
;;; NUEVO EN LA VERSION 8.
;;; - 8.2: Al alcancar el maximo de jugadas: si alguien tiene 19+ gana, en caso contrario tablas.
;;; - 8.1: El acumulador cuenta solo fichas capturadas, no las ganadas por mala gestion contraria
;;; - Se rehabilita el Kalaha y el mecanismo de juego del Mancala original
;;; - Converge mas rapido por lo que se reduce a 50 el numero maximo de jugadas.
;;;
;;; NUEVO EN LA VERSION 7.
;;; - No hay modificaciones funcionales sobre la version 6.31, sin embargo, los
;;;   parametros del juego han sido elegidos para reducir la longitud de las
;;;   partidas, ello permite reducir a 100 el numero maximo de jugadas.
;;;
;;; NUEVO EN LA VERSION 6.31
;;; - el modo *verb* muestra tambien las jugadas de los jugadores automaticos
;;; - el bucle de lectura de jugadas permite modificar parametros con setq|setf, lo cual
;;;   permite modificar el nivel de verbosidad durante una partida
;;; - los nombres de jugadores que empezaban por *jugador... pasan a *jdr...
;;; - incorpora funciones P2P (requiere Allegro +8.2 y cargar mancala6_p2p_b.fasl)
;;;
;;; NUEVO EN LA VERSION 6.30
;;; - Incluye funciones de manejo de un tablero auxiliar
;;; - Evita que quien devuelva un n? negativo muy grande (que puede pasar por nil) obtenga
;;;   tablas. Del mismo modo, si el humano abandona no obtiene tablas sino que pierde.
;;;
;;; COMENTARIOS
;;; NOTA: Las normas del juego son las publicadas. Esta minidoc es solo a efectos de mantenimiento.
;;; - Jugar con profundidad 2 es mas que suficiente para evaluar jugadores y mucho mas rapido que 3 o 4
;;; - Resumen de la variante *m6* :
;;;   * Se permite mover en ambos sentidos, al modo de las damas holandesas.
;;;   * Si se termina de sembrar en un hoyo:
;;;      a) Vacio, termina la jugada actual.
;;;      b) Con menos de X fichas y el contrario tiene menos de Y en el hoyo y menos de Z
;;;         en total, se ceden al contrario nuestras fichas y termina la jugada actual.
;;;      c) Si no es aplicable (b) y hay fichas en el hoyo propio y en el contrario: se
;;;         roban al contrario sus fichas y se sigue sembrando.
;;;      d) En cualquier otro caso se termina la jugada actual.
;;;
;;; EJECUCION
;;; - Cargar el fichero
;;; - Ejecutar alguna de las partidas de prueba que hay al final del fichero
;;; - Puede iniciarse el juego en cualquier posicion, utilizando la variable opcional filas
;;; - Cargar version compilada para mayor eficiencia
;;;            (COMPILE-FILE "D:/ARCHI/uni/Doce/14/ia/p3/mancala8.cl")
;;;            (load "D:/ARCHI/uni/Doce/14/ia/p3/mancala8.fasl")
;;;
;;; NOTAS
;;;   (*) Alguna de las mejoras introducidas no se ha aplicado al Mancala original por lo que
;;;   de momento no estan operativas mas que las versiones Mancala2 y Mancala6
;;;
;;; ==========================================================================================

;;; ------------------------------------------------------------------------------------------
;;; VARIABLES GLOBALES
;;;                   Mancala2  Mancala6  Mancala7  Mancala/8
;;; *long-fila*          8         8         8         6
;;; *fichas-inicio*      3         3         3         3
;;; *fichas-jugador*    15        15        15        18
;;; *kalaha*            nil       nil       nil        t
;;; *inicio-siembra*     1         1         1         1
;;; *m6*                nil        t        nil       nil
;;; *2-sentidos*        nil        t        nil       nil
;;; ------------------------------------------------------------------------------------------

(in-package :common-graphics-user)
(defvar *vers*           "10.0") ; Version "x.y" = mancala X, rel. Y
(defvar *long-fila*        6   ) ; longitud de la fila de cada jugador, incl. el Kalaha en su caso
(defvar *fichas-inicio*    3   ) ; numero de fichas que hay inicialmente en cada hueco
(defvar *fichas-jugador*   18  ) ; numero total de fichas por jugador
(defvar *kalaha*           t   ) ; t=con hoyo almacen (Kalaha), nil=sin Kalaha
(defvar *inicio-siembra*   1   ) ; posicion desde la que se empieza a sembrar
(defvar *m6*               nil ) ; t=Mancala5
(defvar *2-sentidos*       nil ) ; Switch doble sentido: t=movimiento en ambos sentidos, nil=sentido unico
(defvar *verb*             t   ) ; Switch verbose: imprime comentarios sobre evolucion del programa
(defvar *verjugada*        t   ) ; Switch para ver cada jugada (nil = reduce print, p.e. en juego automatico
                                 ; y en su lugar saca una barra para seguir la evolucion de la actividad)
(defvar *vermarcador*      t   ) ; Switch para ver el marcador si *verjugada*=nil
(defvar *maxjugadas*       60  ) ; num maximo de jugadas antes de dar la partida por tablas
(defvar *tablas*           nil ) ; indicador de tablas
(defvar *k*   (if *kalaha* 1 0)) ; variable auxiliar para contar o no con Kalaha
(defvar *max-tengo*        4   ) ; maximo numero de fichas en ultimo hoyo para poder capturar
(defvar *tournament*       nil ) ; t=juego llamado desde torneo, nil=juego individual
(defvar *marcador*  (make-array '(2 2) :initial-element 0))
(defvar *prof*             0   ) ; Profundidad al expandir nodo
(defvar *tablero-aux*      nil ) ; Tablero auxiliar para uso discrecional del alumno (solo mediante funciones especificas)
(defvar *tablero-aux*      nil ) ; Tablero auxiliar para uso discrecional del alumno (solo mediante funciones especificas)
(defconstant +max-val+   99999 ) ; Valor maximo para negamax y otros usos
(defconstant +min-val+  -99999 ) ; Valor minimo para negamax y otros usos

(setf (symbol-function 'r) #'1+) ; Definicion del operador R (siembra a derechas)
(setf (symbol-function 'l) #'1-) ; Definicion del operador L (siembra a izquierdas)

;;; creacion de las cabeceras superior e inferior del tablero
(defvar +hdr1+ (format nil "~{ ~2D~}" (let ((result nil)) (dotimes (i *long-fila* result) (push i result)))))
(defvar +hdr0+ (format nil "~{ ~2D~}" (reverse (let ((result nil)) (dotimes (i *long-fila* result) (push i result))))))
;;; formatos para la impresion de lineas y espacios del tablero
(defvar +fmt1+ (concatenate 'string "~%  ~" (format nil "~A" (* *long-fila* 3)) "{-~}"))
(defvar +fmt2+ (concatenate 'string "~"     (format nil "~A" (* *long-fila* 3)) "{ ~}"))

(defvar *hist-1*)             (defvar *hist-2*)                (defvar *avge1*)              (defvar *avge2*)
(defvar *njugada*)            (defvar *jdr-humano*)            (defvar *jdr-human2*)         (defvar *jdr-aleatorio*)
(defvar *jdr-1st-opt*)        (defvar *jdr-last-opt*)          (defvar *jdr-nmx-bueno*)      (defvar *jdr-nmx-regular*)
(defvar *jdr-erroneo*)        (defvar *jdr-nmx-eval-aleatoria*)(defvar *logfile* t)          (defvar *timeout* 20)
(defvar *thumano* 900)        (defvar *debug-level* 2)         (defvar *debug-nmx* nil)
(defvar *boaster-remoto*)     (defvar *challenger-remoto*)

;;; Solo para versiones superiores a ACL 6.2
;;; Esto deberia funcionar, pero falla en ACL 6.2
;;; (if (packagep (find-package 'cg.base)) (import 'cg.base:copy-array))
;;; Pero ha hecho falta hacerlo asi para la ACL 6.2:
(format t "~2%Game v.~A " *vers*)
(cond ((find-package 'cg.base) (format t "cg.base already loaded or Lisp Ver. > 8~%"))
      (t (import 'copy-array (make-package 'cg.base))
         (export (find-symbol "COPY-ARRAY" 'cg.base) 'cg.base)))

;;; ------------------------------------------------------------------------------------------
;;; ESTRUCTURA PARA REPRESENTAR UN ESTADO DEL JUEGO
;;; ------------------------------------------------------------------------------------------
(defstruct estado
  tablero                        ; como esta el tablero en el momento
  lado-sgte-jugador              ; lado del tablero a cuyo jugador corresponde mover
  debe-pasar-turno               ; flag: t= EL siguiente jugador debe pasar turno, porque el otro
                                 ; realizo una accion que le permite volver a mover (Mancala1)
  accion)                        ; accion que ha llevado a la posicion actual

;;; ------------------------------------------------------------------------------------------
;;; ESTRUCTURA PARA REPRESENTAR UN JUGADOR (incl. P2P)
;;; ------------------------------------------------------------------------------------------
(defstruct jugador
  nombre                         ; nombre mostrado por pantalla para el jugador
  f-juego                        ; funcion de busqueda del jugador
  f-eval                         ; funcion de evaluacion del jugador: negamax u otra
  (host  "" :type string)        ; ""= no p2p, "a.b.c.d"=p2p local o remoto
  (port 0 :type integer))        ; puerto de comunicaciones (0=no p2p)

;;; ------------------------------------------------------------------------------------------
;;; FUNCIONES DE ACCESO AL TABLERO (ENCAPSULAN AREF EN LAS FUNCIONES DE BUSQUEDA)
;;; ------------------------------------------------------------------------------------------

;;; Pone cuantas fichas en determinada posicion del lado del tablero de un jugador
(defun put-fichas (tablero lado posicion cuantas)
  (setf (aref tablero lado posicion) cuantas))

;;; Obtiene el numero de fichas en determinada posicion del lado del tablero de un jugador
(defun get-fichas (tablero lado posicion)
  (aref tablero lado posicion))

;;; Anade una ficha en determinada posicion del lado del tablero de un jugador
(defun anade-ficha (tablero lado posicion)
  (setf (aref tablero lado posicion) (+ 1 (aref tablero lado posicion))))

;;; Anade cuantas fichas en determinada posicion del lado del tablero de un jugador
(defun anade-fichas (tablero lado posicion cuantas)
  (setf (aref tablero lado posicion) (+ cuantas (aref tablero lado posicion))))

;;; Cuenta las fichas que hay en la zona del tablero de un jugador desde la posicion desde hasta el final
(defun cuenta-fichas (tablero lado desde)
  (suma-fila tablero lado *long-fila* desde))

;;; Suma por filas los n elementos decimales de una matriz m de fils filas y cols columnas
;;; Devuelve una lista con fils valores. La lista esta en orden inverso.
;;; Uso: (suma-array (estado-tablero estado) 2 8) => (suma-fila1 suma-fila0)
(defun suma-array (m fils cols)
  (let ((sumas nil))
    (dotimes (i fils sumas)
      (push (suma-fila m i cols) sumas))))

;;; Suma los elementos desde a hasta de la fila fi de una matriz m [fils x cols]
;;; Rango de indices de una matriz de n cols = 0 a n-1. Eg: elementos 3+4+5 de la fila 0:
;;; (suma-fila (estado-tablero estado) 0 6 2)
(defun suma-fila (m fi &optional (hasta (+ *k* *long-fila*)) (desde 0) )
  (let ((suma 0))
    (do ((j desde (1+ j))) ((= (+ j desde) hasta)) (incf suma (aref m fi (+ j desde))))
    suma))

;;; Elimina las fichas que hay en la zona del tablero de un jugador
(defun limpia-fichas (tablero lado desde)
  (cond
   ((>= desde *long-fila*) nil)
   (t
    (put-fichas tablero lado desde 0)
    (limpia-fichas tablero lado (+ 1 desde)))))

;;; Funciones discrecionales del alumno para modificar un array auxiliar semejante al tablero

(defun reset-tablero-aux (&optional (x 0))
  "si *tablero-aux* existe lo reinicializa (si no, lo crea) inicializando a x"
  (if *tablero-aux*
      (dotimes (i (+ *long-fila* *long-fila*) *tablero-aux*) (setf (row-major-aref *tablero-aux* i) x))
    (setq *tablero-aux* (make-array (list 2 (+ *k* *long-fila*)) :initial-element x))))

(defun put-tablero-aux (lado posicion cuantas)
  (setf (aref *tablero-aux* lado posicion) cuantas))


;;; ------------------------------------------------------------------------------------------
;;; FUNCIONES DE INICIALIZACION DE TABLERO Y ESTADO
;;; ------------------------------------------------------------------------------------------

;;; Crea la lista para el initial-contents de la parte de tablero de un jugador
(defun construye-fila-tablero (long fichas)
  (cond
   ((= 0 long) (if *kalaha* '(0) nil))
   ( t (if (>= fichas *fichas-inicio*)
           (cons *fichas-inicio* (construye-fila-tablero (- long 1) (- fichas *fichas-inicio*)))
           (cons fichas (construye-fila-tablero (- long 1) fichas))))))

;;; Crea las listas para el initial-contents del tablero
(defun construye-tablero ()
  (let ((fila (construye-fila-tablero *long-fila* *fichas-jugador*)))
    (if (not *kalaha*) (setq fila (reverse fila)))
    (list fila fila)))

;;; Crea un estado inicial
(defun crea-estado-inicial (lado-sgte-jugador &optional filas)
  (make-estado
   :tablero (make-array (list 2 (+ *k* *long-fila*))
              :initial-contents (if (null filas) (construye-tablero) filas ))
   :lado-sgte-jugador lado-sgte-jugador
   :accion            nil))


;;; ------------------------------------------------------------------------------------------
;;; OTRAS FUNCIONES PARA TRABAJAR CON EL TABLERO
;;; ------------------------------------------------------------------------------------------
;;; Muestra el tablero
;;;   Jugador 1 : lado superior del tablero (i.e. invertido)
;;;   Jugador 0 : lado inferior del tablero

(defun muestra-tablero (estado &optional fin)
  (let ((jug-act (if fin 99 (estado-lado-sgte-jugador estado)))
        (tablero (estado-tablero estado)))
    (format t " ~%TABLERO: ")
    (format t "~2% ~A" +hdr1+)
    (format t +fmt1+ '(""))
    (print-lado (list-lado estado 1) jug-act 1)                  ; fila superior
    (when *kalaha*                                               ; fila central
      (format t "~%~2D" (aref tablero 1 *long-fila*))
      (format t +fmt2+ '(""))
      (format t "~2A" (aref tablero 0 *long-fila*)))
    (print-lado (reverse (list-lado estado 0)) jug-act 0)        ; fila inferior
    (format t +fmt1+ '(""))
    (format t "~% ~A" +hdr0+)))

;;; Imprime un lado del tablero y marca la posicion activa
(defun print-lado (lado jug-act posi)
  (format t "~%~A" (if (= jug-act posi) "*" " "))
  (format t "~{ ~2D~}" lado)
  (format t "     ~2D  Ac: ~2D" (get-pts posi) (get-tot posi)))

;;; Devuelve una lista con el estado de un lado del tablero en orden inverso
(defun list-lado (estado n)
  (let ((result nil))
    (dotimes (i *long-fila* result)
      (push (aref (estado-tablero estado) n i) result))))

;;; Devuelve la lista de posiciones, para un jugador, que tienen alguna ficha
(defun posiciones-con-fichas-lado (tablero lado desde)
  (cond
   ((>= desde *long-fila*) nil)
   ((< 0 (aref tablero lado desde))
    (cons desde (posiciones-con-fichas-lado tablero lado (+ 1 desde))))
   ( t
    (posiciones-con-fichas-lado tablero lado (+ 1 desde)))))

(defun combina-elt-lst (elt lst)
  "Devuelve las combinaciones del elemento atomico elt con una lista"
  (mapcar #'(lambda (x) (list elt x)) lst))

(defun combina-lst-lst (lst1 lst2)
  "Devuelve el producto cartesiano de dos listas"
  (unless (null lst1)
    (append
     (combina-elt-lst (first lst1) lst2)
     (combina-lst-lst (rest lst1) lst2))))

;;; ------------------------------------------------------------------------------------------
;;; FUNCIONES DE ACCESO AL MARCADOR (ENCAPSULAN AREF)
;;; ------------------------------------------------------------------------------------------

(defun reset-contadores (n)
  (reset-marcador)
  (setq *hist-1* (make-list n :initial-element *fichas-jugador*)
        *hist-2* (make-list n :initial-element *fichas-jugador*)
        *avge1* (* n *fichas-jugador*)
        *avge2* *avge1*
        *njugada* 0
        *tablas* nil ))

;;; Actualiza marcadores. Devuelve nil salvo si se pide marcador.
(defun act-marcador (tablero lado &key marcador)
  (let ((pts0 (suma-fila tablero 0))
        (pts1 (suma-fila tablero 1))
        (old-pts0 (get-pts 0))
        (old-pts1 (get-pts 1)))
    (when lado
      (set-pts 0 pts0)
      (set-pts 1 pts1)
      (when (and (> old-pts0 0) (> old-pts1 0))                  ; Acumula en total si no es inicio partida
        (set-tot lado (+ (get-tot lado)
                         (max (if (= lado 0) (- pts0 old-pts0) (- pts1 old-pts1)) 0)))))
    (when marcador (list pts0 pts1))))


;;; Funciones de acceso al marcador
(defun get-pts (i) (aref *marcador* 0 i))
(defun get-tot (i) (aref *marcador* 1 i))
(defun set-pts (i pts) (setf (aref *marcador* 0 i) pts))
(defun set-tot (i pts) (setf (aref *marcador* 1 i) pts))

(defun reset-marcador () (dotimes (i 4) (setf (row-major-aref *marcador* i) 0)))


;;; ------------------------------------------------------------------------------------------
;;; FUNCIONES FUNDAMENTALES DE LA PARTIDA
;;; ------------------------------------------------------------------------------------------

;;; Devuelve el jugador oponente
(defun lado-contrario (lado)
  (if (= lado 0) 1 0))

;;; Cambia el sgte-jugador de un estado, teniendo tambien en cuenta si debe pasar turno
;;; Devuelve el estado tras el cambio
(defun cambia-lado-sgte-jugador (estado debe-pasar-turno)
  (setf (estado-lado-sgte-jugador estado) (lado-contrario (estado-lado-sgte-jugador estado)))
  (setf (estado-debe-pasar-turno  estado) debe-pasar-turno)
  estado)


;;; Devuelve las acciones posibles para un jugador dado el estado
(defun acciones-posibles (estado)
  (if (estado-debe-pasar-turno estado)
      (list nil)
    (let ((l-acciones (posiciones-con-fichas-lado (estado-tablero estado)
                                                  (estado-lado-sgte-jugador estado) 0))
          (sentidos (if *2-sentidos* '(r l) '(r))))
          (combina-lst-lst sentidos l-acciones))))


;;; Hace una copia del estado
;;; TBD: revisar necesidad. copy-estado no crea copia del array.
(defun copia-estado (estado &optional accion)
  (make-estado
   :tablero                (cg.base:copy-array  (estado-tablero estado))
   :lado-sgte-jugador      (estado-lado-sgte-jugador estado)
   :accion                 (if accion accion (estado-accion estado))))

;;; Ejecuta una accion del juego
;;; Si la accion es nil pasa turno (cambia de lado)
;;; Devuelve un estado tras el cambio de lado

(defun ejecuta-accion (estado accion)
  (when *verb* (format t "~%Juega ~A " accion))
  (if  accion
      (let* ((fmov (first accion))
             (hoyo (second accion))
             (nuevo-estado (copia-estado estado accion))
             (tablero (estado-tablero nuevo-estado))
             (lado (estado-lado-sgte-jugador nuevo-estado))
             (numero-fichas (get-fichas tablero lado hoyo))
             (fichas (suma-fila tablero lado (1+ *long-fila*))))
        (put-fichas tablero (estado-lado-sgte-jugador estado) hoyo 0)
        ;(format t "~%  tablero pre  distribuye :~a" tablero)
        (let ((sigue? (distribuye-fichas tablero lado numero-fichas (mov fmov hoyo) t fmov)))
          (when (and (= (suma-fila tablero lado *long-fila*) 1)                                 ; V.10: Si el jugador solo tiene 1 ficha
                     (= (1+ (get-fichas tablero lado *long-fila*)) fichas)                      ; V.10: y no ha ganado nada en esta jugada
                     (> (get-fichas tablero (lado-contrario lado) *long-fila*) 3)               ; V.10: y el contrario tiene >3 en su Kahala
                     (> (suma-fila tablero (lado-contrario lado) *long-fila*) 10))              ; V.10: y mas de 10 en sus hoyos
             (show (format nil " => FUGA: 1 ficha, sin capturas, contrario con muchas fichas")) ; V.10
            (anade-fichas tablero (lado-contrario lado) *long-fila* -1)                         ; V.10: contrario pierde por Fuga de Kalaha
            (anade-fichas tablero lado 0 1))                                                    ; V.10: jugador gana por Fuga de Kalaha
          (cambia-lado-sgte-jugador nuevo-estado sigue?)))
    (cambia-lado-sgte-jugador (copia-estado estado accion) nil)))

(defun mov (fmov n)
  "aplica f. de movimiento a posicion n"
  (let ((newn (funcall fmov n)))
    (mod (if (< newn 0) (+ *k* *long-fila* newn) newn) (+ *k* *long-fila*))))


;;; Realiza recursivamente la siembra de fichas en una jugada
;;;
;;; tablero     : estruct del tablero a actualizar
;;; lado        : lado del tablero que juega
;;; cuantas     : numero de fichas a sembrar
;;; desde       : hoyo inicial de siembra
;;; es-su-zona  : t|nil segun el jugador este o no en su propia zona
;;; fmov        : funcion de movimiento para la siembra (def=1+)
;;;
;;; Devuelve t  : sigue jugando (porque deja la ultima ficha en el kalaha)
;;;          nil: no sigue y pasa turno al contrario

(defun distribuye-fichas (tablero lado cuantas desde &optional es-su-zona fmov)
  (cond
   ((and (>= desde *long-fila*) (> cuantas 1))                          ; caso en el que damos la vuelta
    (cond
     (*kalaha*                                                          ; si modalidad Kalaha
      (anade-ficha tablero lado *long-fila*)                            ; pasa sobre Kahala dejando una ficha
      (distribuye-fichas tablero (lado-contrario lado) (1- cuantas) 0 (not es-su-zona) fmov))
     (t (distribuye-fichas tablero lado cuantas 0 es-su-zona fmov))))
   ((<= cuantas 1)                                                      ; colocando la ultima ficha
    (let* ((tengo (get-fichas tablero lado desde))
           (hoyo-contrario (- *long-fila* desde 1))                     ; =-1 = caso Kalaha
           (captura (if (>= hoyo-contrario 0)                           ; Si fichas en contrario y no Kalaha (> -1)
                        (get-fichas tablero (lado-contrario lado) hoyo-contrario)
                      0)))                                              ; Ult. ficha en Kalaha => sin captura
      (when *verb* (format t " => Ultima ficha en ~D, con ~D, contra: ~D" desde tengo captura))
      (cond
       ((and *kalaha* (= desde *long-fila*))                            ; fin en Kalaha ...
        (anade-ficha tablero lado desde)
        (cond ((and es-su-zona (> (get-fichas tablero lado desde) 1))   ; Si es el propio y no vacio, juega de nuevo
               (show (format nil " => Kalaha y sigue jugando"))
               t)
              (t (show (format nil " => Kalaha contrario o vacio. Cede turno"))  ; Si estaba vacio o es K contrario, termina
                 nil)))
       ;; caso en que ultima ficha cae en propia casilla vacia (Mancala) o llena (Mancala2+)
       ((and es-su-zona (funcall (if *kalaha* #'= #'>) tengo 0))
        (cond
         (*kalaha*                                                      ; Mancala/8/9 (cada version con codigo distinto)
          (cond
           ((> captura 0)                                               ; Si hay captura roba y termina
            (show (format nil " => Captura ~D y termina" captura))
            (anade-fichas tablero lado *long-fila* (+ 1 captura))
            (put-fichas tablero (lado-contrario lado) hoyo-contrario 0)
            nil)
           (t (anade-fichas tablero lado desde cuantas)                 ; Si no hay captura termina
              nil)))
         (t                                                             ; Mancala2+
          (let ((total-contr (suma-fila tablero (lado-contrario lado))))
            (cond                                                       ; Hay entrega o captura?
             ((and *m6* (> tengo 4)(< captura 3)(< total-contr *long-fila*)(> total-contr 0)) ; Entrega
              (put-fichas tablero lado desde 0)
              (put-fichas tablero (lado-contrario lado) hoyo-contrario (+ captura 1 tengo))
              (show (format nil " (Total ~D). Entrega" total-contr) tablero)
              nil)
             ((and (> tengo 0) (< tengo *max-tengo*) (> captura 0))     ; Captura
              (anade-ficha tablero lado desde)
              (put-fichas tablero (lado-contrario lado) hoyo-contrario 0)
              (show (format nil "~% Captura ~D y siembra-~A" captura fmov))
              (distribuye-fichas tablero lado captura (mov fmov desde) es-su-zona fmov))
             (t (anade-ficha tablero lado desde)
                (show (format nil ". Ni captura ni entrega") tablero)
                nil))))))
       ((and (not es-su-zona) *kalaha* (> tengo 5) (= captura 0))       ; Robo especifico Mancala9: termina en contrario>5 y propias=0
        ;;(format t "~%Robo especifico M10: sembrando en contrario tengo=0 (~A) y contrario>5 (~A)" captura tengo)
        (put-fichas tablero lado desde 0)                               ; Roba a contrario y termina
        (put-fichas tablero (lado-contrario lado) hoyo-contrario (+ tengo 1))
        (show (format nil " => Roba (Entrega Inversa) ~D+1 del contrario y cede turno." tengo))
        nil)
       (t                                                               ; cualq.otro caso en q pone ultima ficha
        (anade-ficha tablero lado desde)
        (show "" tablero)
        nil))))
   (t                                                                   ; cualq.otro caso en q pone 1 ficha (no ultima)
    (anade-ficha tablero lado desde)
    (distribuye-fichas tablero lado (1- cuantas) (mov fmov desde) es-su-zona fmov)) ))

(defun show (txt &optional tablero)
  "muestra situacion para seguimiento juego"
  (when *verb*
    (format t "~A~A" txt (if tablero
                             (format nil ": ~a" (act-marcador tablero nil :marcador t))
                           ","))))

;;; Genera los posibles sucesores de un estado
;;; SIDEFECT: A efectos de trazabilidad (No necesario para el juego), guarda la profundidad.
(defun generar-sucesores (estado &optional profundidad)
  (when *verb* (format t "~% -------- Gen.Sucesores de ~a    [Prof:~a]" (estado-tablero estado)
                 (if profundidad (progn (setq *prof* 0) profundidad)
                                       (format nil "0.~3,'0d" (incf *prof*)))))
  (unless (juego-terminado-p estado)
    (mapcar #'(lambda(x) (ejecuta-accion estado x)) (acciones-posibles estado))))

;;; Pide una accion al usuario entre las posibles
(defun pide-accion (posibles-acc)
  (format t "~%Jugador Humano. ~%  Elija entre: ~A~%  o en modo abreviado: ~A~%  Introduzca jugada o x para terminar : "
    posibles-acc (abreviado posibles-acc))
  (let ((input (read)))
    (unless (eq 'x input)
      (let ((accion (if (numberp input)
                        (list (if (>= (signum input) 0) 'r 'l) (abs input))
                      input)))
        (cond
         ((and (listp accion)
               (or (eq (car accion) 'setq) (eq (car accion) 'setf))
               (member (second accion) '(*verb* *verjugada* *vermarcador*)))
          (eval accion) (format t "~%OK")
          (pide-accion posibles-acc))
         ((member accion posibles-acc :test #'equal) accion)
         (t (format t "~%Sintaxis: (S P)|PA  Donde S=sentido siembra, P=posicion valida, PA=posicion en modo abreviado ~%~10TModo abreviado: n>=0 = (R |n|), n<0 = (L |n|)" )
            (pide-accion posibles-acc)))))))

;;; Presenta en modo abreviado las posibles acciones
(defun abreviado (posibles-acc)
  (mapcar #'(lambda (x) (ccase (car x)
                          ('r (second x))
                          ('l (if (= (second x) 0) x (- (second x))))))
    posibles-acc))


;;; Funcion predicado que comprueba si el estado es de fin de partida
(defun juego-terminado-p (estado)
  (let ((tablero (estado-tablero estado)))
    (or
     (<= (suma-fila tablero 0 *long-fila*) (- 1 *k*))
     (<= (suma-fila tablero 1 *long-fila*) (- 1 *k*)))))


;;; Dadas dos puntuaciones devuelve t si una de las dos obtiene una media
;;; de 2 puntos en las ultimas N jugadas o si sobrepasan el *maxjugadas*
;;;
;;; Notese que para evitar realizar divisiones en cada ciclo lo que se hace es comparar
;;; no las medias con 2 sino la suma con un numero igual a (* 2 *long-fila*), que tambien
;;; coincide con el numero de jugadas en la que se comprueba la media.
;;; En el caso de Kahala se resta, pues lo que importa son los hoyos normales.

(defun tablas-p (tablero)
  (let* ((n (* 2 *long-fila*))
         (i (mod *njugada* n))
         (pts1-k (if *kalaha* (- (get-pts 0) (aref tablero 0 *long-fila*)) (get-pts 0)))
         (pts2-k (if *kalaha* (- (get-pts 1) (aref tablero 1 *long-fila*)) (get-pts 1))))
    (setf *avge1* (+ *avge1* (- (nth i *hist-1*)) pts1-k)
      *avge2* (+ *avge2* (- (nth i *hist-2*)) pts2-k)
      (nth i *hist-1*) pts1-k
      (nth i *hist-2*) pts2-k)
    ;;(format t "~% [J ~3D] 0: ~A = ~A~%         1: ~A = ~A " *njugada* *hist-2* *avge2* *hist-1* *avge1*)
    (cond
     ((or (>= *njugada* *maxjugadas*) (<= *avge1* n) (<= *avge2* n) )
      (setq *tablas* t))
     (t (incf *njugada*)
        (setq *tablas* nil)))))


;;; Informa al usuario que ha terminado el juego, mostrando el marcador
(defun informa-final-de-juego (estado lst-jug &optional msg winner)
  (let* ((pts0 (get-pts 0))
         (pts1 (get-pts 1))
         (tablero (estado-tablero estado))
        (ganador (cond
                  (*tablas*
                   (if *kalaha*
                       (cond       ;; Si Kalaha y tablas, comprueba si alguno tiene mas de la mitad en su Kalaha
                        ((> (aref tablero 0 *long-fila*) *fichas-jugador*) 1)
                        ((> (aref tablero 1 *long-fila*) *fichas-jugador*) 2)
                        (t 0))
                     0))
                  (msg winner)
                  ((< pts0 pts1) 2)
                  ((> pts0 pts1) 1)
                  (t 0 ))))
    (when (and (> *debug-level* 1) (not *tournament*))
      (format t "~2%  FIN DEL JUEGO por ~A en ~A Jugadas~%  Marcador:  ~A ~A - ~A ~A~%~%"
        (if (= ganador 0) "TABLAS" "VICTORIA")
        *njugada*
        (jugador-nombre (first lst-jug))
        pts0
        pts1
        (jugador-nombre (second lst-jug))))
    (values ganador nil)))


;;; ------------------------------------------------------------------------------------------
;;; FUNCION PRINCIPAL PARA REALIZAR UNA PARTIDA ENTRE DOS JUGADORES
;;; ------------------------------------------------------------------------------------------
;;; RECIBE:
;;;  - lado:  Lado del tablero a cuyo jugador le corresponde comenzar a jugar
;;;           0=2=Jugador1 (abajo);  1=Jugador2 (arriba)
;;;  - profundidad-max: maxima profundidad de la busqueda negamax
;;;  - lst-jug-partida: (Jugador1 Jugador2)
;;;    Lista compuesta por los dos structs jugador que tomaran parte en la partida.
;;;  - filas: Parametro opcional que fuerza situacion inicial tablero
;;; DEVUELVE: resultado de la partida (0=tablas, 1=gana Jugador1, 2=gana Jugador2)
;;; ------------------------------------------------------------------------------------------

(defun partida (lado profundidad-max lst-jug &optional filas)
  (let* ((lado-01 (mod lado 2))
         (estado (crea-estado-inicial lado-01 filas))
         (boast (/= (jugador-port (second lst-jug)) 0))
         (chall (/= (jugador-port (first lst-jug)) 0)) )
    (reset-contadores (* 2 *long-fila*))
    (if (or *tournament* (and (< *debug-level* 2) (not boast) (not chall)))
        (setq *verjugada* nil *vermarcador* nil)
      (if *verjugada* (format t "~%  Juego: (1) ~a vs ~a (2) "
                        (jugador-nombre (first lst-jug)) (jugador-nombre (second lst-jug)))))

    (cond
     ((and chall boast)
      (@stop "Ambos jugadores son remotos. Uno de ellos debe ser local"))

     (chall                                                             ; Challenger Role
      (setf (jugador-host *boaster-remoto*) *bhost*)                    ; recarga por si ha cambiado
      (chall-p2p-loop estado lado-01 profundidad-max lst-jug))          ; TBD take role out

     (boast                                                             ; Boaster Role
      (let ((bname (fullname (jugador-nombre (first lst-jug)))))
        (format t "~%Boaster ~A registers in majordomo, port ~A " bname *mport*)
        (setf (get '*bname* :plyr) bname)
        (subscr-bst bname "new")
        (with-open-stream (bstr-skt (socket:make-socket :connect :passive :local-port *bport*))
          (socket:set-socket-options bstr-skt :reuse-address t :nodelay t)
          (loop for i from 1 to *bgames* do
                (delayed-refresh)
                (reset-contadores (* 2 *long-fila*))
                (let ((chall-rslt (boast-p2p-loop estado lado-01 profundidad-max lst-jug bstr-skt)))

                  ;; Reject game                       : (cha    msg      vers)
                  ;; Normal game result                : (cha    0|1|2    vers)
                  ;; Error  game result                : (cha (0|1|2 msg) vers)
                  ;; wich are completed & sent to mj as: (n boa cha msg vers mv# gm#)  or
                  ;;  (n boa cha 0|1|2 vers mv# gm#) or  (n boa cha (0|1|2 msg) vers mv# gm# vers)

                  (format t "~%  Game ~a:  ~s after ~a moves~%" i chall-rslt *njugada*)
                  (cond
                   ((or (null chall-rslt) (not (listp chall-rslt)))
                    (format t "~%FATAL: chall-rslt not a list. Ignored (~s)." chall-rslt)
                    (submit-error (list bname "n/a" (list 0 "BST ERROR") *vers* *njugada* (- *bgames* i))))
                   ((stringp (second chall-rslt))
                    (format t "~%  Reject: ~s" chall-rslt)
                    (submit-reject (cons bname (append chall-rslt (list *njugada* *bgames*)))))
                   ((listp (second chall-rslt))
                    (format t "~%Error in player: ~s~%" chall-rslt)
                    (submit-error (cons bname (append chall-rslt (list *njugada* (- *bgames* i)))   )))
                   ((numberp (second chall-rslt))
                    (submit-result (cons bname (append chall-rslt (list *njugada* (- *bgames* i))))))
                   (t (format t "~%FATAL: invalid chall-rslt. Ignored (~s)." chall-rslt))))))))
     (t (local-loop estado lado-01 profundidad-max lst-jug)))))


;;; =======================================================================================
;;: LOOP LOCAL
;;; =======================================================================================

(defun local-loop (estado lado-01 profundidad-max lst-jug)
  "bucle de movimientos alternos hasta conclusion de la partida"
  (when *verb* (format t "~%Local game ~A-~A depth=~A " (jugador-nombre (first lst-jug)) (jugador-nombre (second lst-jug)) profundidad-max))
  (loop
    (let ((ult-lado (mod (+ 1 lado-01) 2)))
      (act-marcador (estado-tablero estado) ult-lado)
      (when (and *verb* (estado-accion estado))
        (format t "~%[J ~A] ~A juega ~A "
          *njugada* (jugador-nombre (nth ult-lado lst-jug)) (estado-accion estado))))
    (let ((curr-plyr (nth lado-01 lst-jug)))
        (cond
         ((or (juego-terminado-p estado)                                ; si juego terminado o tablas
              (tablas-p (estado-tablero estado)))
          (when *verjugada* (muestra-tablero estado t))
          (return (informa-final-de-juego estado lst-jug)))
         (t                                                             ; llamada al jugador que tiene el turno
          (cond
           ((estado-debe-pasar-turno estado)
            (when *verb* (format t "~%[J ~A] ~A pasa turno." *njugada* (jugador-nombre curr-plyr)))
            (cambia-lado-sgte-jugador  estado  nil)
            (setf (estado-accion estado) nil))
           (t
            (if *verjugada*
                (progn
                  (muestra-tablero estado)
                  (format t "~%[J ~A] El turno es de ~A~%" *njugada* (jugador-nombre curr-plyr)))
              (if *vermarcador*
                  (format t "~%~3d ~a-~a" *njugada* (get-pts 0) (get-pts 1))
                (when (= (mod *njugada* 10) 0)
                  (to-logfile (format nil " ~d" *njugada*) 4 t))))
            (setf estado
              (mp:with-timeout ((get-timeout curr-plyr) (to-logfile " T-Out " 2 t) 'timeout)
                (ignore-errors (funcall (jugador-f-juego curr-plyr)
                                        estado
                                        profundidad-max
                                        (jugador-f-eval curr-plyr)))))))
          (when (null estado)                                           ; => abandono, error o return nil|-infinito
            (return (values (winner (nth lado-01 lst-jug) lst-jug) "Error en func. o abandono")))
          (when (eql estado 'timeout)                                   ; timeout de jugada
            (return (values (winner (nth lado-01 lst-jug) lst-jug) "Timeout jugada")))
          (setf lado-01 (mod (1+ lado-01) 2)))))))                      ; inversion: pasa al otro jugador / convierte 1-2 0-1




(defun get-timeout (plyr)
  (cond
   ((eq (jugador-f-juego plyr) #'f-j-humano) *thumano*)
   (t *timeout*)))


(defun winner (loser lst-jug)
  "Devuelve quien es el ganador de la partida"
  (if (eq (jugador-nombre loser) (jugador-nombre (first lst-jug))) 2 1))


;;; ------------------------------------------------------------------------------------------
;;; FUNCIONES DE BUSQUEDA
;;; ------------------------------------------------------------------------------------------

;;; ------------------------------------------------------------------------------------------
;;; ALGORITMO NEGAMAX
;;; ------------------------------------------------------------------------------------------
;;; Funcion que inicia la busqueda y devuelve el siguiente estado elegido por el jugador que
;;; tiene el turno, segun algoritmo negamax
;;; RECIBE:   estado,
;;;           profundidad-max    : limite de profundidad
;;;           f-eval             : funcion de evaluacion
;;; DEVUELVE: estado siguiente del juego
;;; ------------------------------------------------------------------------------------------
(defun negamax (estado profundidad-max f-eval)
  (let* ((oldverb *verb*)
         (*verb* (if *debug-nmx* *verb* nil))
         (estado2 (negamax-1 estado 0 t profundidad-max f-eval))
         (*verb* oldverb))
    estado2))

;;; ------------------------------------------------------------------------------------------
;;; Funcion auxiliar negamax-1
;;; RECIBE:   estado, profundidad-actual,
;;;           devolver-movimiento: flag que indica si devolver un estado (llamada raiz) o un valor numerico (resto de llamadas)
;;;           profundidad-max    : limite de profundidad
;;;           f-eval             : funcion de evaluacion
;;; DEVUELVE: valor negamax en todos los niveles de profundidad, excepto en el nivel 0 que devuelve el estado del juego tras el
;;;           movimiento que escoge realizar
;;; ------------------------------------------------------------------------------------------
(defun negamax-1 (estado profundidad devolver-movimiento profundidad-max f-eval)
  (cond ((>= profundidad profundidad-max)
         (unless devolver-movimiento  (funcall f-eval estado)))
        (t
         (let ((sucesores (generar-sucesores estado profundidad))
               (mejor-valor +min-val+)
                (mejor-sucesor nil))
           (cond ((null sucesores)
                  (unless devolver-movimiento  (funcall f-eval estado)))
                 (t
                  (loop for sucesor in sucesores do
                    (let* ((result-sucesor (- (negamax-1 sucesor (1+ profundidad)
                                        nil profundidad-max f-eval))))
                      ;(format t "~% Nmx-1 Prof:~A result-suc ~3A de suc ~A, mejor=~A" profundidad result-sucesor (estado-tablero sucesor) mejor-valor)
                      (when (> result-sucesor mejor-valor)
                        (setq mejor-valor result-sucesor)
                        (setq mejor-sucesor  sucesor))))
                  (if  devolver-movimiento mejor-sucesor mejor-valor)))))))


;;; ------------------------------------------------------------------------------------------
;;; Implementacion del algoritmo negamax con poda alfa-beta
;;; ------------------------------------------------------------------------------------------


;;; ------------------------------------------------------------------------------------------
;;; ALGORITMO NEGAMAX
;;; ------------------------------------------------------------------------------------------
;;; Funcion que inicia la busqueda y devuelve el siguiente estado elegido por el jugador que
;;; tiene el turno, segun algoritmo negamax
;;; RECIBE:   estado,
;;;           profundidad-max    : limite de profundidad
;;;           f-eval             : funcion de evaluacion
;;; DEVUELVE: estado siguiente del juego
;;; ------------------------------------------------------------------------------------------

(defun negamax-a-b (estado profundidad-max f-eval)
  (let* ((oldverb *verb*)
         (*verb* (if *debug-nmx* *verb* nil))
         (estado2 (negamax-a-b-aux estado 0 t profundidad-max f-eval +min-val+ +max-val+))
         (*verb* oldverb))
    estado2))

; Funcion auxiliar que nos sirve, al igual que en el caso sin poda para la recursion
; pero que ademas nos deja llevar la cuenta de los parametros a(alpha) u b (beta) 
; necesarios para la poda.
(defun negamax-a-b-aux (estado profundidad devolver-movimiento profundidad-max f-eval a b)
  
  ;caso parada, fin arbol
  (cond ((>= profundidad profundidad-max)
         (unless devolver-movimiento  (funcall f-eval estado)))
        (t 
         (let ((sucesores (generar-sucesores estado profundidad))
               (mejor-valor +min-val+)
               (mejor-sucesor nil))
           (cond ((null sucesores) ;;continuamos el algoritmno si existen sucesores
                  (unless devolver-movimiento  (funcall f-eval estado)))
                 (t
                  (loop for sucesor in sucesores do
                        ;iteramos el algoritmo en profundidad
                        (let* ((result-sucesor (- (negamax-a-b-aux sucesor (1+ profundidad)
                                                                   nil profundidad-max f-eval (- b) (- a)))))
                          ;reasignamos los valores necesarios
                          (setq a (max a result-sucesor))
                          (when (> result-sucesor mejor-valor)
                            (setq mejor-valor result-sucesor)
                            (setq mejor-sucesor  sucesor))
                          ;comprobamos si hacemos poda
                          (when (>= a b)
                            (loop-finish))))
                  ;(format t "?% Nmx-1 Prof:?A result-suc ?3A de suc ?A, mejor=?A" profundidad result-sucesor (estado-tablero sucesor) mejor-valor)
                  (if  devolver-movimiento
                      mejor-sucesor
                    mejor-valor)))))))

;;; ------------------------------------------------------------------------------------------
;;; FUNCIONES AUXILIARES
;;; ------------------------------------------------------------------------------------------


;;; Si el log es a fichero y este no esta abierto, informa del error y lo intenta en consola
;;; con una llamada recursiva. Evita bucle infinito poniendo a t from-error.
(defun to-logfile (msg lvl &optional contline from-error)
  (when (<= lvl *debug-level*)
    (if (ignore-errors
         (if contline
             (format *logfile* "~A" msg)
           (multiple-value-bind (ss mn hh dd mm yy) (get-decoded-time)
             (if (= lvl 0)
                 (format *logfile* "~%~2,'0D~2,'0D~2,'0D ~2,'0D~2,'0D~2,'0D ~A"
                   hh mn ss (- yy 2000) mm dd msg)
               (format *logfile* "~%~2,'0D~2,'0D~2,'0D ~A" hh mn ss msg))))
         (force-output *logfile*)
         t)
        msg
      (unless from-error
        (format t "~%ERROR escritura en ~a. ~%Reintento a consola." *logfile*)
        (setq *logfile* t)
        (to-logfile msg lvl contline t)))))

;;; Aborta la ejecucion                                                         [AdS 2008]
;;; ------------------------------------------------------------------------------------------
(defun @stop (&optional (msg "Abortado por usuario"))
 (handler-bind ((error #'invoke-debugger))
                      (error msg)))

;;; ------------------------------------------------------------------------------------------
;;; FUNCIONES DE DEFINICION DE JUGADORES Y PARTIDAS DE PRUEBA
;;; ------------------------------------------------------------------------------------------

;;; f-juego que utiliza busqueda negamax (con o sin poda)
;;; ------------------------------------------------------------------------------------------
(defun f-j-nmx (estado profundidad-max f-eval)
  (negamax-a-b estado profundidad-max f-eval))
  ;;;(negamax estado profundidad-max f-eval))


;;; f-juego para jugador chulo remoto (boaster)
;;; La funcion es realmente un dummy que no deberia ejecutar nunca
;;; ------------------------------------------------------------------------------------------
(defun f-j-rmt-boast (estado &optional profundidad-max f-eval)
  (and profundidad-max f-eval)      ; dummy to avoid compiler warnings
  (format t "~%  jugador-boaster: No deberia ejecutarme!!~%  estado= ~s~%" estado)
  estado)


;;; f-juego controlado por un humano
;;; ------------------------------------------------------------------------------------------
(defun f-j-humano (estado &optional profundidad-max f-eval)
  (and profundidad-max f-eval)      ; dummy to avoid compiler warnings
  (setq *verjugada* t)
  (let ((accion (pide-accion (acciones-posibles estado))))
    (unless (null accion) (ejecuta-accion estado accion))))

(setf *jdr-humano* (make-jugador
                        :nombre   '|Humano|
                        :f-juego  #'f-j-humano
                        :f-eval   nil))

(setf *jdr-human2* (make-jugador
                        :nombre   '|Human2|
                        :f-juego  #'f-j-humano
                        :f-eval   nil))


;;; f-juego para un jugador que realiza movimientos aleatorios
;;; ------------------------------------------------------------------------------------------
(defun f-j-aleatorio (estado &optional profundidad-max f-eval)
  (and profundidad-max f-eval)      ; dummy to avoid compiler warnings
  (let ((lst-acciones (acciones-posibles estado)))
    (ejecuta-accion estado (nth (random (length lst-acciones)) lst-acciones))))

;;; Jugador que no evalua y juega aleatoriamente
;;; ------------------------------------------------------------------------------------------
(setf *jdr-aleatorio* (make-jugador
                        :nombre   '|Ju-Aleatorio|
                        :f-juego  #'f-j-aleatorio
                        :f-eval   nil))


;;; f-juego para un jugador que siempre juega la primera opcion
;;; ------------------------------------------------------------------------------------------
(defun f-j-1st-opt (estado &optional profundidad-max f-eval)
  (and profundidad-max f-eval)      ; dummy to avoid compiler warnings
  (ejecuta-accion estado (first (acciones-posibles estado))))

;;; Jugador que no evalua y juega la primera opcion disponible
;;; ------------------------------------------------------------------------------------------
(setf *jdr-1st-opt* (make-jugador
                        :nombre   '|Ju-1st-Opt|
                        :f-juego  #'f-j-1st-opt
                        :f-eval   nil))


;;; f-juego para un jugador que siempre juega la ultima opcion
;;; ----------------------------------------------------------
(defun f-j-last-opt (estado &optional profundidad-max f-eval)
  (and profundidad-max f-eval)      ; dummy to avoid compiler warnings
  (ejecuta-accion estado (car (last (acciones-posibles estado)))))

;;; Jugador que no evalua y juega la segunda opcion disponible (si puede)
;;; ------------------------------------------------------------------------------------------
(setf *jdr-last-opt* (make-jugador
                        :nombre   '|Ju-Last-Opt|
                        :f-juego  #'f-j-last-opt
                        :f-eval   nil))


;;; Funcion de evaluacion que da al estado recibido una puntuacion aleatoria
;;; ------------------------------------------------------------------------------------------
(defun f-eval-aleatoria (estado)
  (when estado t)                   ; dummy para evitar warnings de compilador
  (random 100) )

;;; f-juego que utiliza busqueda negamax pero evalua aleatoriamente
;;; ------------------------------------------------------------------------------------------
(setf *jdr-nmx-eval-aleatoria* (make-jugador
                        :nombre   '|Ju-Nmx-Eval-Aleatoria|
                        :f-juego  #'f-j-nmx
                        :f-eval   #'f-eval-aleatoria))

;;; f-juego que aborta
;;; ------------------------------------------------------------------------------------------
(defun f-eval-erronea (estado)
  (when estado t)                   ; dummy para evitar warnings de compilador
  (/ 1 0))

(setf *jdr-erroneo* (make-jugador
                        :nombre   '|Ju-Erroneo|
                        :f-juego  #'f-j-nmx
                        :f-eval   #'f-eval-erronea))

;;; Jugador Bueno (debido a que juega con un nivel mas de evaluacion)
;;; ------------------------------------------------------------------------------------------
(defun f-eval-Bueno (estado)
  (if (juego-terminado-p estado)
      -50                              ;; Condicion especial de juego terminado
    ;; Devuelve el maximo del numero de fichas del lado enemigo menos el numero de propias
    (max-list (mapcar #'(lambda(x)
                          (- (suma-fila (estado-tablero x) (lado-contrario (estado-lado-sgte-jugador x)))
                                    (suma-fila (estado-tablero x) (estado-lado-sgte-jugador x))))
                (generar-sucesores estado)))))

(setf *jdr-nmx-Bueno* (make-jugador
                        :nombre   '|Ju-Nmx-Bueno|
                        :f-juego  #'f-j-nmx
                        :f-eval   #'f-eval-Bueno))

;;; Devuelve el top segun test de una lista de nos. y su posicion
(defun max-list (l &key (test #'max))
  (let ((m (reduce test l)))
    (values m (position m l))))

;;; (max-list '(2 5 3 6 2 0))  -> 6, 3
;;; (max-list '(2 5 3 6 2 0) :test #'Min)  -> 0, 5


;;; Jugador Regular
;;; ------------------------------------------------------------------------------------------
(defun f-eval-Regular (estado)
  (- (suma-fila (estado-tablero estado) (estado-lado-sgte-jugador estado))
    (suma-fila (estado-tablero estado) (lado-contrario (estado-lado-sgte-jugador estado)))))

(setf *jdr-nmx-Regular* (make-jugador
                        :nombre   '|Ju-Nmx-Regular|
                        :f-juego  #'f-j-nmx
                        :f-eval   #'f-eval-Regular))


;;; Jugador Propio
;;; ------------------------------------------------------------------------------------------


(defun robar-p (estado))
  

(defun primer-turno-p(estado)
  
  (and  
   (mapcan #'(lambda (x y) (eql x (get-fichas (estado-tablero estado) (estado-lado-sgte-jugador estado) y)))
     '(3 3 3 0 4 4) '(0 1 2 3 4 5))
   (mapcan #'(lambda (y) (eql 3 (get-fichas (estado-tablero estado) (lado-contrario (estado-lado-sgte-jugador estado)) y)))
     '(0 1 2 3 4 5))))

(defun mi-f-ev-primer (estado) 
  
  (if (primer-turno-p estado)
      99999
    (- (+(get-pts (estado-lado-sgte-jugador estado)) ;obtiene las fichas del jugador con el turno
          (get-fichas (estado-tablero estado) (estado-lado-sgte-jugador estado) 6)) ;obtiene las fichas del kalaha del jugador con el turno
       
       ;;;tener fichas en la posicion 5 y 4 es malo
       (*(get-fichas (estado-tablero estado) (estado-lado-sgte-jugador estado) 5) 1)
       (*(get-fichas (estado-tablero estado) (estado-lado-sgte-jugador estado) 4) 1)
       (+(get-pts (lado-contrario (estado-lado-sgte-jugador estado))) ;obtiene las fichas del jugador que no tiene el turno
          (get-fichas (estado-tablero estado) (lado-contrario (estado-lado-sgte-jugador estado)) 6)) ;obtiene las fichas del kalaha del jugador sin el turno
            
       ;;;que tenga fichas el adversario en la posicion 5 y 4 es bueno
       (-(*(get-fichas (estado-tablero estado) (lado-contrario (estado-lado-sgte-jugador estado)) 5) 1))
       (-(*(get-fichas (estado-tablero estado) (lado-contrario (estado-lado-sgte-jugador estado)) 4) 1)))))
            




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mi-f-ev-Sirius (estado)
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
     (* (- (if (= 0 (get-fichas tablero p1 0)) 1 0) (if (= 0 (get-fichas tablero p0 5)) 1 0)) 3)
     (* (- (if (= 0 (get-fichas tablero p1 1)) 1 0) (if (= 0 (get-fichas tablero p0 4)) 1 0)) 3)
     (* (- (if (= 0 (get-fichas tablero p1 2)) 1 0) (if (= 0 (get-fichas tablero p0 3)) 1 0)) 5)
     (* (- (if (= 0 (get-fichas tablero p1 3)) 1 0) (if (= 0 (get-fichas tablero p0 2)) 1 0)) 5)
     (* (- (if (= 0 (get-fichas tablero p1 4)) 1 0) (if (= 0 (get-fichas tablero p0 1)) 1 0)) 3)
     (* (- (if (= 0 (get-fichas tablero p1 5)) 1 0) (if (= 0 (get-fichas tablero p0 0)) 1 0)) 3))))


(setf *Sirius* 
  (make-jugador
   :nombre 'Sirius
   :f-juego #'f-j-nmx
   :f-eval #'mi-f-ev-Sirius))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mi-f-ev-OLDSeverus (estado)
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

(setf *OLDSeverus* 
  (make-jugador
   :nombre 'OLDSeverus
   :f-juego #'f-j-nmx
   :f-eval #'mi-f-ev-OLDSeverus))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mi-f-ev-Referencia (estado)
    ;mis fichas menos las del contrario
  (let ((tablero (estado-tablero estado)) (p0 (estado-lado-sgte-jugador estado))
        (p1 (lado-contrario (estado-lado-sgte-jugador estado))))
  (+ 
     ;las fichas de mi kahala menos las del contrario
     (* (- (get-fichas tablero p0 6) (get-fichas tablero p1 6)) 10)
     ;las fichas en la posicion 5 del contrario menos las mias
     ;consideramos que es malo acumular demasiadas fichas en esta posicion
     (*(- (get-fichas tablero p1 5) (get-fichas tablero p0 5)) 6)
     ;las fichas en la posicion 4 del contrario menos las mias
     ;consideramos que es malo acumular demasiadas fichas en esta posicion
   (*(- (get-fichas tablero p1 4) (get-fichas tablero p0 4)) 5)
   (*(- (get-fichas tablero p1 3) (get-fichas tablero p0 3)) 4)
   (*(- (get-fichas tablero p1 2) (get-fichas tablero p0 2)) 3)
   (*(- (get-fichas tablero p1 1) (get-fichas tablero p0 1)) 2)
   (*(- (get-fichas tablero p1 0) (get-fichas tablero p0 0)) 1))))

(setf *REferencia* 
  (make-jugador
   :nombre 'Referencia
   :f-juego #'f-j-nmx
   :f-eval #'mi-f-ev-Referencia))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun mi-f-ev-Severus (estado)
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
     (* (- (if (= 0 (get-fichas tablero p1 0)) 2 0) (if (= 0 (get-fichas tablero p0 5)) 1 0)) 3)
     (* (- (if (= 0 (get-fichas tablero p1 1)) 2 0) (if (= 0 (get-fichas tablero p0 4)) 1 0)) 3)
     (* (- (if (= 0 (get-fichas tablero p1 2)) 2 0) (if (= 0 (get-fichas tablero p0 3)) 1 0)) 3)
     (* (- (if (= 0 (get-fichas tablero p1 3)) 2 0) (if (= 0 (get-fichas tablero p0 2)) 1 0)) 3)
     (* (- (if (= 0 (get-fichas tablero p1 4)) 2 0) (if (= 0 (get-fichas tablero p0 1)) 1 0)) 3)
     (* (- (if (= 0 (get-fichas tablero p1 5)) 2 0) (if (= 0 (get-fichas tablero p0 0)) 1 0)) 3))))


(setf *Severus* 
  (make-jugador
   :nombre 'Severus
   :f-juego #'f-j-nmx
   :f-eval #'mi-f-ev-Severus))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mi-f-ev-Albus (estado)
  ;mis fichas menos las del contrario
  (let ((tablero (estado-tablero estado)) (p0 (estado-lado-sgte-jugador estado))
        (p1 (lado-contrario (estado-lado-sgte-jugador estado))))
    (+ ;las fichas de mi kahala menos las del contrario
       (- (get-fichas tablero p0 6) (get-fichas tablero p1 6)) 
       ;las fichas en la posicion 5 del contrario menos las mias
       ;consideramos que es malo acumular demasiadas fichas en esta posicion
       (*(- (get-fichas tablero p1 5) (get-fichas tablero p0 5))0.6)
       ;las fichas en la posicion 4 del contrario menos las mias
       ;consideramos que es malo acumular demasiadas fichas en esta posicion
       (*(- (get-fichas tablero p1 4) (get-fichas tablero p0 4))0.5)       
       (*(- (get-fichas tablero p1 3) (get-fichas tablero p0 3))0.4)
       (*(- (get-fichas tablero p1 2) (get-fichas tablero p0 2))0.3)
       (*(- (get-fichas tablero p1 1) (get-fichas tablero p0 1))0.2)
       (*(- (get-fichas tablero p1 0) (get-fichas tablero p0 0))0.1))))


(setf *Albus* 
  (make-jugador
   :nombre 'Albus
   :f-juego #'f-j-nmx
   :f-eval #'mi-f-ev-Albus))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mi-f-ev-Esther (estado)
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
     (* (- (if (= 0 (get-fichas tablero p1 0)) 2 0) (if (= 0 (get-fichas tablero p0 5)) 1 0)) 3)
     (* (- (if (= 0 (get-fichas tablero p1 1)) 2 0) (if (= 0 (get-fichas tablero p0 4)) 1 0)) 3)
     (* (- (if (= 0 (get-fichas tablero p1 2)) 2 0) (if (= 0 (get-fichas tablero p0 3)) 1 0)) 3)
     (* (- (if (= 0 (get-fichas tablero p1 3)) 2 0) (if (= 0 (get-fichas tablero p0 2)) 1 0)) 3)
     (* (- (if (= 0 (get-fichas tablero p1 4)) 2 0) (if (= 0 (get-fichas tablero p0 1)) 1 0)) 3)
     (* (- (if (= 0 (get-fichas tablero p1 5)) 2 0) (if (= 0 (get-fichas tablero p0 0)) 1 0)) 3))))


(setf *Esther* 
  (make-jugador
   :nombre 'Esther
   :f-juego #'f-j-nmx
   :f-eval #'mi-f-ev-Esther))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defun mi-f-ev-Ton (estado)
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


(setf *Ton* 
  (make-jugador
   :nombre 'Ton
   :f-juego #'f-j-nmx
   :f-eval #'mi-f-ev-Ton))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defun mi-f-ev-Prueba (estado)
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
    (print (+      
     ;distancia del rival a ganar
     ;(*(- 19 kahala-p1) 1.5)

     ;las fichas de mi kahala menos las del contrario
     (*(- kahala-p0 kahala-p1) 6)
     ;las fichas en la posicion 5 del contrario menos las mias
     ;consideramos que es malo acumular demasiadas fichas en esta posicion
     (*(- fichas-p1-5 fichas-p0-5)5)
     ;las fichas en la posicion 4 del contrario menos las mias
     ;consideramos que es malo acumular demasiadas fichas en esta posicion
     (*(- fichas-p1-4 fichas-p0-4) 4)
     (*(- fichas-p1-3 fichas-p0-3)3.5)
     (*(- fichas-p1-2 fichas-p0-2)2.5)
     (*(- fichas-p1-1 fichas-p0-1)1.5)
     (*(- fichas-p1-0 fichas-p0-0)1)
     
     ;diferencia de ceros
     (* (- (if (= 0 fichas-p1-0) 2 0) (if (= 0 fichas-p0-5) 1 0)) 1.5)
     (* (- (if (= 0 fichas-p1-1) 2 0) (if (= 0 fichas-p0-4) 1 0)) 1.5)
     (* (- (if (= 0 fichas-p1-2) 2 0) (if (= 0 fichas-p0-3) 1 0)) 1.5)
     (* (- (if (= 0 fichas-p1-3) 2 0) (if (= 0 fichas-p0-2) 1 0)) 1.5)
     (* (- (if (= 0 fichas-p1-4) 2 0) (if (= 0 fichas-p0-1) 1 0)) 1.5)
     (* (- (if (= 0 fichas-p1-5) 2 0) (if (= 0 fichas-p0-0) 1 0)) 1.5)))))


(setf *Prueba* 
  (make-jugador
   :nombre 'Prueba
   :f-juego #'f-j-nmx
   :f-eval #'mi-f-ev-Prueba))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun mi-f-ev-Exp (estado)
  (let* ((tablero (estado-tablero estado)) 
         (p0 (estado-lado-sgte-jugador estado))
         (p1 (lado-contrario (estado-lado-sgte-jugador estado)))
         (fichas-p1-0 (get-fichas tablero p1 0)) (fichas-p0-0 (get-fichas tablero p0 0)) (resta-0 (- fichas-p1-0 fichas-p0-0))
         (fichas-p1-1 (get-fichas tablero p1 1)) (fichas-p0-1 (get-fichas tablero p0 1)) (resta-1 (- fichas-p1-1 fichas-p0-1))
         (fichas-p1-2 (get-fichas tablero p1 2)) (fichas-p0-2 (get-fichas tablero p0 2)) (resta-2 (- fichas-p1-2 fichas-p0-2))
         (fichas-p1-3 (get-fichas tablero p1 3)) (fichas-p0-3 (get-fichas tablero p0 3)) (resta-3 (- fichas-p1-3 fichas-p0-3))
         (fichas-p1-4 (get-fichas tablero p1 4)) (fichas-p0-4 (get-fichas tablero p0 4)) (resta-4 (- fichas-p1-4 fichas-p0-4))
         (fichas-p1-5 (get-fichas tablero p1 5)) (fichas-p0-5 (get-fichas tablero p0 5)) (resta-5 (- fichas-p1-5 fichas-p0-5))
         (kahala-p1 (get-fichas tablero p1 6)) (kahala-p0 (get-fichas tablero p0 6)) (dif-kahala (- kahala-p0 kahala-p1)))
    (+      
     ;distancia del rival a ganar
     (-(/(expt (- 19 kahala-p1) 2) 2))
     
     (if (< (- kahala-p0 19) 0)
         0
       (/(expt (- kahala-p0 19) 2) 2))
     ;las fichas de mi kahala menos las del contrario
     
     ;(*(- kahala-p0 kahala-p1) 12);<-------------------------------------------------------------------------
     (print (if (< dif-kahala 0)
         (/ (expt dif-kahala 2) -1)
       (/ (expt dif-kahala 2) 1)))
     
     
     ;las fichas en la posicion 5 del contrario menos las mias
     ;consideramos que es malo acumular demasiadas fichas en esta posicion
     (print (*(- fichas-p1-5 fichas-p0-5)10))
     ;las fichas en la posicion 4 del contrario menos las mias
     ;consideramos que es malo acumular demasiadas fichas en esta posicion
     (*(- fichas-p1-4 fichas-p0-4)8)
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

(setf *Exp* 
  (make-jugador
   :nombre 'Exp
   :f-juego #'f-j-nmx
   :f-eval #'mi-f-ev-Exp))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mi-f-ev-AlbusACT (estado)
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
     (* (- (if (= 0 (get-fichas tablero p1 0)) 2 0) (if (= 0 (get-fichas tablero p0 5)) 1 0)) 3)
     (* (- (if (= 0 (get-fichas tablero p1 1)) 2 0) (if (= 0 (get-fichas tablero p0 4)) 1 0)) 3)
     (* (- (if (= 0 (get-fichas tablero p1 2)) 2 0) (if (= 0 (get-fichas tablero p0 3)) 1 0)) 3)
     (* (- (if (= 0 (get-fichas tablero p1 3)) 2 0) (if (= 0 (get-fichas tablero p0 2)) 1 0)) 3)
     (* (- (if (= 0 (get-fichas tablero p1 4)) 2 0) (if (= 0 (get-fichas tablero p0 1)) 1 0)) 3)
     (* (- (if (= 0 (get-fichas tablero p1 5)) 2 0) (if (= 0 (get-fichas tablero p0 0)) 1 0)) 3))))


(setf *AlACT* 
  (make-jugador
   :nombre 'AlbusACT
   :f-juego #'f-j-nmx
   :f-eval #'mi-f-ev-AlbusACT))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mi-f-ev-SiriusACT (estado)
    ;mis fichas menos las del contrario
  (let ((tablero (estado-tablero estado)) (p0 (estado-lado-sgte-jugador estado))
        (p1 (lado-contrario (estado-lado-sgte-jugador estado))))
    (+      
     ;distancia del rival a ganar
     (*(- 19 (get-fichas tablero p1 6)) 3)
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

(setf *SiACT* 
  (make-jugador
   :nombre 'SiriusACT
   :f-juego #'f-j-nmx
   :f-eval #'mi-f-ev-SiriusACT))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mi-f-ev-SeverusACT (estado)
    ;mis fichas menos las del contrario
  (let ((tablero (estado-tablero estado)) (p0 (estado-lado-sgte-jugador estado))
        (p1 (lado-contrario (estado-lado-sgte-jugador estado))))
    (+      
     ;distancia del rival a ganar
     (*(- 19 (get-fichas tablero p1 6)) 5)
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

(setf *SevACT* 
  (make-jugador
   :nombre 'SevACT
   :f-juego #'f-j-nmx
   :f-eval #'mi-f-ev-SeverusACT))


;;; ------------------------------------------------------------------------------------------
;;; EJEMPLOS DE PARTIDAS DE PRUEBA
;;; ------------------------------------------------------------------------------------------
;;; Juego manual contra jugador automatico, saca el humano
;(partida 0 2 (list *jdr-humano*      *jdr-nmx-Bueno* ))

;;; Juego manual contra jugador automatico, saca el automatico
;(partida 1 2 (list *jdr-humano*      *jdr-nmx-Bueno* ))

;;; Juego automatico sin presentacion del tablero pero con listado de contador
;(setq *verjugada* nil)   ; valor por defecto
;(setq *vermarcador* t)   ; valor por defecto
;(partida 0 1 (list *jdr-aleatorio*   *jdr-nmx-Bueno*))

;;; Juego automatico con salida mimima por eficiencia, profundidad=2
;;;(setq *verb* nil *debug-level* 0 *verjugada* nil *vermarcador* nil)
;;;(partida 1 2 (list *jdr-nmx-Regular* *jdr-nmx-Regular*))


;;; Ajustes para facilitar el seguimiento paso a paso (pag. 11). Reduzcase el nivel de
;;; detalle cuando se vaya adquiriendo pr?ctica.
;;; *debug-nmx* activa *verb* tambien para jugadores automaticos (normalmente desactivado).
(setq *debug-level* 2)         ; Ajusta a 2 el nivel de detalle
(setq *verb* nil)                ; Activa comentarios para seguir la evolucion de la partida
(setq *verjugada* nil)         ; Activa la visualizacion de jugadas
(setq *vermarcador* nil)         ; Activa la visualizacion del marcador
(setq *debug-nmx* nil)         ; Desactiva debuging de negamax

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;*OLDSeverus*
;*Severus*
;*Albus*
;*Ton*
;*Sirius*
;*Prueba*
;*Exp*

(partida 0 2 (list *Prueba* *AlACT*))
(partida 0 2 (list *AlACT* *Prueba*))

(partida 0 2 (list *SevACT* *Prueba*))
(partida 0 2 (list *Prueba* *SevACT*))


(partida 0 2 (list *Prueba* *SiACT*))
(partida 0 2 (list *SiACT* *Prueba*))

(partida 0 2 (list *OLDSeverus* *Prueba*))
(partida 0 2 (list *Prueba* *OLDSeverus*))

(partida 0 2 (list *Prueba* *jdr-nmx-bueno*))
(partida 0 2 (list *jdr-nmx-bueno* *Prueba*))

(partida 0 2 (list *Prueba* *jdr-humano*))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;(partida 0 2 (list *jdr-nmx-bueno* *mi-jugador-a-b*))


;;; Partida entre dos humanos a partir de una posicion determinada para analisis detallado
;;; Se fuerza la posicion de inicio para jugar a partir de ella (ejemplo Pag.5 del enunciado)
;;;(setq mi-posicion (list '(1 0 1 3 2 4 0) (reverse '(12 0 3 5 2 1 2))))
;;;(setq mi-posicion (list '(1 0 1 3 2 4 7) (reverse '(5 0 3 5 2 1 2))))
;;;(setq mi-posicion (list '(0 3 1 1 1 8 8) (reverse '(4 0 8 2 0 0 0))))q
;;;(setq estado (crea-estado-inicial 0 mi-posicion))
;;;(partida 0 2 (list *jdr-humano*      *jdr-human2*) mi-posicion)

;;; Fuerza posicion: fin juego inevitable a la siguiente jugada
;(setq mi-posicion (list '(2 1 3 0 2 7 29) (reverse '(2 5 0 0 0 0 0))))


;;; Ejemplo de experimentacion a varias profundidades
;;;(setq *debug-level* 2)
;;;(setq *verjugada*   nil)
;;;(setq *vermarcador* nil)
;;;(dolist (n '(1 2 3 4 5)) (print (partida 2 n (list *jdr-nmx-regular* *jdr-nmx-bueno*))))

;;; Timeout jugada: a nivel 8 el aleatorio pierde por tiempo
;;;(partida 0 1 (list *jdr-humano*      *jdr-nmx-eval-aleatoria*))
;;;(partida 0 8 (list *jdr-humano*      *jdr-nmx-eval-aleatoria*))

;;; Ejemplos de partidas para pruebas
;;;(partida 0 2 (list *jdr-nmx-Regular* *jdr-erroneo*))
;;;(partida 0 2 (list *jdr-nmx-Regular* *jdr-nmx-bueno*))
;;;(partida 0 2 (list *jdr-humano*      *jdr-nmx-Regular*))
;;;(partida 0 2 (list *jdr-humano*      *jdr-nmx-Bueno*))
;;;(partida 0 2 (list *jdr-humano*      *jdr-1st-opt*))
;;;(partida 0 2 (list *jdr-humano*      *jdr-last-opt*))
;;;(partida 0 2 (list *jdr-humano*      *jdr-human2*))


;;; Partida contra chulo remoto (boaster)
;(if (string= (machine-instance) "HIPOMENES")
;    (partida 2 2 (list *jdr-humano* *challenger-remoto*))
;    (partida 2 2 (list *boaster-remoto* *jdr-humano*)))

;(partida 2 2 (list *jdr-nmx-Bueno* *challenger-remoto*))
;(partida 2 2 (list *boaster-remoto* *jdr-humano*))



;comprobacion de PODA-A-B


;(setq mi-posicion (list '(1 0 1 3 3 4 0) (reverse '(4 0 3 5 1 1 0))))
;(setq estado (crea-estado-inicial 0 mi-posicion))
;(time (negamax estado 4 'f-eval-Bueno))
;(time (negamax-a-b estado 4 'f-eval-Bueno))

