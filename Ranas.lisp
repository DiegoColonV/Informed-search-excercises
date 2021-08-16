;;;======================================================================================
;;;  Ranas.lisp
;;;      Resuelve el problema de ranas y el estanque con búsqueda ciega, a lo profundo y a lo ancho.
;;;   
;;;      Representación de los estados: 
;;;         Lista con la posición de cada rana
;;;         Ranas verdes (V), Ranas café(C), Piedra vacía (X)
;;;               Estado inciial:        Estado meta:
;;;               (V V V X C C C)       (C C C X V V V)
;;;
;;;======================================================================================
(defparameter  *open* '())    ;; Frontera de busqueda...                                              
(defparameter  *memory* '())  ;; Memoria de intentos previos

(defparameter  *ops*  '( (:Verde-doble         (V 2))
			  (:Cafe-doble          (C -2))
                         (:Verde-normal        (V 1))
                         (:Cafe-normal         (C -1))
                         (:Verde-normal-r      (V -1))
                         (:Cafe-normal-r       (C 1))
                         (:Verde-doble-r       (V -2))
                         (:Cafe-doble-r        (C 2))))

(defparameter  *id*  -1)  ;; Identificador del ultimo nodo creado
(defparameter  *current-ancestor*  nil)  ;;Id del ancestro común a todos los descendientes que se generen
(defparameter  *solucion*  nil)  ;;lista donde se almacenará la solución recuperada de la memoria

;;; Indicadores de desempeño
(defparameter *nc* 0)  ;; nodos creados
(defparameter *ne* 0)  ;; nodos expandidos
(defparameter *lmfb* 0) ;; longitud máxima de la frontera de búsqueda
(defparameter *t1* 0)  ;; tiempo en el que se inició el proceso
(defparameter *t2* 0)  ;; tiempo en el que terminó el proceso

;;;=======================================================================================
;;  CREATE-NODE (estado  op)  
;;      estado - Un estado del problema a resolver (sistema)...
;;          op - El operador cuya aplicación generó el [estado]...
;;;=======================================================================================
(defun  create-node (estado  op)
  "Construye y regresa un nuevo nodo de búsqueda que contiene al estado y operador recibidos como parámetro"
      (incf  *id*)  ;;incrementamos primero para que lo último en procesarse sea la respuesta
      (incf  *nc*)
      (list  *id*  estado  *current-ancestor*  (first op)) )  ;;los nodos generados son descendientes de *current-ancestor*

;;;=======================================================================================
;;  INSERT-TO-OPEN   y   GET-FROM-OPEN  
;;        
;;        Insert-to-open  recibe una lista y una llave que identifica el metodo a usar para insertar:
;;             :depth-first     Inserta los elementos de la lista en orden inverso y por el inicio de la lista
;;             :breath-first    Inserta los elementos de la lista en orden normal y por el final de la lista
;;        Get-from-open  siempre retira el primer elemento de la lista *open*
;;;=======================================================================================
(defun insert-to-open (estado  op  metodo) 
"Permite insertar nodos de la frontera de busqueda *open* de forma apta para buscar a lo profundo y a lo ancho"
     (let ((nodo  (create-node  estado  op)))
         (cond ((eql  metodo  :depth-first)
	                  (push  nodo  *open*))
	           ((eql  metodo  :breath-first)
		          (setq  *open*  (append  *open*  (list nodo))))
	   	   (T  Nil))
	  (if (> (length *open*) *lmfb* )
	      (setq *lmfb* (length *open*)))))


(defun get-from-open ()
"Recupera el siguiente elemento a revisar de  frontera de busqueda *open*"
      (pop  *open*))

;;;=======================================================================================
;;  POS-RANAS-V	y	POS-RANAS-C 
;;        Regresa una lista con la posición de las ranas verdes o cafés en el estado. 
;;;=======================================================================================

(defun pos-ranas-v (estado)
	   (let ((res '()))
	     (do ((i 0 (+ i 1)))
		 ((= i 7) res)
	       (if (equal (nth i estado) 'V)
	         (setq res (append res (list i)))))))
		  
(defun pos-ranas-c (estado)
	   (let ((res '()))
	     (do ((i 0 (+ i 1)))
		 ((= i 7) res)
	       (if (equal (nth i estado) 'C)
	         (setq res (append res (list i)))))))
	         

;;;=======================================================================================
;;  VALID-OPERATOR? [op, estado]
;;        Predicado.  Indica si es posible aplicar el operador [op] a [estado] segun la posición de las ranas
;;;=======================================================================================

(defun valid-operator? (op estado)
	   (let ((posv (pos-ranas-v estado))
		 (posc (pos-ranas-c estado))
		 (saltos (second (second op))))
	     (if (equal (first (second op)) 'V) 
		 (or (and (and (>= (+ (first posv) saltos) 0) (< (+ (first posv) saltos) 7)) (equal (nth (+ (first posv) saltos) estado) 'X))
		     (and (and (>= (+ (second posv) saltos) 0) (< (+ (second posv) saltos) 7)) (equal (nth (+ (second posv) saltos) estado) 'X))
		     (and (and (>= (+ (third posv) saltos) 0) (< (+ (third posv) saltos) 7)) (equal (nth (+ (third posv) saltos) estado) 'X)))
	         (or (and (and (>= (+ (third posc) saltos) 0) (< (+ (third posc) saltos) 7)) (equal (nth (+ (third posc) saltos) estado) 'X))
		     (and (and (>= (+ (second posc) saltos) 0) (< (+ (second posc) saltos) 7)) (equal (nth (+ (second posc) saltos) estado) 'X))
		     (and (and (>= (+ (first posc) saltos) 0) (< (+ (first posc) saltos) 7)) (equal (nth (+ (first posc) saltos) estado) 'X))))))
		     
;;;=======================================================================================
;;  MOVER-V	Y	MOVER-C
;;        Regresa una lista con la posición de la rana al antes y después de saltar. ((-1.-1) si no es válido)
;;		Rana Saltó de 3 a 4 
;;		      (3 4)
;;;=======================================================================================

(defun mover-v (saltos posv estado)
	   (let ((res '()))
	     (if (and (and (>= (+ (first posv) saltos) 0) (< (+ (first posv) saltos) 7)) (equal (nth (+ (first posv) saltos) estado) 'X))
		 (setq res (list (first posv) (+ (first posv) saltos)))
		 (if (and (and (>= (+ (second posv) saltos) 0) (< (+ (second posv) saltos) 7)) (equal (nth (+ (second posv) saltos) estado) 'X))
		     (setq res (list (second posv) (+ (second posv) saltos)))
		     (if (and (and (>= (+ (third posv) saltos) 0) (< (+ (third posv) saltos) 7)) (equal (nth (+ (third posv) saltos) estado) 'X))
			 (setq res (list (third posv) (+ (third posv) saltos)))
			 (setq res (list -1 -1)))))))
			 
(defun mover-c (saltos posc estado)
	   (let ((res '()))
	     (if (and (and (>= (+ (third posc) saltos) 0) (< (+ (third posc) saltos) 7)) (equal (nth (+ (third posc) saltos) estado) 'X))
		 (setq res (list (third posc) (+ (third posc) saltos)))
		 (if (and (and (>= (+ (second posc) saltos) 0) (< (+ (second posc) saltos) 7)) (equal (nth (+ (second posc) saltos) estado) 'X))
		     (setq res (list (second posc) (+ (second posc) saltos)))
		     (if (and (and (>= (+ (first posc) saltos) 0) (< (+ (first posc) saltos) 7)) (equal (nth (+ (first posc) saltos) estado) 'X))
			 (setq res (list (first posc) (+ (first posc) saltos)))
			 (setq res (list -1 -1)))))))


;;;=======================================================================================
;;  MOVER-RANA
;;        Regresa el nuevo estado después de aplicar el movimiento
;;;=======================================================================================

(defun mover-rana (lis color estado)
	   (let ((res '()))
	     (do ((i 0 (+ i 1)))
		 ((= i 7) res)
	       (if (= (first lis) i)
		   (setq res (append res (list 'X)))
		   (if (= (second lis) i)
		       (setq res (append res (list color)))
		       (setq res (append res (list (nth i estado)))))))))
    
;;;=======================================================================================
;;  APPLY-OPERATOR (op, estado)
;;        Resuelve la tarea básica de cambiar de estado el sistema...
;;;=======================================================================================

(defun  apply-operator (op  estado) 
	(let   ((posv (pos-ranas-v estado))
		(posc (pos-ranas-c estado))
	    	(saltos (second (second op))) 
	        (operador (first op))
	        (color (first (second op))))
	 (case operador 
	    (:Verde-normal (if (>= (first (mover-v saltos posv estado)) 0)
	    			(mover-rana (mover-v saltos posv estado) color estado)))
	    (:Verde-doble (if (>= (first (mover-v saltos posv estado)) 0)
	    			(mover-rana (mover-v saltos posv estado) color estado)))
	    (:Verde-normal-r (if (>= (first (mover-v saltos posv estado)) 0)
	    			(mover-rana (mover-v saltos posv estado) color estado)))
	    (:Verde-doble-r (if (>= (first (mover-v saltos posv estado)) 0)
	    			(mover-rana (mover-v saltos posv estado) color estado)))
	    (:Cafe-normal (if (>= (first (mover-c saltos posc estado)) 0)
	    			(mover-rana (mover-c saltos posc estado) color estado)))
	    (:Cafe-doble (if (>= (first (mover-c saltos posc estado)) 0)
	    			(mover-rana (mover-c saltos posc estado) color estado)))
	    (:Cafe-normal-r (if (>= (first (mover-c saltos posc estado)) 0)
	    			(mover-rana (mover-c saltos posc estado) color estado)))
	    (:Cafe-doble-r (if (>= (first (mover-c saltos posc estado)) 0)
	    			(mover-rana (mover-c saltos posc estado) color estado)))
	    (T "error"))))


;;;=======================================================================================
;;  EXPAND (estado)
;;        Construye y regresa una lista con todos los descendientes validos de [estado]
;;;=======================================================================================
(defun expand (estado)
"Obtiene todos los descendientes válidos de un estado, aplicando todos los operadores en *ops* en ese mismo órden"
     (let ((descendientes  nil)
	     (nuevo-estado  nil))
           (dolist  (op  *Ops*  descendientes) 
		 (if (valid-operator?  op  estado)
		     (progn	 
		       (setq  nuevo-estado  (apply-operator  op estado))  
		       (incf *ne*)
		       (setq  descendientes  (cons  (list nuevo-estado op) descendientes)))))))


;;;=======================================================================================
;;  REMEMBER-STATE?  y  FILTER-MEMORIES
;;        Permiten administrar la memoria de intentos previos
;;;=======================================================================================
(defun  remember-state?  (estado  lista-memoria)
"Busca un estado en una lista de nodos que sirve como memoria de intentos previos"  
     (cond ((null  lista-memoria)  Nil)
	        ((equal  estado  (second (first  lista-memoria)))  T)  ;;el estado es igual al que se encuentra en el nodo?
		(T  (remember-state?  estado  (rest  lista-memoria))))  )


(defun  filter-memories (lista-estados-y-ops) 
"Filtra una lista de estados-y-operadores quitando aquellos elementos cuyo estado está en la memoria *memory*
     la lista de estados y operadores tiene estructura: [(<estado> <op>) (<estado> <op>) ... ]"
     (cond ((null  lista-estados-y-ops)  Nil)
	       ((remember-state? (first (first  lista-estados-y-ops)) *memory*)  ;; si se recuerda el primer elemento de la lista, filtrarlo...
		       (filter-memories  (rest  lista-estados-y-ops)))
		(T  (cons  (first lista-estados-y-ops) (filter-memories  (rest  lista-estados-y-ops))))) )  ;; de lo contrario, incluirlo en la respuesta

;;;=======================================================================================
;;  EXTRACT-SOLUTION  y  DISPLAY-SOLUTION
;;       Recuperan y despliegan la secuencia de solucion del problema...
;;       extract-solution   recibe un nodo (el que contiene al estado meta) que ya se encuentra en la memoria y
;;                                    rastrea todos sus ancestros hasta llegar  al  nodo que contiene al estado inicial...
;;       display-solution  despliega en pantalla la lista global *solucion* donde ya se encuentra, en orden correcto,
;;                                    el proceso de solución del problema...
;;
;;  DISPLAY-INDICATORS
;;	Muestra todos los indicadores de desempeño 
;;;=======================================================================================
(defun extract-solution (nodo)
"Rastrea en *memory* todos los descendientes de [nodo] hasta llegar al estado inicial"
     (labels ((locate-node  (id  lista)       ;; función local que busca un nodo por Id  y si lo encuentra regresa el nodo completo
		  (cond ((null  lista)  Nil)
		        ((eql  id  (first (first  lista))) (first  lista))
		        (T  (locate-node  id (rest  lista))))))
	  (let ((current  (locate-node  (first  nodo)  *memory*)))
	     (loop  while  (not (null  current))  do                        
		 (push  current  *solucion*)     ;; agregar a la solución el nodo actual
		 (setq  current  (locate-node  (third  current) *memory*))))  ;; y luego cambiar a su antecesor...
	     *solucion*))


(defun  display-solution (lista-nodos)
"Despliega la solución en forma conveniente y numerando los pasos"
    (format  t  "Solución con ~A  pasos:~%~%" (1- (length  lista-nodos)))
    (let  ((nodo  nil))
         (dotimes  (i (length  lista-nodos))
	      (setq  nodo  (nth  i  lista-nodos))
	      (if  (= i 0)
		   (format t "Inicio en: ~A~%" (second  nodo))  ;; a partir de este estado inicial
	       ;;else
		   (format t "\(~2A\)  aplicando ~20A se llega a ~A~%"  i (fourth  nodo)  (second  nodo)))))  )  ;; imprimir el número de paso, operador y estado...

(defun display-indicators ()
	(format t "*** INDICADORES DE DESEMPEÑO ***~%")
	(format t "Nodos creados: ~A~%" *nc*)
	(format t "Nodos expandidos: ~A~%" *ne*)
	(format t "Longitud máxima de la frontera de búsqueda: ~A~%" *lmfb*)
	(format t "Tiempo de ejecucíon: ~6f segundos" (/ (- *t2* *t1*) internal-time-units-per-second)))

;;;=======================================================================================
;;  RESET-ALL  y  BLIND-SEARCH
;;
;;       Recuperan y despliegan la secuencia de solucion del problema...
;;
;;       reset-all   Reinicializa todas las variables globales para una nueva ejecución
;;       blind-search  Función principal, realiza búsqueda desde un estado inicial a un estado meta
;;;=======================================================================================
(defun reset-all () 
"Reinicia todas las variables globales para realizar una nueva búsqueda..."
     (setq  *open*  nil)
     (setq  *memory*  nil)
     (setq  *id*  0)
     (setq  *current-ancestor*  nil)
     (setq  *solucion*  nil)
     (setq  *nc*  0)
     (setq  *ne*  0)
     (setq  *lmfb*  0)
     (setq  *t1*  0)
     (setq  *t2*  0))


(defun  blind-search (edo-inicial  edo-meta  metodo)
"Realiza una búsqueda ciega, por el método especificado y desde un estado inicial hasta un estado meta
    los métodos posibles son:  :depth-first - búsqueda en profundidad
                               :breath-first - búsqueda en anchura"
  (reset-all)
  (setq *t1* (get-internal-run-time))
  (let ((nodo nil)
	  (estado nil)
	  (sucesores  '())
	  (operador  nil)
	  (meta-encontrada  nil))

      (insert-to-open   edo-inicial  nil  metodo)
      (loop until  (or  meta-encontrada
                        (null *open*))  do
	   (setq nodo    (get-from-open)              ;;Extraer el siguiente nodo de la frontera de búsquea
		     estado  (second  nodo)               ;;Identificar el estado y operador que contiene
		     operador  (third  nodo))             
	   (push  nodo  *memory*)                     ;;Recordarlo antes de que algo pueda pasar...
	   (cond    ((equal  edo-meta  estado) 
	                        (setq *t2* (get-internal-run-time))
		                (format  t  "Éxito. Meta encontrada en ~A  intentos~%" (first  nodo))
		                (display-solution  (extract-solution  nodo))
		                (display-indicators)
		                (setq  meta-encontrada  T))
		         (t (setq  *current-ancestor*  (first  nodo)) 
			     (setq  sucesores  (expand estado))
			     (setq  sucesores  (filter-memories  sucesores))     ;;Filtrar los estados ya revisados...
			      (loop for  element  in  sucesores  do
				    (insert-to-open  (first element)  (second element)  metodo))))))  )
				    
;;;=======================================================================================
;;;=======================================================================================
;;			 EJEMPLO DE EJECUCIÓN Y RESULTADO   
;; 
;;(blind-search '(V V V X C C C) '(C C C X V V V) :breath-first)
;;Éxito. Meta encontrada en 360  intentos
;;Solución con 15  pasos:
;;
;;Inicio en: (V V V X C C C)
;;(1 )  aplicando CAFE-NORMAL          se llega a (V V V C X C C)
;;(2 )  aplicando VERDE-DOBLE          se llega a (V V X C V C C)
;;(3 )  aplicando VERDE-NORMAL         se llega a (V X V C V C C)
;;(4 )  aplicando CAFE-DOBLE           se llega a (V C V X V C C)
;;(5 )  aplicando CAFE-DOBLE           se llega a (V C V C V X C)
;;(6 )  aplicando CAFE-NORMAL          se llega a (V C V C V C X)
;;(7 )  aplicando VERDE-DOBLE          se llega a (V C V C X C V)
;;(8 )  aplicando VERDE-DOBLE          se llega a (V C X C V C V)
;;(9 )  aplicando VERDE-DOBLE          se llega a (X C V C V C V)
;;(10)  aplicando CAFE-NORMAL          se llega a (C X V C V C V)
;;(11)  aplicando CAFE-DOBLE           se llega a (C C V X V C V)
;;(12)  aplicando CAFE-DOBLE           se llega a (C C V C V X V)
;;(13)  aplicando VERDE-NORMAL         se llega a (C C V C X V V)
;;(14)  aplicando VERDE-DOBLE          se llega a (C C X C V V V)
;;(15)  aplicando CAFE-NORMAL          se llega a (C C C X V V V)
;;*** INDICADORES DE DESEMPEÑO ***
;;Nodos creados: 369
;;Nodos expandidos: 1142
;;Longitud máxima de la frontera de búsqueda: 50
;;Tiempo de ejecucíon:  0.009 segundos     
;;;=======================================================================================
;;;=======================================================================================
