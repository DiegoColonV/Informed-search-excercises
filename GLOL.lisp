;;;======================================================================================
;;;  GLOL.lisp
;;;      Resuelve el problema de granjero, lobo, oveja, legumbre con búsqueda ciega, a lo profundo y a lo ancho.
;;;   
;;;      Representación de los estados: 
;;;         Lista con dos sublistas internas, una por cada orilla. 
;;;         En cada orilla, (lobo oveja legumbre barca).	*El granjero no se toma en cuenta ya que siempre se mueve con la barca
;;;                 Estado inciial:        Estado meta:
;;;               ((1 1 1 1) (0 0 0 0))     ((0 0 0 0) (1 1 1 1))
;;;
;;;======================================================================================
(defparameter  *open* '())    ;; Frontera de busqueda...                                              
(defparameter  *memory* '())  ;; Memoria de intentos previos

(defparameter  *ops*  '((:Barca     (0 0 0 1)) 
			 (:Lobo      (1 0 0 0))
                        (:Oveja     (0 1 0 0))
                        (:Legumbre  (0 0 1 0))))

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
	      (setq *lmfb* (length *open*))))  )


(defun get-from-open ()
"Recupera el siguiente elemento a revisar de  frontera de busqueda *open*"
      (pop  *open*))

;;;=======================================================================================
;;  BARGE-SHORE (estado)
;;        Regresa la orilla del rio en la que se encuentra la barca en  [estado]
;;           0 - Orilla origen (primer sublista del estado)
;;           1 - Orilla destino (segunda sublista del estado)
;;;=======================================================================================
(defun  barge-shore (estado)
"Regresa la orilla del río en la que se encuentra la barca en el estado recibido como parámetro:  
  0 - origen  1 - destino"
     (if  (= 1 (fourth (first  estado)))  0  1))


;;;=======================================================================================
;;  VALID-OPERATOR [op, estado]
;;        Predicado.  Indica si es posible aplicar el operador [op] a [estado] segun los recursos en la orilla de la barca
;;;=======================================================================================
(defun  valid-operator? (op  estado)
"Predicado. Valida la aplicación de un operador a un estado..."
  (let*  ((orilla  (barge-shore  estado))                         
	    (lobo  (first  (nth  orilla  estado)))   
	    (oveja  (second  (nth  orilla  estado))) 
	    (legumbre  (third  (nth  orilla  estado)))) 
	  (and (>=  lobo  (first (second op)))
    	       (>=  oveja  (second (second op)))
    	       (>=  legumbre  (third (second op))))))


;;;=======================================================================================
;;  VALID-STATE (estado)
;;        Predicado.  Indica si [estado]  es valido segun las restricciones del problema
;;                          Es decir, si en c/orilla se encuentra el lobo y la oveja o la oveja y la legumbre sin el granjero (barca) presente
;;;=======================================================================================
(defun  valid-state? (estado)
	(not (or
		(and (= (first (first estado)) 1) (= (second (first estado)) 1) (= (fourth (first estado)) 0))
		(and (= (second (first estado)) 1) (= (third (first estado)) 1) (= (fourth (first estado)) 0))
		(and (= (first (second estado)) 1) (= (second (second estado)) 1) (= (fourth (second estado)) 0))
		(and (= (second (second estado)) 1) (= (third (second estado)) 1) (= (fourth (second estado)) 0)))))

    
;;;=======================================================================================
;;  APPLY-OPERATOR (op, estado)
;;        Resuelve la tarea básica de cambiar de estado el sistema...
;;;=======================================================================================

(defun flip (bit)  (boole  BOOLE-XOR  bit  1))

(defun  apply-operator (op  estado) 
"Obtiene el descendiente de [estado] al aplicarle  [op]  SIN VALIDACIONES"
    (let*  ((orilla1  (first  estado))
	       (orilla2  (second  estado))
	       (b0   (fourth  orilla1))
	       (b1   (fourth   orilla2))
	       (operador (first op)))     ;; este operador es la etiqueta humana del operador...
	 (case operador 
	    (:Lobo     (list  (list  (flip (first orilla1)) (second orilla1) (third orilla1) (flip b0))   
	                      (list  (flip (first orilla2)) (second orilla2) (third orilla2) (flip b1))))
	    (:Oveja    (list  (list  (first orilla1) (flip (second orilla1)) (third orilla1) (flip b0))   
	                      (list  (first orilla2) (flip (second orilla2)) (third orilla2) (flip b1)))) 
	    (:Legumbre (list  (list  (first orilla1) (second orilla1) (flip (third orilla1)) (flip b0))   
	                      (list  (first orilla2) (second orilla2) (flip (third orilla2)) (flip b1))))  
	    (:Barca    (list  (list  (first orilla1) (second orilla1) (third orilla1) (flip b0))   
	                      (list  (first orilla2) (second orilla2) (third orilla2) (flip b1))))
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
		 (when (valid-operator?  op  estado)
	               (setq  nuevo-estado  (apply-operator  op estado))
	               (when (valid-state? nuevo-estado)
	                     (incf *ne*)
	                     (setq  descendientes  (cons  (list nuevo-estado op) descendientes)))))))


;;;=======================================================================================
;;  REMEMBER-STATE?  y  FILTER-MEMORIES
;;        Permiten administrar la memoria de intentos previos
;;;=======================================================================================
(defun  remember-state?  (estado  lista-memoria)
"Busca un estado en una lista de nodos que sirve como memoria de intentos previos
     el estado tiene estructura:  [(<m0><c0><b0>) (<m1><c1><b1>)],
     el nodo tiene estructura : [<Id> <estado> <id-ancestro> <operador> ]"  
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

;;				EJEMPPLO DE EJECUCIÓN Y RESULTADO
;;
;;(blind-search '((1 1 1 1) (0 0 0 0)) '((0 0 0 0) (1 1 1 1)) :breath-first)
;;Éxito. Meta encontrada en 12  intentos
;;Solución con 7  pasos:
;;
;;Inicio en: ((1 1 1 1) (0 0 0 0))
;;(1 )  aplicando OVEJA                se llega a ((1 0 1 0) (0 1 0 1))
;;(2 )  aplicando BARCA                se llega a ((1 0 1 1) (0 1 0 0))
;;(3 )  aplicando LEGUMBRE             se llega a ((1 0 0 0) (0 1 1 1))
;;(4 )  aplicando OVEJA                se llega a ((1 1 0 1) (0 0 1 0))
;;(5 )  aplicando LOBO                 se llega a ((0 1 0 0) (1 0 1 1))
;;(6 )  aplicando BARCA                se llega a ((0 1 0 1) (1 0 1 0))
;;(7 )  aplicando OVEJA                se llega a ((0 0 0 0) (1 1 1 1))
;;*** INDICADORES DE DESEMPEÑO ***
;;Nodos creados: 13
;;Nodos expandidos: 24
;;Longitud máxima de la frontera de búsqueda: 2
;;Tiempo de ejecucíon:    0.0 segundos

;;;=======================================================================================
;;;=======================================================================================
