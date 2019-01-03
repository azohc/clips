; juan chozas & miguel menéndez

; sistema de reglas en CLIPS para empaquetar artículos en cajas de forma automática. 

(deftemplate producto
    (slot nombre (type STRING))
    (slot tipo (type SYMBOL)(allowed-values fragil pesado normal))
    (slot envuelto (allowed-values si no))
    (slot volumen (type INTEGER)(range 0 200))
    (slot empaq (type SYMBOL) (default no))
)

(deftemplate caja
	(slot id)
    (slot volumen (type INTEGER))
    (slot abierta (allowed-values si no))
    (slot tipo (type SYMBOL)(allowed-values nulo fragil pesado normal) (default nulo))
)

; Dispongo de un número limitado de cajas iguales, 
;  es decir, con la misma capacidad máxima. 
(deffacts cajas
    (caja (id 1) (volumen 25) (abierta no))
    (caja (id 2) (volumen 25) (abierta no))
    (caja (id 3) (volumen 25) (abierta no))
)

(deffacts productos
    (producto (nombre "almax")          (tipo normal) (envuelto no) (volumen 11))
    (producto (nombre "paracetamol")    (tipo normal) (envuelto si) (volumen 21))
    (producto (nombre "almax forte")    (tipo pesado) (envuelto no) (volumen 9))
    (producto (nombre "tranquis")       (tipo pesado) (envuelto si) (volumen 6))
    (producto (nombre "mierda fragil")  (tipo fragil) (envuelto no) (volumen 7))
    (producto (nombre "likens")         (tipo fragil) (envuelto no) (volumen 19))
    (producto (nombre "likens light")   (tipo fragil) (envuelto no) (volumen 6))
    ;(producto (nombre "kaka")           (tipo pesado) (envuelto no) (volumen 12))
; Comentado -> prueba el comentario de abajo del TODO -> no quedan más productos de un tipo
; Sin comentario -> Empaqueta todo !!
)

;--------------------- Reglas ---------------------

; - Cada artículo se envuelve de manera individual.
; - En las cajas sólo podemos empaquetar artículos envueltos.
; - Todos los artículos de la misma caja son del mismo tipo, es decir, 
;   un artículo frágil no se empaquetará en una caja que ya se ha abierto para meter artículos pesados.

; - Si tenemos una caja ya empezada (tiene tipo asignado?) con algún artículo, seguiremos 
;   llenándola con artículos del mismo tipo (frágil o pesado) antes de abrir una caja nueva.

; - Cada artículo y caja tienen unos volúmenes asociados, de forma que un 
;   artículo podrá empaquetarse en una caja si el volumen disponible en ésta es 
;   mayor que el volumen del artículo.

; def_function NO ES DEFRULE!
; not afines -> simetrico
; dictar orden de ejecucion (activaciono) de reglas con variables de control

(deftemplate control
    (slot cajaAbierta (type INTEGER) (default -1))
    (slot cerrarCaja (allowed-values si no) (default no)) 
)
(deffacts ctrl
    (control)
)
; con el controlador he conseguido que solo se pueda abrir una caja
; cuando cajaAbierta (rollo puntero a la caja abierta) es igual a -1, hay que abrir caja

; TODO: encontrar manera de disparar cierraCaja cuando NO QUEDEN MÁS productos
; del TIPO de la caja con id cajaAbierta

(defrule envolverProducto
    ?hp <- (producto (envuelto no))
    =>
    (modify ?hp (envuelto si))
    (printout t crlf "Producto envuelto" ?hp crlf)
)

;(defrule cajaLlena
    ;?hc <- (caja (volumen ?vc) (abierta si) (tipo ?tc))
    ;?hpPesado <- (producto (tipo ?tp) (volumen ?vpesi))
    ;?hpLigero <- (producto (tipo ?tp) (volumen ?vlig))
    ;(test (>= ?vpes ?vlig))
;
;)

(defrule empaquetarProducto
    ?ctrl <- (control (cajaAbierta ?cid))
    ?hc <- (caja (id ?id) (abierta si) (volumen ?vc) (tipo ?tc))
    ?hp <- (producto (envuelto si) (empaq no) (volumen ?vp) (tipo ?tp))

    (test (eq ?id ?cid))
    (test (>= ?vc ?vp))
    (test (eq ?tc ?tp))
    =>
    (modify ?hp (empaq ?id))
    (modify ?hc (volumen (- ?vc ?vp)))
    (printout t crlf "Producto empaquetado: " ?vp ?vc crlf)
)

(defrule noQuedanProductosPorEmpaquetar
    ?ctrl   <- (control (cajaAbierta ?ca) (cerrarCaja no))
    ?hc     <- (caja (id ?id) (tipo ?tc) (volumen ?vc))

    (test (eq ?id ?ca))
    (test      
            (or                                ;OR : Una de dos posibilidades
                (eq                             ;1: la cantidad de productos no empaquetados del tipo de la caja abierta es 0
                    (length$ (find-all-facts ((?f producto)) (and (eq ?f:tipo ?tc) (eq ?f:empaq no)))) 
                    0
                )
                (eq                         ;2: la cantidad de los productos no empaquetados del tipo de la caja que caben en la caja = 0  
                    (length$ (find-all-facts ((?f producto))    (and
                                                                    (<= ?f:volumen ?vc)
                                                                    (and 
                                                                        (eq ?f:tipo ?tc) 
                                                                        (eq ?f:empaq no)
                                                                    ))) 
                                                                )
                    0
                )
            )
    )
    =>
    (modify ?ctrl (cerrarCaja si))
)


(defrule cierraCaja
    ?ctrl <- (control (cajaAbierta ?ca) (cerrarCaja si))
    ;?hp <- (producto (tipo ?tp) (volumen ?vp) (empaq no))
    ?hc <- (caja (id ?idcaja) (volumen ?vc) (abierta si) (tipo ?tc))

    (test (eq ?ca ?idcaja))
    =>
    (modify ?ctrl (cajaAbierta -1) (cerrarCaja no))
    (modify ?hc (abierta no))
)


(defrule abreCaja
    ?ctrl <- (control (cajaAbierta -1))
    ?hc <- (caja (id ?id) (tipo nulo) (abierta no))
    ?hp <- (producto (envuelto si) (empaq no) (tipo ?tp))

    =>

    (modify ?ctrl (cajaAbierta ?id))
    (modify ?hc (tipo ?tp) (abierta si))
    (printout t crlf "Caja ligada " ?tp crlf)
)

