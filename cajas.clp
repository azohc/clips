; juan chozas & miguel menéndez

; sistema de reglas en CLIPS para empaquetar artículos en cajas de forma automática. 

(deftemplate producto
    (slot nombre (type STRING))
    (slot tipo (type SYMBOL)(allowed-values fragil pesado normal))
    (slot envuelto (allowed-values S N))
    (slot volumen (type INTEGER)(range 0 200))
    (slot empaq (allowed-values S N) (default N))
)

(deftemplate caja
	(slot id)
    (slot volumen (type INTEGER))
    (slot abierta (allowed-values S N))
    (slot tipo (type SYMBOL)(allowed-values nulo fragil pesado normal) (default nulo))
)

; Dispongo de un número limitado de cajas iguales, 
;  es decir, con la misma capacidad máxima. 
(deffacts cajas
    (caja (id 1) (volumen 25) (abierta N))
    (caja (id 2) (volumen 25) (abierta N))
    (caja (id 3) (volumen 25) (abierta N))
)

(deffacts productos
    (producto (nombre "almax")          (tipo normal) (envuelto N) (volumen 11))
    (producto (nombre "paracetamol")    (tipo normal) (envuelto S) (volumen 21))
    (producto (nombre "almax forte")    (tipo pesado) (envuelto N) (volumen 9))
    (producto (nombre "tranquis")       (tipo pesado) (envuelto S) (volumen 6))
    (producto (nombre "mierda fragil")  (tipo fragil) (envuelto N) (volumen 7))
    (producto (nombre "likens")         (tipo fragil) (envuelto N) (volumen 19))
    (producto (nombre "likens light")   (tipo fragil) (envuelto N) (volumen 6))

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
; dictar orden de ejecucion (activacion) de reglas con variables de control



(defrule envolverProducto
    ?hp <- (producto (envuelto N))
    =>
    (modify ?hp (envuelto S))
    (printout t crlf "Producto envuelto" ?hp crlf)
)

;(defrule cajaLlena
    ;?hc <- (caja (volumen ?vc) (abierta S) (tipo ?tc))
    ;?hpPesado <- (producto (tipo ?tp) (volumen ?vpes))
    ;?hpLigero <- (producto (tipo ?tp) (volumen ?vlig))
    ;(test (>= ?vpes ?vlig))
;
;)

(defrule pruebe
    (forall 
        (caja (abierta ?a))
        (caja (tipo ?t))    
    )
    =>
    (printout t crlf "Todo evalua a " ?a " tipo " ?t crlf)
)


;(defrule abreCaja ;estabn
    ;?hp <- (producto (envuelto S) (empaq N) (tipo ?tp))
    ;?hc <- (caja (tipo nulo) (abierta N))
;
    ;;?hcabierta <- (caja (abierta S))
    ;;(test ?hcabierta estaLlena)
    ;=>
    ;;cerrar ?hcabierta
    ;;abrir ?hc
;
    ;(modify ?hc (tipo ?tp) (abierta S))
    ;(printout t crlf "Caja ligada " ?tp crlf)
;)

(defrule empaquetarProducto
    ?hc <- (caja (abierta S) (volumen ?vc) (tipo ?tc))
    ?hp <- (producto (envuelto S) (empaq N) (volumen ?vp) (tipo ?tp))
    (test (>= ?vc ?vp))
    (test (eq ?tc ?tp))
    =>
    (modify ?hp (empaq S))
    (modify ?hc (volumen (- ?vc ?vp)))
    (printout t crlf "Producto empaquetado: " ?vp ?vc crlf)
)


