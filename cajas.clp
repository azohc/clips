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
    (caja (id 4) (volumen 25) (abierta no))
)

(deffacts productos
    (producto (nombre "almax")          (tipo normal) (envuelto no) (volumen 11))
    (producto (nombre "paracetamol")    (tipo normal) (envuelto si) (volumen 21))
    (producto (nombre "almax forte")    (tipo pesado) (envuelto no) (volumen 9))
    (producto (nombre "tranquis")       (tipo pesado) (envuelto si) (volumen 6))
    (producto (nombre "mierda fragil")  (tipo fragil) (envuelto no) (volumen 7))
    (producto (nombre "likens")         (tipo fragil) (envuelto no) (volumen 19))
    (producto (nombre "likens light")   (tipo fragil) (envuelto no) (volumen 6))
)


(deftemplate control                                      
    (slot cajaAbierta (type INTEGER) (default -1))          ;cuando una caja este abierta, este slot mantiene el id de la caja
    (slot cerrarCaja (allowed-values si no) (default no))   ;cuando se tenga que cerrar una caja, este slot tomará el valor si
)
(deffacts ctrl
    (control)
)

;--------------------- Reglas ---------------------


(defrule envolverProducto
    ?hp <- (producto (nombre ?n) (envuelto no))
    =>
    (modify ?hp (envuelto si))
    (printout t crlf "Producto " ?n " envuelto" crlf)
)

(defrule empaquetarProducto
    ?ctrl <- (control (cajaAbierta ?cid))
    ?hc <- (caja (id ?id) (abierta si) (volumen ?vc) (tipo ?tc))
    ?hp <- (producto (nombre ?n) (envuelto si) (empaq no) (volumen ?vp) (tipo ?tp))

    (test (eq ?id ?cid))
    (test (>= ?vc ?vp))
    (test (eq ?tc ?tp))
    =>
    (modify ?hp (empaq ?id))
    (modify ?hc (volumen (- ?vc ?vp)))
    (printout t crlf "Producto " ?n " empaquetado" crlf)
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
                                                                    (<= ?f:volumen ?vc)     ; cabe en la caja?
                                                                    (and 
                                                                        (eq ?f:tipo ?tc)    ; es del mismo tipo que el de la caja?
                                                                        (eq ?f:empaq no)    ; esta por empaquetar?
                                                                    ))) 
                                                                )
                    0
                )
            )
    )
    =>
    (modify ?ctrl (cerrarCaja si))
    (printout t crlf "No se pueden empaquetar mas productos de tipo " ?tc crlf)
)


(defrule cierraCaja
    ?ctrl <- (control (cajaAbierta ?ca) (cerrarCaja si))
    ;?hp <- (producto (tipo ?tp) (volumen ?vp) (empaq no))
    ?hc <- (caja (id ?id) (volumen ?vc) (abierta si) (tipo ?tc))

    (test (eq ?ca ?id))
    =>
    (modify ?ctrl (cajaAbierta -1) (cerrarCaja no))
    (modify ?hc (abierta no))
    (printout t crlf "Caja " ?id " cerrada" crlf)
)


(defrule abreCaja
    ?ctrl <- (control (cajaAbierta -1))
    ?hc <- (caja (id ?id) (tipo nulo) (abierta no))
    ?hp <- (producto (envuelto si) (empaq no) (tipo ?tp))

    =>

    (modify ?ctrl (cajaAbierta ?id))
    (modify ?hc (tipo ?tp) (abierta si))
    (printout t crlf "Caja " ?id " abierta. Tipo " ?tp crlf)
)

