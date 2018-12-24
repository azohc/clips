; Se quiere construir un sistema de reglas en CLIPS que permita 
;  empaquetar artículos en cajas de forma automática. 

; Se partirá de un conjunto de hechos iniciales que describen 
;  los productos que tenemos que empaquetar 
;  y las cajas disponibles en el almacén. 

; Dispongo de un número limitado de cajas iguales, 
;  es decir, con la misma capacidad máxima. 

; - Cada artículo se envuelve de manera individual.
; - En las cajas sólo podemos empaquetar artículos envueltos.
; - Todos los artículos de la misma caja son del mismo tipo, es decir, 
;   un artículo frágil no se empaquetará en una caja que ya se ha abierto para meter artículos pesados.
; - Si tenemos una caja ya empezada (abierta?) con algún artículo, seguiremos 
;   llenándola con artículos del mismo tipo (frágil o pesado) antes de abrir una caja nueva.
; - Cada artículo y caja tienen unos volúmenes asociados, de forma que un 
;   artículo podrá empaquetarse en una caja si el volumen disponible en ésta es 
;   mayor que el volumen del artículo.

; juan chozas & miguel menéndez


(deftemplate producto
    (multislot nombre (type STRING))
    (slot tipo (type SYMBOL)(allowed-values fragil pesado normal))
    (slot envuelto (allowed-values S N))
    (slot volumen (type INTEGER)(range 0 200))
)

(deftemplate caja
    (slot volumen (type INTEGER))
    (slot abierta (allowed-values S N))
    (slot tipo (type SYMBOL)(allowed-values fragil pesado normal))
)

(deffacts cajas
    (caja (volumen 25) (abierta N))
    (caja (volumen 25) (abierta N))
    (caja (volumen 25) (abierta N))
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

