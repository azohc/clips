(deftemplate persona
	(slot nombre)
	(slot sexo)
	(slot edad)
   	(slot tipoPareja)
	(slot numAmigosFacebook (type NUMBER) (default 0))
	(multislot gustos (type SYMBOL)(allowed-values musica lectura cine teatro))
   	(slot caracter (type SYMBOL))
	
)

(deffacts Personas
	(persona (nombre Pepe) (sexo H) (edad 26) (tipoPareja M)(numAmigosFacebook 81)(gustos teatro))
	(persona (nombre Pepa) (sexo M) (edad 25) (tipoPareja H)(numAmigosFacebook 51)(gustos teatro))
	(persona (nombre Luis) (sexo H) (edad 35) (tipoPareja M)(numAmigosFacebook 0)(gustos cine))
	(persona (nombre Luisa) (sexo M) (edad 31) (tipoPareja H)(numAmigosFacebook 10)(gustos cine))
)

;;--------------------- Reglas ---------------------


(defrule extrovertido
?p <- (persona (numAmigosFacebook ?num))
(test (>= ?num 50))
=>
(modify ?p (caracter extrovertido))
)

(defrule introvertido
?p <- (persona (numAmigosFacebook ?num))
(test (> ?num 0))
(test (<= ?num 50))
=>
(modify ?p (caracter introvertido))
)

(defrule noClasificable
?p <- (persona (numAmigosFacebook ?num))
(test (eq ?num 0))
=>
(modify ?p (caracter noDefinido))
)

(deffunction calculaAfinidad (?gustos1 ?gustos2)
	(if (eq ?gustos1 ?gustos2) then (bind ?resultado 1)
    else (bind ?resultado 0))
    (return ?resultado)
)


(defrule afines
(persona(nombre ?nom1)(gustos ?gustos1))
(persona(nombre ?nom2)(gustos ?gustos2))
(test (neq ?nom1 ?nom2))
(test (> (calculaAfinidad ?gustos1 ?gustos2) 0))
(not (afines ?nom2 ?nom1))

=>
(assert (afines ?nom1 ?nom2))          
)


(defrule compatibles
(persona(nombre ?nom1)(sexo ?sexo1)(tipoPareja ?tipoPareja1)(caracter ?cara))
(persona(nombre ?nom2)(sexo ?sexo2)(tipoPareja ?tipoPareja2)(caracter ?cara))
(afines ?nom1 ?nom2)
(test (eq ?sexo1 ?tipoPareja2))
(test (eq ?sexo2 ?tipoPareja1))    
(test (neq ?nom1 ?nom2))
=>
(assert(compatibles ?nom1 ?nom2))
)

(defrule cita1
(compatibles ?nom1 ?nom2)
(persona(nombre ?nom1)(edad ?edad1))
(persona(nombre ?nom2)(edad ?edad2))
(test (neq ?nom1 ?nom2))
(test (< (abs (- ?edad1 ?edad2)) 10))
(not (cita ?nom2 ?nom1))
=>
(assert(cita ?nom1 ?nom2))
)

