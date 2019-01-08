
;APP_RECOMENDATOR

(deftemplate app
    (slot nombre (type STRING))
    (slot categoria (type SYMBOL))
    (slot puntuacion (type FLOAT)(range 0.0 5.0)) 														; la antepenultima(abajo del todo) en la lista de apps tiene NaN en vez de una float
    (slot reviews (type INTEGER))
    (slot size (type SYMBOL))
    (slot installs (type SYMBOL))
    (slot coste (type SYMBOL) (allowed-values Free Paid)) 												; si miras la lista de apps no hay mas tipos idk..
    (slot price (type FLOAT)) 																			; la mayoria sino todas son coste..
    (slot content_rating (type SYMBOL) (allowed-values Everyone Teen 10+ 17+))
    (multislot genero (type STRING)) 																	; Art & Design ; Creativity ; ...
    (slot fecha_actualiz (type STRING)) 																;esta es una fecha
    (slot current_version (type SYMBOL)) 																;estas dos ultmas son rollo 1.0.2 (and up)
    (slot android_version (type STRING))
)

;no hacer un perfil muy pesado!
(deftemplate usuario
    (slot nombre (type STRING))
    (slot sexo (type SYMBOL) (allowed-values Hombre Mujer))
    (slot edad (type INTEGER))
    (slot anio_movil (type INTEGER))
    (slot ha_pagado (type SYMBOL) (allowed-values si no))
    (slot movil_lleno (type SYMBOL) (allowed-values Mucho Normal Poco))
    (multislot usos_movil (type SYMBOL) (allowed-values Jugar Ocio Trabajo DeTodo Social Llamar Casual))    
)

(deffacts apps
	(app (nombre "Photo Editor & Candy Camera & Grid & ScrapBook") (categoria ART_AND_DESIGN) (puntuacion 4.1) (reviews 159) (size 19M) (installs 10,000+) (coste Free) (price 0.0) (content_rating Everyone) (genero "Art & Design") (fecha_actualiz "January 7, 2018") (current_version 1.0.0) (android_version "4.0.3 and up"))
	
	(app (nombre "Coloring book moana") (categoria ART_AND_DESIGN) (puntuacion 4.1) (reviews 50001) (size 14M) (installs 100,000,000+) (coste Paid) (price 5.0) (content_rating Everyone) (genero "Art & Design" "Pretend Play") (fecha_actualiz "January 15, 2018") (current_version 2.0.0) (android_version "4.0.3 and up"))
	
	(app (nombre "U Launcher Lite – FREE Live Cool Themes, Hide Apps") (categoria ART_AND_DESIGN) (puntuacion 4.7) (reviews 87510) (size 8.7M) (installs 50,000,000+) (coste Free) (price 0.0) (content_rating Everyone) (genero "Art & Design") (fecha_actualiz "August 1, 2018") (current_version 1.2.4) (android_version "4.0.3 and up"))
)

(deffacts users
	(usuario (nombre "Juan Pedro") (sexo Mujer) (edad 29) (anio_movil 2016) (ha_pagado no) (movil_lleno Normal) (usos_movil Jugar Ocio Llamar))
	(usuario (nombre "Ronaldinho") (sexo Hombre) (edad 38) (anio_movil 2019) (ha_pagado si) (movil_lleno Mucho) (usos_movil Trabajo Casual Social))
)

; tools -> trabajadores
; dating -> mayores

; mejores valoradas (mirando reviews) -> todos
; ligera -> todos
; pesadas -> poco lleno

; vieja -> movil viejo


(defrule masDescargadas
	(app (nombre ?na) (installs ?i))
	(test 
		(or (or
				(eq ?i 10,000,000+)
				(eq ?i 50,000,000+))	
			(or
				(eq ?i 100,000,000+)
				(eq ?i 1,000,000,000+))
		)
	)
	=>
	(assert (masDescargada ?na))
)

(defrule appBienValorada
	?ha <- (app (nombre ?na) (puntuacion ?p) (reviews ?r))
	(test (>= ?p 4.0))
	(test (>= ?r 50000))
	=>
	(assert (appBienValorada ?na))
)

(defrule recomiendaGratuitas
	?ha <- (app (nombre ?na) (coste Free))
	?hu <- (usuario (nombre ?nu))
	?hmd <- (masDescargada ?nmd)
	?hbv <- (appBienValorada ?nbv)

	(test (eq ?nbv ?na))
	(test (eq ?na ?nmd))
	=>
	(assert (recomendacion ?nu ?na))
	(printout t crlf "Recomendacion para " ?nu ": " ?na crlf)
)

(defrule recomiendaNoGratuitas
	?ha <- (app (nombre ?na) (coste Paid))
	?hu <- (usuario (nombre ?nu) (ha_pagado si))
	?hmd <- (masDescargada ?nmd)
	?hbv <- (appBienValorada ?nbv)

	(test (eq ?nbv ?na))
	(test (eq ?na ?nmd))
	=>
	(assert (recomendacion ?nu ?na))
	(printout t crlf "Recomendacion para " ?nu ": " ?na crlf)
)

(defrule recomiendaCitas
	?ha <- (app (nombre ?na) (categoria DATING))
	?hbv <- (appBienValorada ?nbv)
	?hmd <- (masDescargada ?nmd)
	?hu <- (usuario (nombre ?nu) (edad ?e))
	
	(test (>= ?e 18))
	(test (eq ?nbv ?na))
	(test (eq ?na ?nmd))
	=>
	(assert (recomendacion ?nu ?na))
	(printout t crlf "Recomendacion para " ?nu ": " ?na crlf)
)

(defrule recomiendaHerram
	?ha <- (app (nombre ?na) (categoria TOOLS))
	?hbv <- (appBienValorada ?nbv)
	?hmd <- (masDescargada ?nmd)
	?hu <- (usuario (nombre ?nu) (usos_movil Trabajo))

	(test (eq ?nbv ?na))
	(test (eq ?na ?nmd))
	=>
	(assert (recomendacion ?nu ?na))
	(printout t crlf "Recomendacion para " ?nu ": " ?na crlf)
)