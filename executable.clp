;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SHARED
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmodule SHARED (import ONTO ?ALL) (export ?ALL))
(defglobal ?*cost-begudes-generals* = 3.50)

(deftemplate SHARED::entrada
  (slot tipus              (allowed-symbols Boda Comunió Bateig Congrés))
  (slot epoca              (allowed-symbols Primavera Estiu Tardor Hivern Anual))
  (slot estil              (allowed-symbols Clàssic Modern Regional Sibarita))
  (slot origen_regional (default -))
  (slot comensals          (type INTEGER) (default 0))
  (slot num_adults         (type INTEGER) (default 0))
  (slot num_nens           (type INTEGER) (default 0))
  (slot num_celiacs        (type INTEGER) (default 0))
  (slot num_halal          (type INTEGER) (default 0))
  (slot num_lactosa        (type INTEGER) (default 0))
  (slot num_vegans         (type INTEGER) (default 0))
  (slot num_vegetarians    (type INTEGER) (default 0))
  (slot num_fruitsecs      (type INTEGER) (default 0))
  (slot pressupost_min     (type FLOAT) (default 0.0))
  (slot pressupost_max     (type FLOAT) (default 0.0))
  (slot menu_infantil      (allowed-symbols si no) (default no))
  (slot preferencia_beguda (allowed-symbols Vol-maridatge Vol-vi) (default Vol-vi))
  (slot num_entrants       (type INTEGER) (default 1))
  (multislot ingredients_prohibits (type INSTANCE-NAME))
  (slot pressupost_min_infantil (type FLOAT) (default 0.0))
  (slot pressupost_max_infantil (type FLOAT) (default 0.0))
  (slot preferencia_complexitat (allowed-symbols Baixa Mitjana Alta) (default Mitjana))
  (slot menu_infantil_celiac (allowed-symbols si no) (default no))
  (slot menu_infantil_halal (allowed-symbols si no) (default no))
  (slot menu_infantil_lactosa (allowed-symbols si no) (default no))
  (slot menu_infantil_vega (allowed-symbols si no) (default no))
  (slot menu_infantil_vegetaria (allowed-symbols si no) (default no))
  (slot menu_infantil_fruits_secs (allowed-symbols si no) (default no))
)

(deftemplate SHARED::preu-objectiu
  (slot opcio (allowed-symbols O1 O2 O3))
  (slot percap (type FLOAT))
)

(deftemplate SHARED::preu-objectiu-infantil
  (slot opcio (allowed-symbols O1 O2 O3))
  (slot percap (type FLOAT))
)

(deftemplate SHARED::abstracte
  (slot mida-event              (allowed-symbols Íntim Petit Mitjà Gran Massiu))
  (slot temp-preferent          (allowed-symbols Fred Calent Indiferent))
  (slot necessita-menu-infantil (allowed-symbols si no))
  (slot te-restriccions         (allowed-symbols si no))
)

(deftemplate SHARED::arquetip
  (slot menu-tipus       (allowed-symbols Clàssic Modern Regional Sibarita Especial-Dietètic Infantil))
  (slot politica-begudes (allowed-symbols Maridatge Vi-General))
  (slot estructura-ok    (allowed-symbols si no) (default no))
)

(deffunction ask$ (?prompt $?options)
  (printout t ?prompt " " $?options " -> " )
  (bind ?x (read))
  (while (not (member$ ?x (create$ $?options))) do
    (printout t "Valor invàlid. Torna-ho a provar: " $?options " -> ")
    (bind ?x (read)))
  (return ?x))

(deffunction ask-int (?prompt)
  (printout t ?prompt " -> ")
  (bind ?x (read))
  (while (not (integerp ?x)) do
    (printout t "Entra un enter: ")
    (bind ?x (read)))
  (return ?x))

(deffunction ask-float (?prompt)
  (printout t ?prompt " -> ")
  (bind ?x (read))
  (while (not (numberp ?x)) do
    (printout t "Entra un número (float): ")
    (bind ?x (read)))
  (return (float ?x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PREGUNTES (versió millorada, més natural)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmodule PREGUNTES (import SHARED ?ALL) (import ONTO ?ALL) (export ?ALL))

(deffunction PREGUNTES::pascalize (?s)
  (bind ?s (lowcase (str-cat ?s)))
  (bind ?out "") (bind ?cap TRUE)
  (bind ?len (str-length ?s)) (bind ?i 1)
  (while (<= ?i ?len) do
    (bind ?ch (sub-string ?i ?i ?s))
    (if (or (eq ?ch "_") (eq ?ch "-") (eq ?ch " "))
      then (bind ?cap TRUE)
      else
        (if ?cap then (bind ?out (str-cat ?out (upcase ?ch))) else (bind ?out (str-cat ?out ?ch)))
        (bind ?cap FALSE))
    (bind ?i (+ ?i 1)))
  (return (sym-cat ?out)))

(deffunction PREGUNTES::map-prohib-symbol->instance (?s)
  (bind ?cand  (PREGUNTES::pascalize ?s))
  (bind ?sname (sym-cat "Ing:" ?cand))
  (bind ?iname (symbol-to-instance-name ?sname))
  (if (instance-existp ?iname) then (return ?iname) else (return FALSE)))

(deffunction PREGUNTES::parse-prohibits-line->instances$ (?line)
  (if (eq ?line "-") then (return (create$)))
  (bind $?syms (explode$ ?line))
  (bind $?out (create$))
  (foreach ?tok $?syms
    (bind ?inst (PREGUNTES::map-prohib-symbol->instance ?tok))
    (if ?inst
        then (bind $?out (create$ $?out ?inst))
        else (printout t "Ingredient desconegut: " ?tok crlf)))
  (return $?out))

(deffunction PREGUNTES::ask-int-0..N (?label ?N)
  (bind ?msg (format nil "%s (0..%d):" ?label ?N))
  (bind ?x (ask-int ?msg))
  (while (or (< ?x 0) (> ?x ?N)) do
    (printout t "El valor ha d'estar entre 0 i " ?N "." crlf)
    (bind ?x (ask-int ?msg)))
  (return ?x))

(defrule PREGUNTES::demanar-dades
  (declare (salience 100))
  (not (entrada))
  =>
  (printout t "Benvingut/da a RicoRico — Assistent de Menús Personalitzats!" crlf)
  (printout t "Comencem a preparar el teu menú ideal per a l’esdeveniment." crlf)
  (printout t "Et faré unes preguntes perquè puguem adaptar-lo al màxim possible." crlf)
  (printout t "----------------------------------------------------------------" crlf crlf)

  ;; Informació general
  (bind ?tipus (ask$ "Primer de tot, quin tipus d’esdeveniment esteu organitzant?" Boda Comunió Bateig Congrés))
  (bind ?epoca (ask$ "En quina època de l’any se celebrarà o vols que estigui ambientat l'esdeveniment?" Primavera Estiu Tardor Hivern Anual))
  (bind ?estil (ask$ "Com definiries l’estil del menú que busques?" Clàssic Modern Regional Sibarita))

  (bind ?origen "-")
  (if (eq ?estil Regional)
    then
    (bind ?origen
          (ask$ "Has triat estil Regional. Quin tipus de cuina o origen prefereixes?"
                Mediterrani Nordic Mundial França Europa Espanya Japo Estatsunits Itàlia Portugal Cuba Alemanya OrientMitjà Líban Marroc Àsia)))

  (bind ?cpx (ask$ "Quin nivell de complexitat o elaboració vols per al menú?" Baixa Mitjana Alta))
  (printout t crlf "Perfecte! Ara parlem dels comensals i les seves necessitats." crlf crlf)

  ;; Informació Comensals
  (bind ?n (ask-int "Quants comensals hi haurà en total?"))
  (bind ?nn (PREGUNTES::ask-int-0..N "D’aquests, quants seran nens?" ?n))
  (bind ?ad (- ?n ?nn))
  (printout t "Resum: " ?ad " adults i " ?nn " nens (" ?n " en total)." crlf)

  (bind ?mi (if (> ?nn 0)
                then (ask$ "Vols que preparem també un menú especial per als nens?" si no)
                else no))
  (if (= ?nn 0)
      then (printout t "Menú infantil: no (no hi ha nens)" crlf)
      else (printout t "Menú infantil: " ?mi crlf))

  (printout t crlf "Ara indiquem si hi ha algun tipus de dieta especial o intolerància." crlf)

  ;; Restriccions alimentàries
  (bind ?cel (PREGUNTES::ask-int-0..N "Nombre de persones celíaques" ?n))
  (bind ?menuinf_cel no)
  (if (and (> ?nn 0) (eq ?mi si) (> ?cel 0))
      then (bind ?menuinf_cel (ask$ "Voleu menú infantil apte per a celíacs?" si no)))

  (bind ?hal (PREGUNTES::ask-int-0..N "Nombre de persones que segueixen dieta halal" ?n))
  (bind ?menuinf_halal no)
  (if (and (> ?nn 0) (eq ?mi si) (> ?hal 0))
      then (bind ?menuinf_halal (ask$ "Voleu menú infantil halal?" si no)))

  (bind ?lac (PREGUNTES::ask-int-0..N "Nombre d’intolerants a la lactosa" ?n))
  (bind ?menuinf_lac no)
  (if (and (> ?nn 0) (eq ?mi si) (> ?lac 0))
      then (bind ?menuinf_lac (ask$ "Voleu menú infantil sense lactosa?" si no)))

  (bind ?vegans (PREGUNTES::ask-int-0..N "Nombre de persones veganes" ?n))
  (bind ?menuinf_vega no)
  (if (and (> ?nn 0) (eq ?mi si) (> ?vegans 0))
      then (bind ?menuinf_vega (ask$ "Voleu menú infantil vegà?" si no)))

  (bind ?vege (PREGUNTES::ask-int-0..N "Nombre de persones vegetarianes" ?n))
  (bind ?menuinf_vege no)
  (if (and (> ?nn 0) (eq ?mi si) (> ?vege 0))
      then (bind ?menuinf_vege (ask$ "Voleu menú infantil vegetarià?" si no)))

  (bind ?nuts (PREGUNTES::ask-int-0..N "Nombre de persones al·lèrgiques als fruits secs" ?n))
  (bind ?menuinf_fs no)
  (if (and (> ?nn 0) (eq ?mi si) (> ?nuts 0))
      then (bind ?menuinf_fs (ask$ "Voleu menú infantil sense fruits secs?" si no)))

  ;; Pressupostos
  (printout t crlf "Indica ara el pressupost aproximat per persona (adults)." crlf)
  (bind ?pmin (ask-float "Pressupost mínim €/pax (adults):"))
  (bind ?pmax (ask-float "Pressupost màxim €/pax (adults):"))
  (while (< ?pmax ?pmin) do
    (printout t "El màxim ha de ser igual o superior al mínim. Torna-ho a introduir." crlf)
    (bind ?pmin (ask-float "Pressupost mínim €/pax (adults):"))
    (bind ?pmax (ask-float "Pressupost màxim €/pax (adults):")))

  (bind ?pmin_i 0.0)
  (bind ?pmax_i 0.0)
  (if (eq ?mi si) then
    (printout t crlf "I ara el pressupost per al menú infantil:" crlf)
    (bind ?pmin_i (ask-float "Pressupost mínim €/pax (infantil):"))
    (bind ?pmax_i (ask-float "Pressupost màxim €/pax (infantil):"))
    (while (< ?pmax_i ?pmin_i) do
      (printout t "El màxim ha de ser ≥ el mínim. Torna-ho a entrar." crlf)
      (bind ?pmin_i (ask-float "Pressupost mínim €/pax (infantil):"))
      (bind ?pmax_i (ask-float "Pressupost màxim €/pax (infantil):"))))

  ;; Begudes i estructura del menú
  (bind ?beg (ask$ "Quina és la teva preferència de beguda principal?" Vol-maridatge Vol-vi))
  (bind ?qent (ask-int "Quants entrants vols que tingui el menú?"))

  ;; Ingredients prohibits 
  (printout t crlf "Vols evitar algun ingredient concret al menú?" crlf)
  (printout t "Escriu els ingredients principals separats per espais (ex.: 'lactosa gluten marisc')." crlf)
  (printout t "Si no n’hi ha cap, escriu simplement '-' i prem Enter." crlf)
  (bind ?line (readline))
  (bind $?proh (PREGUNTES::parse-prohibits-line->instances$ ?line))

  ;; Creació del fet entrada
  (assert (entrada
              (tipus ?tipus) (epoca ?epoca) (estil ?estil)
              (origen_regional ?origen)
              (comensals ?n) (num_adults ?ad) (num_nens ?nn)
              (num_celiacs ?cel) (num_halal ?hal) (num_lactosa ?lac)
              (num_vegans ?vegans) (num_vegetarians ?vege) (num_fruitsecs ?nuts)
              (menu_infantil_celiac ?menuinf_cel)
              (menu_infantil_halal ?menuinf_halal)
              (menu_infantil_lactosa ?menuinf_lac)
              (menu_infantil_vegetaria ?menuinf_vege)
              (menu_infantil_vega ?menuinf_vega)
              (menu_infantil_fruits_secs ?menuinf_fs)
              (preferencia_complexitat ?cpx)
              (menu_infantil ?mi) (preferencia_beguda ?beg)
              (num_entrants ?qent)
              (ingredients_prohibits $?proh)
              (pressupost_min ?pmin) (pressupost_max ?pmax)
              (pressupost_min_infantil ?pmin_i) (pressupost_max_infantil ?pmax_i)))

  (printout t crlf)
  (printout t "Les teves respostes s’han registrat correctament." crlf)
  (printout t "Ara començarem a generar propostes de menú adaptades al que ens has indicat." crlf crlf)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ABSTRACCIO
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmodule ABSTRACCIO (import SHARED ?ALL) (export ?ALL))

(defrule ABSTRACCIO::mida-intim   (entrada (comensals ?n&:(< ?n 25)))  => (assert (abstracte (mida-event Íntim))))
(defrule ABSTRACCIO::mida-petit   (entrada (comensals ?n&:(and (>= ?n 25) (< ?n 60))))  => (assert (abstracte (mida-event Petit))))
(defrule ABSTRACCIO::mida-mitja   (entrada (comensals ?n&:(and (>= ?n 60) (< ?n 120)))) => (assert (abstracte (mida-event Mitjà))))
(defrule ABSTRACCIO::mida-gran    (entrada (comensals ?n&:(and (>= ?n 120) (< ?n 250)))) => (assert (abstracte (mida-event Gran))))
(defrule ABSTRACCIO::mida-massiu  (entrada (comensals ?n&:(>= ?n 250))) => (assert (abstracte (mida-event Massiu))))

(defrule ABSTRACCIO::derivar-preus-objectiu
  (entrada (pressupost_min ?pmin) (pressupost_max ?pmax))
  (not (preu-objectiu))
  =>
  (bind ?mid (/ (+ ?pmin ?pmax) 2.0))
  (assert (preu-objectiu (opcio O1) (percap ?pmin)))
  (assert (preu-objectiu (opcio O2) (percap ?mid)))
  (assert (preu-objectiu (opcio O3) (percap ?pmax))))

(defrule ABSTRACCIO::derivar-preus-objectiu-infantil-triple
  (entrada (pressupost_min_infantil ?pmini) (pressupost_max_infantil ?pmaxi))
  (test (> ?pmaxi 0.0))
  (not (preu-objectiu-infantil (opcio O1)))
  =>
  (bind ?mid (/ (+ ?pmini ?pmaxi) 2.0))
  (assert (preu-objectiu-infantil (opcio O1) (percap ?pmini)))
  (assert (preu-objectiu-infantil (opcio O2) (percap ?mid)))
  (assert (preu-objectiu-infantil (opcio O3) (percap ?pmaxi))))

(defrule ABSTRACCIO::tempe-estiu   (entrada (epoca Estiu))     => (assert (abstracte (temp-preferent Fred))))
(defrule ABSTRACCIO::tempe-hivern  (entrada (epoca Hivern))    => (assert (abstracte (temp-preferent Calent))))
(defrule ABSTRACCIO::tempe-altres  (entrada (epoca ?e&:(or (eq ?e Primavera) (eq ?e Tardor) (eq ?e Anual))))
                                   => (assert (abstracte (temp-preferent Indiferent))))

(defrule ABSTRACCIO::necessita-infantil
  (entrada (num_nens ?nn&:(> ?nn 0)) (menu_infantil si))
  => (assert (abstracte (necessita-menu-infantil si))))

(defrule ABSTRACCIO::no-necessita-infantil
  (or (entrada (num_nens ?nn&:(= ?nn 0))) (entrada (menu_infantil no)))
  => (assert (abstracte (necessita-menu-infantil no))))

(defrule ABSTRACCIO::hi-ha-restriccions
  (entrada
    (num_celiacs ?a) (num_halal ?b) (num_lactosa ?c)
    (num_vegans ?d) (num_vegetarians ?e) (num_fruitsecs ?f))
  (test (> (+ ?a ?b ?c ?d ?e ?f) 0))
  => (assert (abstracte (te-restriccions si))))

(defrule ABSTRACCIO::no-hi-ha-restriccions
  (entrada
    (num_celiacs ?a) (num_halal ?b) (num_lactosa ?c)
    (num_vegans ?d) (num_vegetarians ?e) (num_fruitsecs ?f))
  (test (= (+ ?a ?b ?c ?d ?e ?f) 0))
  => (assert (abstracte (te-restriccions no))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ASSOCIACIO  — (evitem duplicats i ping-pong)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmodule ASSOCIACIO (import SHARED ?ALL) (export ?ALL))

;; Arquetips bàsics
(defrule ASSOCIACIO::arquetip-cl
  (declare (salience 40))
  (entrada (estil Clàssic))
  (abstracte (necessita-menu-infantil no))
  (abstracte (te-restriccions no))
  (not (arquetip (menu-tipus Clàssic)))
  =>
  (assert (arquetip (menu-tipus Clàssic))))

(defrule ASSOCIACIO::arquetip-md
  (declare (salience 40))
  (entrada (estil Modern))
  (abstracte (necessita-menu-infantil no))
  (abstracte (te-restriccions no))
  (not (arquetip (menu-tipus Modern)))
  =>
  (assert (arquetip (menu-tipus Modern))))

(defrule ASSOCIACIO::arquetip-rg
  (declare (salience 40))
  (entrada (estil Regional))
  (abstracte (necessita-menu-infantil no))
  (abstracte (te-restriccions no))
  (not (arquetip (menu-tipus Regional)))
  =>
  (assert (arquetip (menu-tipus Regional))))

(defrule ASSOCIACIO::arquetip-sb
  (declare (salience 40))
  (entrada (estil Sibarita))
  (abstracte (necessita-menu-infantil no))
  (abstracte (te-restriccions no))
  (not (arquetip (menu-tipus Sibarita)))
  =>
  (assert (arquetip (menu-tipus Sibarita))))

;; Arquetip Infantil 
(defrule ASSOCIACIO::arquetip-infantil
  (declare (salience 35))
  (abstracte (necessita-menu-infantil si))
  (not (arquetip (menu-tipus Infantil)))
  =>
  (assert (arquetip (menu-tipus Infantil))))

;; Arquetip Especial-Dietètic
(defrule ASSOCIACIO::arquetip-especial
  (declare (salience 30))
  (abstracte (necessita-menu-infantil no))
  (abstracte (te-restriccions si))
  (not (arquetip (menu-tipus Especial-Dietètic)))
  =>
  (assert (arquetip (menu-tipus Especial-Dietètic))))

;; Política de begudes
(defrule ASSOCIACIO::begudes-maridatge
  (declare (salience 20))
  (entrada (preferencia_beguda Vol-maridatge))
  (not (arquetip (politica-begudes Maridatge)))
  =>
  (assert (arquetip (politica-begudes Maridatge))))

(defrule ASSOCIACIO::begudes-vi-general
  (declare (salience 20))
  (entrada (preferencia_beguda Vol-vi))
  (not (arquetip (politica-begudes Vi-General)))
  =>
  (assert (arquetip (politica-begudes Vi-General))))

;; Marcat d'estructura OK
(defrule ASSOCIACIO::estructura-ok
  (declare (salience 10))
  (not (arquetip (estructura-ok si)))
  =>
  (assert (arquetip (estructura-ok si))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ONTOBRIDGE — mapes d’entrada → instàncies de l’ontologia
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmodule ONTOBRIDGE (import SHARED ?ALL) (import ONTO ?ALL) (export ?ALL))

(deftemplate ONTOBRIDGE::epoca-inst     (slot inst))
(deftemplate ONTOBRIDGE::estil-inst     (slot inst))
(deftemplate ONTOBRIDGE::tipusplat-inst (slot entr) (slot pri) (slot seg) (slot post))

(defrule ONTOBRIDGE::map-epoca-estiu     (entrada (epoca Estiu))     => (assert (epoca-inst (inst [TemporadaEstiu]))))
(defrule ONTOBRIDGE::map-epoca-hivern    (entrada (epoca Hivern))    => (assert (epoca-inst (inst [TemporadaHivern]))))
(defrule ONTOBRIDGE::map-epoca-primavera (entrada (epoca Primavera)) => (assert (epoca-inst (inst [TemporadaPrimavera]))))
(defrule ONTOBRIDGE::map-epoca-tardor    (entrada (epoca Tardor))    => (assert (epoca-inst (inst [TemporadaTardor]))))
(defrule ONTOBRIDGE::map-epoca-anual     (entrada (epoca Anual))     => (assert (epoca-inst (inst [TemporadaAnual]))))

(defrule ONTOBRIDGE::map-estil-classic   (entrada (estil Clàssic))  => (assert (estil-inst (inst [PlatClassic]))))
(defrule ONTOBRIDGE::map-estil-modern    (entrada (estil Modern))   => (assert (estil-inst (inst [PlatModern]))))
(defrule ONTOBRIDGE::map-estil-regional  (entrada (estil Regional)) => (assert (estil-inst (inst [PlatRegional]))))
(defrule ONTOBRIDGE::map-estil-sibarita  (entrada (estil Sibarita)) => (assert (estil-inst (inst [PlatSibarita]))))

(defrule ONTOBRIDGE::map-tipus-plat
  (not (tipusplat-inst))
  =>
  (assert (tipusplat-inst
           (entr [PlatEntrants]) (pri [PlatPrimer]) (seg [PlatSegon]) (post [PlatPostre]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; REFINAMENT (matching suau + puntuació)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmodule REFINAMENT
  (import SHARED ?ALL) (import ONTO ?ALL) (import ONTOBRIDGE ?ALL)
  (export ?ALL))

;; Helpers instàncies
(deffunction REFINAMENT::to-addr (?x)
  (if (instancep ?x) then (return ?x))
  (if (instance-namep ?x) then
    (if (instance-existp ?x) then (return (instance-address ?x)) else (return FALSE)))
  (if (symbolp ?x) then
    (bind ?iname (symbol-to-instance-name ?x))
    (if (instance-existp ?iname) then (return (instance-address ?iname)) else (return FALSE)))
  (return FALSE))

(deffunction REFINAMENT::name-of (?x)
  (bind ?a (REFINAMENT::to-addr ?x))
  (if ?a then
    (return (instance-name-to-symbol (instance-name ?a)))
    else (return "-")))

(deffunction REFINAMENT::preu-of (?x)
  (bind ?a (REFINAMENT::to-addr ?x))
  (if (not ?a) then (return 0.0))
  (return (send ?a get-preu)))

(deffunction REFINAMENT::sum-preus$ ($?xs)
  (bind ?s 0.0)
  (foreach ?x $?xs
    (bind ?a (REFINAMENT::to-addr ?x))
    (if ?a then (bind ?s (+ ?s (REFINAMENT::preu-of ?a)))))
  (return ?s))

;; Helpers de normalització
(deffunction REFINAMENT::symbol-of (?x)
  (if (instancep ?x) then (return (instance-name-to-symbol (instance-name ?x))))
  (if (instance-namep ?x) then (return (instance-name-to-symbol ?x)))
  (return ?x))

(deffunction REFINAMENT::lower-ns (?x)
  (bind ?s (lowcase (str-cat ?x)))
  (bind ?s (str-replace ?s " " "")) (bind ?s (str-replace ?s "_" ""))
  (bind ?s (str-replace ?s "," "")) (bind ?s (str-replace ?s "-" ""))
  (bind ?s (str-replace ?s "à" "a")) (bind ?s (str-replace ?s "è" "e"))
  (bind ?s (str-replace ?s "é" "e")) (bind ?s (str-replace ?s "í" "i"))
  (bind ?s (str-replace ?s "ï" "i")) (bind ?s (str-replace ?s "ò" "o"))
  (bind ?s (str-replace ?s "ó" "o")) (bind ?s (str-replace ?s "ú" "u"))
  (bind ?s (str-replace ?s "ü" "u"))
  (return ?s))

(deffunction REFINAMENT::nom-epoca-net (?inst)
   (if (not (instancep ?inst)) then (return ?inst))
   (bind ?nom (instance-name-to-symbol ?inst))
   (return (lowcase (sub-string 10 (str-length ?nom) ?nom))))

(deffunction REFINAMENT::nom-estil-net (?inst)
   (if (not (instancep ?inst))
      then (bind ?txt (str-cat ?inst))
      else (bind ?txt (instance-name-to-symbol ?inst)))
   (if (str-index "Estil" ?txt)
       then (bind ?txt (sub-string 6 (str-length ?txt) ?txt)))
   (if (str-index "Plat" ?txt)
       then (bind ?txt (sub-string 5 (str-length ?txt) ?txt)))
   (bind ?txt (lowcase ?txt))
   (bind ?txt (str-replace ?txt "à" "a"))
   (bind ?txt (str-replace ?txt "á" "a"))
   (bind ?txt (str-replace ?txt "è" "e"))
   (bind ?txt (str-replace ?txt "é" "e"))
   (bind ?txt (str-replace ?txt "í" "i"))
   (bind ?txt (str-replace ?txt "ï" "i"))
   (bind ?txt (str-replace ?txt "ò" "o"))
   (bind ?txt (str-replace ?txt "ó" "o"))
   (bind ?txt (str-replace ?txt "ú" "u"))
   (bind ?txt (str-replace ?txt "ü" "u"))
   (return ?txt))

(deffunction REFINAMENT::nom-origen-net (?inst)
   (if (not (instancep ?inst)) then (return (lowcase (str-cat ?inst))))
   (bind ?txt (str-cat (instance-name ?inst)))
   (if (str-index "Org" ?txt)
       then (bind ?txt (sub-string 5 (str-length ?txt) ?txt)))
   (bind ?txt (lowcase ?txt))
   (bind ?txt (str-replace ?txt "à" "a"))
   (bind ?txt (str-replace ?txt "á" "a"))
   (bind ?txt (str-replace ?txt "è" "e"))
   (bind ?txt (str-replace ?txt "é" "e"))
   (bind ?txt (str-replace ?txt "í" "i"))
   (bind ?txt (str-replace ?txt "ï" "i"))
   (bind ?txt (str-replace ?txt "ò" "o"))
   (bind ?txt (str-replace ?txt "ó" "o"))
   (bind ?txt (str-replace ?txt "ú" "u"))
   (bind ?txt (str-replace ?txt "ü" "u"))
   (return ?txt))

(deffunction REFINAMENT::plat-apte? (?plat ?restriccio)
  (bind ?addr (REFINAMENT::to-addr ?plat))
  (if (not ?addr) then (return FALSE))

  (bind ?d (send ?addr get-dieta))

  (bind ?diet-name
        (if (instancep ?d)
            then (instance-name-to-symbol (instance-name ?d))
            else ?d))

  (bind ?dieta (REFINAMENT::lower-ns ?diet-name))

  (switch ?restriccio
    (case celiac then
      (return (or (str-index "sensegluten" ?dieta)
                  (str-index "glutenfree"  ?dieta))))
    (case halal then
      (return (str-index "halal" ?dieta)))
    (case lactosa then
      (return (or (str-index "senselactosa" ?dieta)
                  (str-index "lactosefree"  ?dieta))))
    (case vega then
      (return (or (str-index "vegan" ?dieta)
                  (str-index "vega"  ?dieta))))
    (case vegetaria then
      (return (str-index "vegetari" ?dieta)))
    (case fruitsecs then
      (return (or (str-index "sensefruitssecs" ?dieta)
                  (str-index "sensefruitsecs" ?dieta)
                  (str-index "nofruits" ?dieta))))
    (default (return FALSE))
  )
)

(deftemplate preu-real-infantil
   (slot opcio)
   (slot percap (type FLOAT)))

(deffunction REFINAMENT::preu-menu-percap-des-de-binds
  (?P ?S ?PO ?Elist ?BGlist ?BPRlist ?BSElist ?BPOlist)
  "Calcula el preu del menú per persona:
   - Entrants compartits entre 6 persones
   - Begudes compartides entre 4 persones
   - Primer, Segon i Postre sencers per persona"

  ;; Entrants compartits (entre 6 persones)
  (bind ?sumE (REFINAMENT::sum-preus$ ?Elist))
  (bind ?sum (/ ?sumE 6.0))

  ;; Plats principals (sencers per persona)
  (if (REFINAMENT::to-addr ?P)  then (bind ?sum (+ ?sum (REFINAMENT::preu-of ?P))))
  (if (REFINAMENT::to-addr ?S)  then (bind ?sum (+ ?sum (REFINAMENT::preu-of ?S))))
  (if (REFINAMENT::to-addr ?PO) then (bind ?sum (+ ?sum (REFINAMENT::preu-of ?PO))))

  ;; Begudes compartides (entre 4 persones)
  (bind ?sumB 0.0)
  (foreach ?b ?BGlist  (bind ?sumB (+ ?sumB (REFINAMENT::preu-of ?b))))
  (foreach ?b ?BPRlist (bind ?sumB (+ ?sumB (REFINAMENT::preu-of ?b))))
  (foreach ?b ?BSElist (bind ?sumB (+ ?sumB (REFINAMENT::preu-of ?b))))
  (foreach ?b ?BPOlist (bind ?sumB (+ ?sumB (REFINAMENT::preu-of ?b))))

  (bind ?sum (+ ?sum (/ ?sumB 4.0)))
  (return ?sum))

;; Utilitats de llista
(deffunction REFINAMENT::take$ (?n $?xs)
  (bind ?out (create$)) (bind ?i 1)
  (while (and (<= ?i ?n) (<= ?i (length$ ?xs))) do
    (bind ?out (create$ ?out (nth$ ?i $?xs))) (bind ?i (+ ?i 1)))
  (return $?out))

(deffunction REFINAMENT::drop$ (?n $?xs)
  (if (<= ?n 0) then (return $?xs))
  (bind ?l (length$ ?xs))
  (if (> ?n ?l) then (return (create$)))
  (return (subseq$ $?xs (+ ?n 1) ?l)))

(deffunction REFINAMENT::repeat-until$ (?target $?xs)
  (if (or (<= ?target 0) (= (length$ ?xs) 0))
      then (return (create$)))
  (bind ?acc (create$))
  (while (< (length$ ?acc) ?target) do
    (bind ?acc (create$ ?acc $?xs)))
  (return (REFINAMENT::take$ ?target $?acc)))

;; Nivell per opció
(deffunction REFINAMENT::nivell-per-opcio (?op)
  (if (eq ?op O1) then (return Baixa))
  (if (eq ?op O2) then (return Mitjana))
  (return Alta))

;; Toleràncies / compatibilitats
(deffunction REFINAMENT::cx-accepta (?target ?cx)
  (bind ?t (if (eq ?target Baixa) then baixa else (if (eq ?target Mitjana) then mitjana else alta)))
  (if (eq ?cx ?t) then (return TRUE))
  (if (and (eq ?t mitjana) (or (eq ?cx baixa) (eq ?cx mitjana))) then (return TRUE))
  (if (and (eq ?t alta) TRUE) then (return TRUE))
  (return (eq ?cx baixa)))

(deffunction REFINAMENT::temp-compat (?pref ?tplat)
  (if (eq ?pref Indiferent) then (return TRUE))
  (if (eq ?pref Fred)  then (return (or (eq ?tplat fred) (eq ?tplat temperatura-freda) (eq ?tplat freda))))
  (if (eq ?pref Calent) then (return (or (eq ?tplat calent) (eq ?tplat temperatura-calenta) (eq ?tplat calenta))))
  (return TRUE))

(deffunction REFINAMENT::prohibit? (?ing $?prohs)
  (bind ?ing-sym (REFINAMENT::symbol-of ?ing))
  (bind ?ing-key (REFINAMENT::lower-ns ?ing-sym))
  (foreach ?p $?prohs
    (if (eq ?ing-key (REFINAMENT::lower-ns (REFINAMENT::symbol-of ?p)))
        then (return TRUE)))
  (return FALSE))

;; Helpers de tipus
(deffunction REFINAMENT::es-tipus? (?x ?TP)
  (return (eq (send ?x get-tipus-plat) ?TP)))

;; Puntuació de plats
(deffunction REFINAMENT::temporada-score (?plat ?EPOCA)
  (bind ?t (send ?plat get-temporada))
  (if (not ?t) then (return 0))
  (if (eq ?t [TemporadaAnual]) then (return 2))
  (if (eq ?t ?EPOCA) then (return 3))
  (return 0))

(deffunction REFINAMENT::complexitat-score (?plat ?nivell)
  (bind ?cx (send ?plat get-complexitat))
  (bind ?t (if (eq ?nivell Baixa) then baixa else (if (eq ?nivell Mitjana) then mitjana else alta)))
  (if (eq ?cx ?t) then (return 2))
  (if (REFINAMENT::cx-accepta ?nivell ?cx) then (return 1))
  (return 0))

(deffunction REFINAMENT::estil-score (?plat ?ESTIL)
  (bind ?e (send ?plat get-estil_plat))
  (if (not ?e) then (return 0))                           
  (if (eq ?e [EstilGeneral]) then (return 2))
  (if (eq ?e ?ESTIL) then (return 3))
  (return 0))

(deffunction REFINAMENT::regional-score (?plat ?ESTIL)
  
  (if (neq ?ESTIL [EstilRegional]) then (return 0))

  (bind ?orig "-")
  (do-for-fact ((?e entrada)) TRUE
    (if (neq (lowcase (str-cat ?e:origen_regional)) "-")
      then (bind ?orig (lowcase (str-cat ?e:origen_regional)))))

  (if (eq ?orig "-") then (return 0))

  (bind ?org-inst (send ?plat get-origen))
  (if (not (instancep ?org-inst)) then (return 0))
  (bind ?nom (lowcase (str-cat (send ?org-inst get-nom))))

  (if (eq ?nom ?orig) then
    (return 5)
    (if (str-index ?orig ?nom) then
      (return 2)
      (return 0)))
)

(deffunction REFINAMENT::score-plat-total (?plat ?EPOCA ?ESTIL ?nivell)
  (bind ?sTemp (REFINAMENT::temporada-score ?plat ?EPOCA))
  (bind ?sEstil (REFINAMENT::estil-score ?plat ?ESTIL))
  (bind ?sCompl (REFINAMENT::complexitat-score ?plat ?nivell))
  (bind ?sReg (REFINAMENT::regional-score ?plat ?ESTIL)) 

  (bind ?score (+ (* 5 ?sTemp) (* 4 ?sEstil) (* 6 ?sReg) ?sCompl))

  ;; Penalització si no coincideix res rellevant
  (if (and (< ?sTemp 2) (< ?sEstil 2) (= ?sReg 0)) then
    (bind ?score (- ?score 3)))

  (return ?score)
)

(deffunction REFINAMENT::pool-entrants$-per-nivell
  (?nivell ?EPOCA ?ESTIL $?PROHS)
  (bind ?orig "-")
  (do-for-fact ((?e entrada))
    TRUE
    (if (neq (lowcase (str-cat ?e:origen_regional)) "-")
      then (bind ?orig (lowcase (str-cat ?e:origen_regional)))))

  (if (neq ?orig "-") then
    (bind $?tots (find-all-instances ((?x ONTO::Plat))
                     (REFINAMENT::es-tipus? ?x [PlatEntrants])))

    (bind $?cands (create$))
    (foreach ?x $?tots
      (bind ?org-inst (send ?x get-origen))
      (if (and (instancep ?org-inst)
               (eq (lowcase (str-cat (send ?org-inst get-nom))) ?orig))
        then (bind $?cands (create$ $?cands ?x))))

    (if (= (length$ $?cands) 0) then
      (printout t "[AVÍS] No hi ha entrants exactament d’origen " ?orig
                     ". Es prioritzaran plats amb estil Regional o similars." crlf)
      (bind $?cands (find-all-instances ((?x ONTO::Plat))
                       (and (REFINAMENT::es-tipus? ?x [PlatEntrants])
                            (eq (send ?x get-estil_plat) [EstilRegional])))))

    (if (= (length$ $?cands) 0) then
      (bind $?cands $?tots))

    (bind $?valids (create$))
    (foreach ?x $?cands
      (if (not (REFINAMENT::prohibit? (send ?x get-ingredient-principal) $?PROHS))
        then (bind $?valids (create$ $?valids ?x))))

    (if (= (length$ $?valids) 0) then
      (printout t "[AVÍS] Cap entrant compleix totes les restriccions. Es farà servir tot el conjunt d’origen." crlf)
      (bind $?valids $?cands))

    (return $?valids)
  else
    (bind $?cands (find-all-instances ((?x ONTO::Plat))
                     (and (REFINAMENT::es-tipus? ?x [PlatEntrants])
                          (not (REFINAMENT::prohibit? (send ?x get-ingredient-principal) $?PROHS)))))
    (return $?cands)
  )
)

(deffunction REFINAMENT::pick-una-per-tipus
  (?TIPUS ?nivell ?EPOCA ?ESTIL $?PROHS)
  "Tria un plat del tipus indicat aplicant filtres de prohibicions i origen regional si s’ha demanat,
   prioritzant fortament plats que coincideixin amb època i estil de l’usuari, però garantint sempre una opció final."

  (bind ?orig "-")
  (do-for-fact ((?e entrada)) TRUE
    (if (neq (lowcase (str-cat ?e:origen_regional)) "-")
      then (bind ?orig (lowcase (str-cat ?e:origen_regional)))))

  (bind $?tots (find-all-instances ((?x ONTO::Plat))
                   (REFINAMENT::es-tipus? ?x ?TIPUS)))

  (bind $?cands (create$))
  (if (neq ?orig "-") then
    (foreach ?x $?tots
      (bind ?org-inst (send ?x get-origen))
      (if (and (instancep ?org-inst)
               (eq (lowcase (str-cat (send ?org-inst get-nom))) ?orig))
          then (bind $?cands (create$ $?cands ?x))))
    (if (= (length$ $?cands) 0) then
      (printout t "[AVÍS] No hi ha plats del tipus " ?TIPUS
                     " amb origen " ?orig ". Es farà cerca sense origen." crlf)
      (bind $?cands $?tots))
  else
    (bind $?cands $?tots))

  (bind $?valids (create$))
  (foreach ?x $?cands
    (if (not (REFINAMENT::prohibit? (send ?x get-ingredient-principal) $?PROHS))
        then (bind $?valids (create$ $?valids ?x))))

  (if (= (length$ $?valids) 0) then
    (printout t "[AVÍS] Cap plat compleix totes les restriccions. Es relaxa la cerca." crlf)
    (bind $?valids $?cands))

  (if (= (length$ $?valids) 0) then
    (printout t "[AVÍS] No hi ha cap plat compatible. Es triarà un plat aleatori del tipus " ?TIPUS "." crlf)
    (bind $?valids $?tots))

  (bind ?best FALSE)
  (bind ?bestS -999)
  (foreach ?c $?valids
    (bind ?s (REFINAMENT::score-plat-total ?c ?EPOCA ?ESTIL ?nivell))
    (if (> ?s ?bestS) then
      (bind ?best ?c)
      (bind ?bestS ?s)))

  (if (or (not ?best) (<= ?bestS 0)) then
    (bind ?i (+ 1 (random 0 (- (length$ $?valids) 1))))
    (bind ?best (nth$ ?i $?valids))
    (printout t "[AVÍS] No hi ha cap plat que coincideixi amb època ni estil. Seleccionat un a l’atzar." crlf))

  (return ?best)
)

(deffunction REFINAMENT::pick-una-per-tipus-restriccio
  (?TIPUS ?nivell ?EPOCA ?ESTIL ?restriccio $?PROHS)
  "Tria un plat del tipus indicat que sigui preferentment apte per la restricció. 
   Si no n'hi ha, en tria un del tipus correcte."

  (bind $?cands (find-all-instances ((?x ONTO::Plat))
                  (and (REFINAMENT::es-tipus? ?x ?TIPUS)
                       (REFINAMENT::plat-apte? ?x ?restriccio)
                       (not (REFINAMENT::prohibit? (send ?x get-ingredient-principal) $?PROHS)))))

  (if (= (length$ $?cands) 0) then
    (bind $?cands (find-all-instances ((?x ONTO::Plat))
                     (and (REFINAMENT::es-tipus? ?x ?TIPUS)
                          (not (REFINAMENT::prohibit? (send ?x get-ingredient-principal) $?PROHS)))))
    (printout t "[AVÍS] No s'han trobat plats 100% aptes per la restricció " ?restriccio
                  ". Triant-ne un del tipus correcte." crlf))

  (if (= (length$ $?cands) 0) then (return FALSE))

  (bind ?i (+ 1 (random 0 (- (length$ $?cands) 1))))
  (return (nth$ ?i $?cands)))

;; versions "normals" per cada tipus
(deffunction REFINAMENT::pick-primer-inst-per-nivell (?nivell ?EPOCA ?ESTIL $?PROHS)
  (return (REFINAMENT::pick-una-per-tipus [PlatPrimer] ?nivell ?EPOCA ?ESTIL $?PROHS)))

(deffunction REFINAMENT::pick-segon-inst-per-nivell  (?nivell ?EPOCA ?ESTIL $?PROHS)
  (return (REFINAMENT::pick-una-per-tipus [PlatSegon]  ?nivell ?EPOCA ?ESTIL $?PROHS)))

(deffunction REFINAMENT::pick-postre-inst-per-nivell (?nivell ?EPOCA ?ESTIL $?PROHS)
  (return (REFINAMENT::pick-una-per-tipus [PlatPostre] ?nivell ?EPOCA ?ESTIL $?PROHS)))

;; versions "amb restricció" per cada tipus
(deffunction REFINAMENT::pick-primer-inst-per-restriccio (?nivell ?EPOCA ?ESTIL ?restriccio $?PROHS)
  (return (REFINAMENT::pick-una-per-tipus-restriccio [PlatPrimer] ?nivell ?EPOCA ?ESTIL ?restriccio $?PROHS)))

(deffunction REFINAMENT::pick-segon-inst-per-restriccio  (?nivell ?EPOCA ?ESTIL ?restriccio $?PROHS)
  (return (REFINAMENT::pick-una-per-tipus-restriccio [PlatSegon]  ?nivell ?EPOCA ?ESTIL ?restriccio $?PROHS)))

(deffunction REFINAMENT::pick-postre-inst-per-restriccio (?nivell ?EPOCA ?ESTIL ?restriccio $?PROHS)
  (return (REFINAMENT::pick-una-per-tipus-restriccio [PlatPostre] ?nivell ?EPOCA ?ESTIL ?restriccio $?PROHS)))

;; Begudes
(deffunction REFINAMENT::begudes-general$-per-nivell (?nivell)
  (bind $?b (find-all-instances ((?b ONTO::Beguda)) TRUE))
  (if (> (length$ $?b) 0) then (return (REFINAMENT::take$ 2 $?b)) else (return (create$))))
(deffunction REFINAMENT::beguda-maridatge-primer ()
  (bind $?b (find-all-instances ((?b ONTO::Beguda)) TRUE))
  (if (> (length$ $?b) 0) then (return (nth$ 1 $?b)) else (return FALSE)))
(deffunction REFINAMENT::beguda-maridatge-segon  ()
  (bind $?b (find-all-instances ((?b ONTO::Beguda)) TRUE))
  (if (> (length$ $?b) 1) then (return (nth$ 2 $?b)) else (return FALSE)))
(deffunction REFINAMENT::beguda-maridatge-postre ()
  (bind $?b (find-all-instances ((?b ONTO::Beguda)) TRUE))
  (if (> (length$ $?b) 2) then (return (nth$ 3 $?b)) else (return FALSE)))

(deftemplate REFINAMENT::_pool-entrants (multislot tots))
(deftemplate REFINAMENT::menu-opcio-preu (slot qui) (slot opcio) (slot percap))

(deftemplate REFINAMENT::impressio-feta
  (slot opcio
    (allowed-symbols
      O1 O2 O3
      InfantilO1 InfantilO2 InfantilO3
      Infantil Celiac Halal Lactosa Vegetaria Vega Fruitsecs
      InfantilCeliac InfantilHalal InfantilLactosa
      InfantilVegetaria InfantilVega InfantilFruitsecs
      BegudaRecomanada
      PastisBoda)))

(defrule REFINAMENT::preparar-pool-entrants
  (tipusplat-inst (entr ?TENT))
  (epoca-inst (inst ?EPOCA))
  (estil-inst (inst ?ESTIL))
  (entrada (num_entrants ?NE) (ingredients_prohibits $?PROHS))
  (not (_pool-entrants))
  =>
  (bind $?POOL (REFINAMENT::pool-entrants$-per-nivell Mitjana ?EPOCA ?ESTIL $?PROHS))

  (if (= (length$ $?POOL) 0) then
    (printout t "[AVÍS] No s'han trobat entrants compatibles. Es continuarà amb la millor aproximació possible." crlf)
    (assert (_pool-entrants (tots)))
   else
    (bind ?total (* ?NE 3))
    (bind $?ALL (REFINAMENT::repeat-until$ ?total $?POOL))
    (assert (_pool-entrants (tots $?ALL)))))

;; Nivell per opció (fixat segons preferència de l’usuari)
(deffunction REFINAMENT::nivell-per-opcio2 (?op)
  "Retorna el nivell de complexitat real demanat per l’usuari a l'entrada.
   Aplica tant per menús adults com infantils (O1/O2/O3, InfantilO1/2/3)."

  (bind ?nivell Mitjana)
  (do-for-fact ((?e entrada)) TRUE
    (bind ?nivell ?e:preferencia_complexitat))

  (return ?nivell))

;; Construcció de menús segons preu objectiu
(defrule REFINAMENT::construir-menu-per-opcio-O1
  (arquetip (politica-begudes ?pol))
  (epoca-inst (inst ?EPOCA))
  (estil-inst (inst ?ESTIL))
  (entrada (num_entrants ?NE)
           (ingredients_prohibits $?PROHS)
           (preferencia_complexitat ?CPX))
  (preu-objectiu (opcio O1) (percap ?target))
  ?pool <- (_pool-entrants (tots $?ALL))
  (not (object (name [MenuOpcio1])))
  =>
  (bind ?nivell (REFINAMENT::nivell-per-opcio O1))
  (bind $?E (REFINAMENT::take$ ?NE $?ALL))
  (modify ?pool (tots (REFINAMENT::drop$ ?NE $?ALL)))

  (bind ?PRI (REFINAMENT::pick-primer-inst-per-nivell ?nivell ?EPOCA ?ESTIL $?PROHS))
  (bind ?SEG (REFINAMENT::pick-segon-inst-per-nivell  ?nivell ?EPOCA ?ESTIL $?PROHS))
  (bind ?POS (REFINAMENT::pick-postre-inst-per-nivell ?nivell ?EPOCA ?ESTIL $?PROHS))

  (bind $?BG  (REFINAMENT::begudes-general$-per-nivell ?nivell))
  (bind $?BPR (create$)) (bind $?BSE (create$)) (bind $?BPO (create$))
  (if (eq ?pol Maridatge) then
    (bind ?x (REFINAMENT::beguda-maridatge-primer))  (if ?x then (bind $?BPR (create$ ?x)))
    (bind ?y (REFINAMENT::beguda-maridatge-segon))   (if ?y then (bind $?BSE (create$ ?y)))
    (bind ?z (REFINAMENT::beguda-maridatge-postre))  (if ?z then (bind $?BPO (create$ ?z))))

  (make-instance [MenuOpcio1] of Menu
    (entrantsMenu $?E) (primerMenu ?PRI) (segonMenu ?SEG) (postresMenu ?POS)
    (begudaGeneralMenu $?BG)
    (begudaPrimerMenu $?BPR) (begudaSegonMenu $?BSE) (begudaPostreMenu $?BPO))

  (assert (menu-opcio-preu (qui [MenuOpcio1]) (opcio O1) (percap ?target))))

(defrule REFINAMENT::construir-menu-per-opcio-O2
  (arquetip (politica-begudes ?pol))
  (epoca-inst (inst ?EPOCA))
  (estil-inst (inst ?ESTIL))
  (entrada (num_entrants ?NE)
           (ingredients_prohibits $?PROHS)
           (preferencia_complexitat ?CPX))
  (preu-objectiu (opcio O2) (percap ?target))
  ?pool <- (_pool-entrants (tots $?ALL))
  (not (object (name [MenuOpcio2])))
  =>
  (bind ?nivell (REFINAMENT::nivell-per-opcio O2))
  (bind $?E (REFINAMENT::take$ ?NE $?ALL))
  (modify ?pool (tots (REFINAMENT::drop$ ?NE $?ALL)))

  (bind ?PRI (REFINAMENT::pick-primer-inst-per-nivell ?nivell ?EPOCA ?ESTIL $?PROHS))
  (bind ?SEG (REFINAMENT::pick-segon-inst-per-nivell  ?nivell ?EPOCA ?ESTIL $?PROHS))
  (bind ?POS (REFINAMENT::pick-postre-inst-per-nivell ?nivell ?EPOCA ?ESTIL $?PROHS))

  (bind $?BG  (REFINAMENT::begudes-general$-per-nivell ?nivell))
  (bind $?BPR (create$)) (bind $?BSE (create$)) (bind $?BPO (create$))
  (if (eq ?pol Maridatge) then
    (bind ?x (REFINAMENT::beguda-maridatge-primer))  (if ?x then (bind $?BPR (create$ ?x)))
    (bind ?y (REFINAMENT::beguda-maridatge-segon))   (if ?y then (bind $?BSE (create$ ?y)))
    (bind ?z (REFINAMENT::beguda-maridatge-postre))  (if ?z then (bind $?BPO (create$ ?z))))

  (make-instance [MenuOpcio2] of Menu
    (entrantsMenu $?E) (primerMenu ?PRI) (segonMenu ?SEG) (postresMenu ?POS)
    (begudaGeneralMenu $?BG)
    (begudaPrimerMenu $?BPR) (begudaSegonMenu $?BSE) (begudaPostreMenu $?BPO))

  (assert (menu-opcio-preu (qui [MenuOpcio2]) (opcio O2) (percap ?target))))

(defrule REFINAMENT::construir-menu-per-opcio-O3
  (arquetip (politica-begudes ?pol))
  (epoca-inst (inst ?EPOCA))
  (estil-inst (inst ?ESTIL))
  (entrada (num_entrants ?NE)
           (ingredients_prohibits $?PROHS)
           (preferencia_complexitat ?CPX))
  (preu-objectiu (opcio O3) (percap ?target))
  ?pool <- (_pool-entrants (tots $?ALL))
  (not (object (name [MenuOpcio3])))
  =>
  (bind ?nivell (REFINAMENT::nivell-per-opcio O3))
  (bind $?E (REFINAMENT::take$ ?NE $?ALL))
  (modify ?pool (tots (REFINAMENT::drop$ ?NE $?ALL)))

  (bind ?PRI (REFINAMENT::pick-primer-inst-per-nivell ?nivell ?EPOCA ?ESTIL $?PROHS))
  (bind ?SEG (REFINAMENT::pick-segon-inst-per-nivell  ?nivell ?EPOCA ?ESTIL $?PROHS))
  (bind ?POS (REFINAMENT::pick-postre-inst-per-nivell ?nivell ?EPOCA ?ESTIL $?PROHS))

  (bind $?BG  (REFINAMENT::begudes-general$-per-nivell ?nivell))
  (bind $?BPR (create$)) (bind $?BSE (create$)) (bind $?BPO (create$))
  (if (eq ?pol Maridatge) then
    (bind ?x (REFINAMENT::beguda-maridatge-primer))  (if ?x then (bind $?BPR (create$ ?x)))
    (bind ?y (REFINAMENT::beguda-maridatge-segon))   (if ?y then (bind $?BSE (create$ ?y)))
    (bind ?z (REFINAMENT::beguda-maridatge-postre))  (if ?z then (bind $?BPO (create$ ?z))))

  (make-instance [MenuOpcio3] of Menu
    (entrantsMenu $?E) (primerMenu ?PRI) (segonMenu ?SEG) (postresMenu ?POS)
    (begudaGeneralMenu $?BG)
    (begudaPrimerMenu $?BPR) (begudaSegonMenu $?BSE) (begudaPostreMenu $?BPO))

  (assert (menu-opcio-preu (qui [MenuOpcio3]) (opcio O3) (percap ?target))))

(defrule REFINAMENT::marcar-menus-creats
  (object (name [MenuOpcio1]) (is-a Menu))
  (object (name [MenuOpcio2]) (is-a Menu))
  (object (name [MenuOpcio3]) (is-a Menu))
  (not (menus-creats))
  => (assert (menus-creats)))


;; Menú infantil (Tipus-public = Nens + ajust pressupost)
(deffunction REFINAMENT::public-nens? (?plat)
  (bind ?a (REFINAMENT::to-addr ?plat))
  (if (not ?a) then (return FALSE))

  (bind ?tp (lowcase (str-cat (send ?a get-tipus_public))))
  (bind ?diet (lowcase (str-cat (send ?a get-dieta))))

  (return (or (eq ?tp "nens")
              (str-index "infantil" ?diet))))

(deffunction REFINAMENT::pool-entrants$-infantil (?ESTIL $?PROHS)

  (bind $?cands (find-all-instances ((?x ONTO::Plat))
                  (and (REFINAMENT::es-tipus? ?x [PlatEntrants])
                       (REFINAMENT::public-nens? ?x)
                       (not (REFINAMENT::prohibit? (send ?x get-ingredient-principal) $?PROHS)))))

  (if (= (length$ $?cands) 0) then (return (create$)))

  (bind $?mix (create$))
  (while (> (length$ $?cands) 0) do
    (bind ?i (+ 1 (random 0 (- (length$ $?cands) 1))))
    (bind $?mix (create$ $?mix (nth$ ?i $?cands)))
    (bind $?cands (delete$ $?cands ?i ?i)))
  (return $?mix))


(deffunction REFINAMENT::pool-per-tipus$-nens (?TIPUS ?ESTIL $?PROHS)

  (return (find-all-instances ((?x ONTO::Plat))
            (and (REFINAMENT::es-tipus? ?x ?TIPUS)
                 (REFINAMENT::public-nens? ?x)
                 (not (REFINAMENT::prohibit? (send ?x get-ingredient-principal) $?PROHS))))))

(deffunction REFINAMENT::sort-by-preu$ (?ordre $?xs)
  (bind $?out (create$))
  (bind $?work $?xs)
  (while (> (length$ $?work) 0) do
    (bind ?best (nth$ 1 $?work))
    (foreach ?y $?work
      (if (and (eq ?ordre asc) (< (REFINAMENT::preu-of ?y) (REFINAMENT::preu-of ?best))) then (bind ?best ?y))
      (if (and (eq ?ordre desc) (> (REFINAMENT::preu-of ?y) (REFINAMENT::preu-of ?best))) then (bind ?best ?y)))
    (bind $?out  (create$ $?out ?best))
    (bind $?tmp (create$)) (bind ?removed FALSE)
    (foreach ?z $?work
      (if (and (not ?removed) (eq (REFINAMENT::name-of ?z) (REFINAMENT::name-of ?best)))
          then (bind ?removed TRUE)
          else (bind $?tmp (create$ $?tmp ?z))))
    (bind $?work $?tmp))
  (return $?out))

(deffunction REFINAMENT::pick-una-per-tipus-nens (?TIPUS ?nivell ?EPOCA ?ESTIL $?PROHS)

  (bind $?cands (find-all-instances ((?x ONTO::Plat))
                  (and (REFINAMENT::es-tipus? ?x ?TIPUS)
                       (REFINAMENT::public-nens? ?x)
                       (not (REFINAMENT::prohibit? (send ?x get-ingredient-principal) $?PROHS)))))

  (if (= (length$ $?cands) 0) then
    (bind $?cands (find-all-instances ((?x ONTO::Plat)) (REFINAMENT::public-nens? ?x))))
  (if (= (length$ $?cands) 0) then (return FALSE))

  (bind ?bestS -1)
  (bind $?best (create$))
  (foreach ?c $?cands
    (bind ?s (REFINAMENT::complexitat-score ?c ?nivell))
    (if (> ?s ?bestS) then
      (bind ?bestS ?s)
      (bind $?best (create$ ?c)))
    (if (= ?s ?bestS) then
      (bind $?best (create$ $?best ?c))))

  (bind ?i (+ 1 (random 0 (- (length$ $?best) 1))))
  (return (nth$ ?i $?best)))

(deffunction REFINAMENT::pick-cheapest-nens (?TIPUS ?ESTIL $?PROHS)
  "Retorna el plat infantil més econòmic del tipus indicat."
  (bind $?xs (REFINAMENT::pool-per-tipus$-nens ?TIPUS ?ESTIL $?PROHS))
  (if (= (length$ $?xs) 0) then (return FALSE))
  (return (nth$ 1 (REFINAMENT::sort-by-preu$ asc $?xs))))

(deffunction REFINAMENT::pick-priciest-nens (?TIPUS ?ESTIL $?PROHS)
  "Retorna el plat infantil més car del tipus indicat."
  (bind $?xs (REFINAMENT::pool-per-tipus$-nens ?TIPUS ?ESTIL $?PROHS))
  (if (= (length$ $?xs) 0) then (return FALSE))
  (return (nth$ 1 (REFINAMENT::sort-by-preu$ desc $?xs))))

;; Construcció + ajust a llindars infantils
(deffunction REFINAMENT::beguda-general$-infantil ()
  (bind $?b (find-all-instances ((?b ONTO::Beguda)) TRUE))
  (if (> (length$ $?b) 0) then (return (REFINAMENT::take$ 2 $?b)) else (return (create$))))

(defrule REFINAMENT::construir-menu-infantil-O1
  (abstracte (necessita-menu-infantil si))
  (estil-inst (inst ?ESTIL))
  (epoca-inst (inst ?EPOCA))
  (entrada (num_entrants ?NE)
           (ingredients_prohibits $?PROHS)
           (preferencia_complexitat ?CPX))
  (preu-objectiu-infantil (opcio O1) (percap ?TARGETI))
  (not (object (name [MenuInfantilO1])))
  =>
  (bind ?nivell (REFINAMENT::nivell-per-opcio O1))
  (bind $?POOL (REFINAMENT::pool-entrants$-infantil ?ESTIL $?PROHS))
  (bind $?E (REFINAMENT::take$ ?NE $?POOL))

  (bind ?PRI (REFINAMENT::pick-una-per-tipus-nens [PlatPrimer] ?nivell ?EPOCA ?ESTIL $?PROHS))
  (bind ?SEG (REFINAMENT::pick-una-per-tipus-nens [PlatSegon]  ?nivell ?EPOCA ?ESTIL $?PROHS))
  (bind ?POS (REFINAMENT::pick-una-per-tipus-nens [PlatPostre] ?nivell ?EPOCA ?ESTIL $?PROHS))

  (bind $?BG (REFINAMENT::beguda-general$-infantil))

  (make-instance [MenuInfantilO1] of Menu
    (entrantsMenu $?E) (primerMenu ?PRI)
    (segonMenu ?SEG) (postresMenu ?POS)
    (begudaGeneralMenu $?BG))

  (assert (menu-opcio-preu (qui [MenuInfantilO1]) (opcio O1) (percap ?TARGETI))))

(defrule REFINAMENT::construir-menu-infantil-O2
  (abstracte (necessita-menu-infantil si))
  (estil-inst (inst ?ESTIL))
  (epoca-inst (inst ?EPOCA))
  (entrada (num_entrants ?NE)
           (ingredients_prohibits $?PROHS)
           (preferencia_complexitat ?CPX))
  (preu-objectiu-infantil (opcio O2) (percap ?TARGETI))
  (not (object (name [MenuInfantilO2])))
  =>
  (bind ?nivell (REFINAMENT::nivell-per-opcio O2))
  (bind $?POOL (REFINAMENT::pool-entrants$-infantil ?ESTIL $?PROHS))
  (bind $?E (REFINAMENT::take$ ?NE (REFINAMENT::drop$ 1 $?POOL)))

  (bind ?PRI (REFINAMENT::pick-una-per-tipus-nens [PlatPrimer] ?nivell ?EPOCA ?ESTIL $?PROHS))
  (bind ?SEG (REFINAMENT::pick-una-per-tipus-nens [PlatSegon]  ?nivell ?EPOCA ?ESTIL $?PROHS))
  (bind ?POS (REFINAMENT::pick-una-per-tipus-nens [PlatPostre] ?nivell ?EPOCA ?ESTIL $?PROHS))

  (bind $?BG (REFINAMENT::beguda-general$-infantil))

  (make-instance [MenuInfantilO2] of Menu
    (entrantsMenu $?E) (primerMenu ?PRI)
    (segonMenu ?SEG) (postresMenu ?POS)
    (begudaGeneralMenu $?BG))

  (assert (menu-opcio-preu (qui [MenuInfantilO2]) (opcio O2) (percap ?TARGETI))))

(defrule REFINAMENT::construir-menu-infantil-O3
  (abstracte (necessita-menu-infantil si))
  (estil-inst (inst ?ESTIL))
  (epoca-inst (inst ?EPOCA))
  (entrada (num_entrants ?NE)
           (ingredients_prohibits $?PROHS)
           (preferencia_complexitat ?CPX))
  (preu-objectiu-infantil (opcio O3) (percap ?TARGETI))
  (not (object (name [MenuInfantilO3])))
  =>
  (bind ?nivell (REFINAMENT::nivell-per-opcio O3))
  (bind $?POOL (REFINAMENT::pool-entrants$-infantil ?ESTIL $?PROHS))
  (bind $?E (REFINAMENT::take$ ?NE (REFINAMENT::drop$ 2 $?POOL)))

  (bind ?PRI (REFINAMENT::pick-una-per-tipus-nens [PlatPrimer] ?nivell ?EPOCA ?ESTIL $?PROHS))
  (bind ?SEG (REFINAMENT::pick-una-per-tipus-nens [PlatSegon]  ?nivell ?EPOCA ?ESTIL $?PROHS))
  (bind ?POS (REFINAMENT::pick-una-per-tipus-nens [PlatPostre] ?nivell ?EPOCA ?ESTIL $?PROHS))

  (bind $?BG (REFINAMENT::beguda-general$-infantil))

  (make-instance [MenuInfantilO3] of Menu
    (entrantsMenu $?E) (primerMenu ?PRI)
    (segonMenu ?SEG) (postresMenu ?POS)
    (begudaGeneralMenu $?BG))

  (assert (menu-opcio-preu (qui [MenuInfantilO3]) (opcio O3) (percap ?TARGETI))))

(defrule REFINAMENT::menus-infantils-creats
  (object (name [MenuInfantilO1]) (is-a Menu))
  (object (name [MenuInfantilO2]) (is-a Menu))
  (object (name [MenuInfantilO3]) (is-a Menu))
  (not (menus-infantils-creats))
  =>
  (assert (menus-infantils-creats)))

;;; MENÚ CELÍAC
(defrule REFINAMENT::construir-menu-celiac
  (estil-inst (inst ?ESTIL))
  (epoca-inst (inst ?EPOCA))
  (entrada (num_entrants ?NE) (ingredients_prohibits $?PROHS) (num_celiacs ?nCEL))
  (preu-objectiu (opcio O2) (percap ?target))
  (test (> ?nCEL 0))
  (not (menu-creat celiac))
  =>
  (bind ?nivell Mitjana)
  (bind $?allE (find-all-instances ((?x ONTO::Plat))
                  (and (REFINAMENT::es-tipus? ?x [PlatEntrants])
                       (REFINAMENT::plat-apte? ?x celiac))))
  (if (> ?NE 0)
      then (bind $?E (REFINAMENT::take$ ?NE $?allE))
      else (bind $?E (create$)))

  (bind ?P  (REFINAMENT::pick-una-per-tipus-restriccio [PlatPrimer] ?nivell ?EPOCA ?ESTIL celiac $?PROHS))
  (bind ?S  (REFINAMENT::pick-una-per-tipus-restriccio [PlatSegon]  ?nivell ?EPOCA ?ESTIL celiac $?PROHS))
  (bind ?PO (REFINAMENT::pick-una-per-tipus-restriccio [PlatPostre] ?nivell ?EPOCA ?ESTIL celiac $?PROHS))

  (if (not ?P)  then (bind ?P  (REFINAMENT::pick-una-per-tipus [PlatPrimer] ?nivell ?EPOCA ?ESTIL $?PROHS)))
  (if (not ?S)  then (bind ?S  (REFINAMENT::pick-una-per-tipus [PlatSegon]  ?nivell ?EPOCA ?ESTIL $?PROHS)))
  (if (not ?PO) then (bind ?PO (REFINAMENT::pick-una-per-tipus [PlatPostre] ?nivell ?EPOCA ?ESTIL $?PROHS)))

  (bind $?BG (REFINAMENT::begudes-general$-per-nivell ?nivell))

  (make-instance [MenuCeliac] of Menu
    (entrantsMenu $?E) (primerMenu ?P) (segonMenu ?S)
    (postresMenu ?PO) (begudaGeneralMenu $?BG))

  (assert (menu-opcio-preu (qui [MenuCeliac]) (opcio Celiac) (percap ?target)))
  (assert (menu-creat celiac)))

(defrule REFINAMENT::construir-menu-infantil-celiac
  (abstracte (necessita-menu-infantil si))
  (entrada (menu_infantil_celiac si)
           (num_entrants ?NE)
           (ingredients_prohibits $?PROHS))
  (estil-inst (inst ?ESTIL))
  (epoca-inst (inst ?EPOCA))
  (preu-objectiu-infantil (opcio O2) (percap ?target))
  (not (object (name [MenuInfantilCeliac])))
  =>
  (bind ?nivell Mitjana)
  (bind $?E (find-all-instances ((?x ONTO::Plat))
              (and (REFINAMENT::es-tipus? ?x [PlatEntrants])
                   (REFINAMENT::public-nens? ?x)
                   (REFINAMENT::plat-apte? ?x celiac)
                   (not (REFINAMENT::prohibit? (send ?x get-ingredient-principal) $?PROHS)))))

  (if (< (length$ $?E) ?NE) then
    (bind $?extra (find-all-instances ((?y ONTO::Plat))
                    (and (REFINAMENT::es-tipus? ?y [PlatEntrants])
                         (REFINAMENT::plat-apte? ?y celiac)
                         (not (REFINAMENT::prohibit? (send ?y get-ingredient-principal) $?PROHS)))))
    (bind $?E (create$ $?E (subseq$ $?extra 1 (min ?NE (length$ $?extra))))))

  (bind $?E (REFINAMENT::take$ ?NE $?E))

  (bind ?P (find-instance ((?x ONTO::Plat))
             (and (REFINAMENT::es-tipus? ?x [PlatPrimer])
                  (REFINAMENT::plat-apte? ?x celiac)
                  (REFINAMENT::public-nens? ?x)
                  (not (REFINAMENT::prohibit? (send ?x get-ingredient-principal) $?PROHS)))))
  (if (not ?P)
    then (bind ?P (REFINAMENT::pick-una-per-tipus-restriccio [PlatPrimer] ?nivell ?EPOCA ?ESTIL celiac $?PROHS)))

  (bind ?S (find-instance ((?x ONTO::Plat))
             (and (REFINAMENT::es-tipus? ?x [PlatSegon])
                  (REFINAMENT::plat-apte? ?x celiac)
                  (REFINAMENT::public-nens? ?x)
                  (not (REFINAMENT::prohibit? (send ?x get-ingredient-principal) $?PROHS)))))
  (if (not ?S)
    then (bind ?S (REFINAMENT::pick-una-per-tipus-restriccio [PlatSegon] ?nivell ?EPOCA ?ESTIL celiac $?PROHS)))

  (bind ?PO (find-instance ((?x ONTO::Plat))
              (and (REFINAMENT::es-tipus? ?x [PlatPostre])
                   (REFINAMENT::plat-apte? ?x celiac)
                   (REFINAMENT::public-nens? ?x)
                   (not (REFINAMENT::prohibit? (send ?x get-ingredient-principal) $?PROHS)))))
  (if (not ?PO)
    then (bind ?PO (REFINAMENT::pick-una-per-tipus-restriccio [PlatPostre] ?nivell ?EPOCA ?ESTIL celiac $?PROHS)))

  (bind $?BG (REFINAMENT::beguda-general$-infantil))

  (make-instance [MenuInfantilCeliac] of Menu
    (entrantsMenu $?E)
    (primerMenu ?P)
    (segonMenu ?S)
    (postresMenu ?PO)
    (begudaGeneralMenu $?BG))

  (assert (menu-opcio-preu (qui [MenuInfantilCeliac]) (opcio InfantilCeliac) (percap ?target))))

;;; MENÚ HALAL
(defrule REFINAMENT::construir-menu-halal
  (estil-inst (inst ?ESTIL))
  (epoca-inst (inst ?EPOCA))
  (entrada (num_entrants ?NE) (ingredients_prohibits $?PROHS) (num_halal ?nH))
  (preu-objectiu (opcio O2) (percap ?target))
  (test (> ?nH 0))
  (not (menu-creat halal))
  =>
  (bind ?nivell Mitjana)
  (bind $?allE (find-all-instances ((?x ONTO::Plat))
                  (and (REFINAMENT::es-tipus? ?x [PlatEntrants])
                       (REFINAMENT::plat-apte? ?x halal))))
  (if (> ?NE 0)
      then (bind $?E (REFINAMENT::take$ ?NE $?allE))
      else (bind $?E (create$)))

  (bind ?P  (REFINAMENT::pick-una-per-tipus-restriccio [PlatPrimer] ?nivell ?EPOCA ?ESTIL halal $?PROHS))
  (bind ?S  (REFINAMENT::pick-una-per-tipus-restriccio [PlatSegon]  ?nivell ?EPOCA ?ESTIL halal $?PROHS))
  (bind ?PO (REFINAMENT::pick-una-per-tipus-restriccio [PlatPostre] ?nivell ?EPOCA ?ESTIL halal $?PROHS))

  (if (not ?P)  then (bind ?P  (REFINAMENT::pick-una-per-tipus [PlatPrimer] ?nivell ?EPOCA ?ESTIL $?PROHS)))
  (if (not ?S)  then (bind ?S  (REFINAMENT::pick-una-per-tipus [PlatSegon]  ?nivell ?EPOCA ?ESTIL $?PROHS)))
  (if (not ?PO) then (bind ?PO (REFINAMENT::pick-una-per-tipus [PlatPostre] ?nivell ?EPOCA ?ESTIL $?PROHS)))

  (bind $?BG (REFINAMENT::begudes-general$-per-nivell ?nivell))

  (make-instance [MenuHalal] of Menu
    (entrantsMenu $?E) (primerMenu ?P) (segonMenu ?S)
    (postresMenu ?PO) (begudaGeneralMenu $?BG))

  (assert (menu-opcio-preu (qui [MenuHalal]) (opcio Halal) (percap ?target)))
  (assert (menu-creat halal)))

(defrule REFINAMENT::construir-menu-infantil-halal
  (abstracte (necessita-menu-infantil si))
  (entrada (menu_infantil_halal si)
           (num_entrants ?NE)
           (ingredients_prohibits $?PROHS))
  (estil-inst (inst ?ESTIL))
  (epoca-inst (inst ?EPOCA))
  (preu-objectiu-infantil (opcio O2) (percap ?target))
  (not (object (name [MenuInfantilHalal])))
  =>
  (bind ?nivell Mitjana)
  (bind $?E (find-all-instances ((?x ONTO::Plat))
              (and (REFINAMENT::es-tipus? ?x [PlatEntrants])
                   (REFINAMENT::public-nens? ?x)
                   (REFINAMENT::plat-apte? ?x halal)
                   (not (REFINAMENT::prohibit? (send ?x get-ingredient-principal) $?PROHS)))))

  (if (< (length$ $?E) ?NE) then
    (bind $?extra (find-all-instances ((?y ONTO::Plat))
                    (and (REFINAMENT::es-tipus? ?y [PlatEntrants])
                         (REFINAMENT::plat-apte? ?y halal)
                         (not (REFINAMENT::prohibit? (send ?y get-ingredient-principal) $?PROHS)))))
    (bind $?E (create$ $?E (subseq$ $?extra 1 (min ?NE (length$ $?extra))))))
  (bind $?E (REFINAMENT::take$ ?NE $?E))

  (bind ?P (find-instance ((?x ONTO::Plat))
             (and (REFINAMENT::es-tipus? ?x [PlatPrimer])
                  (REFINAMENT::plat-apte? ?x halal)
                  (REFINAMENT::public-nens? ?x)
                  (not (REFINAMENT::prohibit? (send ?x get-ingredient-principal) $?PROHS)))))
  (if (not ?P)
    then (bind ?P (REFINAMENT::pick-una-per-tipus-restriccio [PlatPrimer] ?nivell ?EPOCA ?ESTIL halal $?PROHS)))

  (bind ?S (find-instance ((?x ONTO::Plat))
             (and (REFINAMENT::es-tipus? ?x [PlatSegon])
                  (REFINAMENT::plat-apte? ?x halal)
                  (REFINAMENT::public-nens? ?x)
                  (not (REFINAMENT::prohibit? (send ?x get-ingredient-principal) $?PROHS)))))
  (if (not ?S)
    then (bind ?S (REFINAMENT::pick-una-per-tipus-restriccio [PlatSegon] ?nivell ?EPOCA ?ESTIL halal $?PROHS)))

  (bind ?PO (find-instance ((?x ONTO::Plat))
              (and (REFINAMENT::es-tipus? ?x [PlatPostre])
                   (REFINAMENT::plat-apte? ?x halal)
                   (REFINAMENT::public-nens? ?x)
                   (not (REFINAMENT::prohibit? (send ?x get-ingredient-principal) $?PROHS)))))
  (if (not ?PO)
    then (bind ?PO (REFINAMENT::pick-una-per-tipus-restriccio [PlatPostre] ?nivell ?EPOCA ?ESTIL halal $?PROHS)))

  (bind $?BG (REFINAMENT::beguda-general$-infantil))

  (make-instance [MenuInfantilHalal] of Menu
    (entrantsMenu $?E)
    (primerMenu ?P)
    (segonMenu ?S)
    (postresMenu ?PO)
    (begudaGeneralMenu $?BG))

  (assert (menu-opcio-preu (qui [MenuInfantilHalal]) (opcio InfantilHalal) (percap ?target))))

;;; MENÚ SENSE LACTOSA
(defrule REFINAMENT::construir-menu-lactosa
  (estil-inst (inst ?ESTIL))
  (epoca-inst (inst ?EPOCA))
  (entrada (num_entrants ?NE) (ingredients_prohibits $?PROHS) (num_lactosa ?nL))
  (preu-objectiu (opcio O2) (percap ?target))
  (test (> ?nL 0))
  (not (menu-creat lactosa))
  =>
  (bind ?nivell Mitjana)
  (bind $?allE (find-all-instances ((?x ONTO::Plat))
                  (and (REFINAMENT::es-tipus? ?x [PlatEntrants])
                       (REFINAMENT::plat-apte? ?x lactosa))))
  (if (> ?NE 0)
      then (bind $?E (REFINAMENT::take$ ?NE $?allE))
      else (bind $?E (create$)))

  (bind ?P  (REFINAMENT::pick-una-per-tipus-restriccio [PlatPrimer] ?nivell ?EPOCA ?ESTIL lactosa $?PROHS))
  (bind ?S  (REFINAMENT::pick-una-per-tipus-restriccio [PlatSegon]  ?nivell ?EPOCA ?ESTIL lactosa $?PROHS))
  (bind ?PO (REFINAMENT::pick-una-per-tipus-restriccio [PlatPostre] ?nivell ?EPOCA ?ESTIL lactosa $?PROHS))

  (if (not ?P)  then (bind ?P  (REFINAMENT::pick-una-per-tipus [PlatPrimer] ?nivell ?EPOCA ?ESTIL $?PROHS)))
  (if (not ?S)  then (bind ?S  (REFINAMENT::pick-una-per-tipus [PlatSegon]  ?nivell ?EPOCA ?ESTIL $?PROHS)))
  (if (not ?PO) then (bind ?PO (REFINAMENT::pick-una-per-tipus [PlatPostre] ?nivell ?EPOCA ?ESTIL $?PROHS)))

  (bind $?BG (REFINAMENT::begudes-general$-per-nivell ?nivell))

  (make-instance [MenuLactosa] of Menu
    (entrantsMenu $?E)
    (primerMenu ?P)
    (segonMenu ?S)
    (postresMenu ?PO)
    (begudaGeneralMenu $?BG))

  (assert (menu-opcio-preu (qui [MenuLactosa]) (opcio Lactosa) (percap ?target)))
  (assert (menu-creat lactosa)))

(defrule REFINAMENT::construir-menu-infantil-lactosa
  (abstracte (necessita-menu-infantil si))
  (entrada (menu_infantil_lactosa si)
           (num_entrants ?NE)
           (ingredients_prohibits $?PROHS))
  (estil-inst (inst ?ESTIL))
  (epoca-inst (inst ?EPOCA))
  (preu-objectiu-infantil (opcio O2) (percap ?target))
  (not (object (name [MenuInfantilLactosa])))
  =>
  (bind ?nivell Mitjana)
  (bind $?E (find-all-instances ((?x ONTO::Plat))
              (and (REFINAMENT::es-tipus? ?x [PlatEntrants])
                   (REFINAMENT::public-nens? ?x)
                   (REFINAMENT::plat-apte? ?x lactosa)
                   (not (REFINAMENT::prohibit? (send ?x get-ingredient-principal) $?PROHS)))))

  (if (< (length$ $?E) ?NE) then
    (bind $?extra (find-all-instances ((?y ONTO::Plat))
                    (and (REFINAMENT::es-tipus? ?y [PlatEntrants])
                         (REFINAMENT::plat-apte? ?y lactosa)
                         (not (REFINAMENT::prohibit? (send ?y get-ingredient-principal) $?PROHS)))))
    (bind $?E (create$ $?E (subseq$ $?extra 1 (min ?NE (length$ $?extra))))))
  (bind $?E (REFINAMENT::take$ ?NE $?E))

  (bind ?P (find-instance ((?x ONTO::Plat))
             (and (REFINAMENT::es-tipus? ?x [PlatPrimer])
                  (REFINAMENT::plat-apte? ?x lactosa)
                  (REFINAMENT::public-nens? ?x)
                  (not (REFINAMENT::prohibit? (send ?x get-ingredient-principal) $?PROHS)))))
  (if (not ?P)
    then (bind ?P (REFINAMENT::pick-una-per-tipus-restriccio [PlatPrimer] ?nivell ?EPOCA ?ESTIL lactosa $?PROHS)))

  (bind ?S (find-instance ((?x ONTO::Plat))
             (and (REFINAMENT::es-tipus? ?x [PlatSegon])
                  (REFINAMENT::plat-apte? ?x lactosa)
                  (REFINAMENT::public-nens? ?x)
                  (not (REFINAMENT::prohibit? (send ?x get-ingredient-principal) $?PROHS)))))
  (if (not ?S)
    then (bind ?S (REFINAMENT::pick-una-per-tipus-restriccio [PlatSegon] ?nivell ?EPOCA ?ESTIL lactosa $?PROHS)))

  (bind ?PO (find-instance ((?x ONTO::Plat))
              (and (REFINAMENT::es-tipus? ?x [PlatPostre])
                   (REFINAMENT::plat-apte? ?x lactosa)
                   (REFINAMENT::public-nens? ?x)
                   (not (REFINAMENT::prohibit? (send ?x get-ingredient-principal) $?PROHS)))))
  (if (not ?PO)
    then (bind ?PO (REFINAMENT::pick-una-per-tipus-restriccio [PlatPostre] ?nivell ?EPOCA ?ESTIL lactosa $?PROHS)))

  (bind $?BG (REFINAMENT::beguda-general$-infantil))

  (make-instance [MenuInfantilLactosa] of Menu
    (entrantsMenu $?E)
    (primerMenu ?P)
    (segonMenu ?S)
    (postresMenu ?PO)
    (begudaGeneralMenu $?BG))

  (assert (menu-opcio-preu (qui [MenuInfantilLactosa]) (opcio InfantilLactosa) (percap ?target))))

;;; MENÚ VEGETARIÀ
(defrule REFINAMENT::construir-menu-vegetaria
  (estil-inst (inst ?ESTIL))
  (epoca-inst (inst ?EPOCA))
  (entrada (num_entrants ?NE) (ingredients_prohibits $?PROHS) (num_vegetarians ?nV))
  (preu-objectiu (opcio O2) (percap ?target))
  (test (> ?nV 0))
  (not (menu-creat vegetaria))
  =>
  (bind ?nivell Mitjana)
  (bind $?allE (find-all-instances ((?x ONTO::Plat))
                  (and (REFINAMENT::es-tipus? ?x [PlatEntrants])
                       (REFINAMENT::plat-apte? ?x vegetaria))))
  (if (> ?NE 0)
      then (bind $?E (REFINAMENT::take$ ?NE $?allE))
      else (bind $?E (create$)))

  (bind ?P  (REFINAMENT::pick-una-per-tipus-restriccio [PlatPrimer] ?nivell ?EPOCA ?ESTIL vegetaria $?PROHS))
  (bind ?S  (REFINAMENT::pick-una-per-tipus-restriccio [PlatSegon]  ?nivell ?EPOCA ?ESTIL vegetaria $?PROHS))
  (bind ?PO (REFINAMENT::pick-una-per-tipus-restriccio [PlatPostre] ?nivell ?EPOCA ?ESTIL vegetaria $?PROHS))

  (if (not ?P)  then (bind ?P  (REFINAMENT::pick-una-per-tipus [PlatPrimer] ?nivell ?EPOCA ?ESTIL $?PROHS)))
  (if (not ?S)  then (bind ?S  (REFINAMENT::pick-una-per-tipus [PlatSegon]  ?nivell ?EPOCA ?ESTIL $?PROHS)))
  (if (not ?PO) then (bind ?PO (REFINAMENT::pick-una-per-tipus [PlatPostre] ?nivell ?EPOCA ?ESTIL $?PROHS)))

  (bind $?BG (REFINAMENT::begudes-general$-per-nivell ?nivell))

  (make-instance [MenuVegetaria] of Menu
    (entrantsMenu $?E)
    (primerMenu ?P)
    (segonMenu ?S)
    (postresMenu ?PO)
    (begudaGeneralMenu $?BG))

  (assert (menu-opcio-preu (qui [MenuVegetaria]) (opcio Vegetaria) (percap ?target)))
  (assert (menu-creat vegetaria)))

(defrule REFINAMENT::construir-menu-infantil-vegetaria
  (abstracte (necessita-menu-infantil si))
  (entrada (menu_infantil_vegetaria si)
           (num_entrants ?NE)
           (ingredients_prohibits $?PROHS))
  (estil-inst (inst ?ESTIL))
  (epoca-inst (inst ?EPOCA))
  (preu-objectiu-infantil (opcio O2) (percap ?target))
  (not (object (name [MenuInfantilVegetaria])))
  =>
  (bind ?nivell Mitjana)
  (bind $?E (find-all-instances ((?x ONTO::Plat))
              (and (REFINAMENT::es-tipus? ?x [PlatEntrants])
                   (REFINAMENT::public-nens? ?x)
                   (REFINAMENT::plat-apte? ?x vegetaria)
                   (not (REFINAMENT::prohibit? (send ?x get-ingredient-principal) $?PROHS)))))

  (if (< (length$ $?E) ?NE) then
    (bind $?extra (find-all-instances ((?y ONTO::Plat))
                    (and (REFINAMENT::es-tipus? ?y [PlatEntrants])
                         (REFINAMENT::plat-apte? ?y vegetaria)
                         (not (REFINAMENT::prohibit? (send ?y get-ingredient-principal) $?PROHS)))))
    (bind $?E (create$ $?E (subseq$ $?extra 1 (min ?NE (length$ $?extra))))))
  (bind $?E (REFINAMENT::take$ ?NE $?E))

  (bind ?P (find-instance ((?x ONTO::Plat))
             (and (REFINAMENT::es-tipus? ?x [PlatPrimer])
                  (REFINAMENT::plat-apte? ?x vegetaria)
                  (REFINAMENT::public-nens? ?x)
                  (not (REFINAMENT::prohibit? (send ?x get-ingredient-principal) $?PROHS)))))
  (if (not ?P)
    then (bind ?P (REFINAMENT::pick-una-per-tipus-restriccio [PlatPrimer] ?nivell ?EPOCA ?ESTIL vegetaria $?PROHS)))

  (bind ?S (find-instance ((?x ONTO::Plat))
             (and (REFINAMENT::es-tipus? ?x [PlatSegon])
                  (REFINAMENT::plat-apte? ?x vegetaria)
                  (REFINAMENT::public-nens? ?x)
                  (not (REFINAMENT::prohibit? (send ?x get-ingredient-principal) $?PROHS)))))
  (if (not ?S)
    then (bind ?S (REFINAMENT::pick-una-per-tipus-restriccio [PlatSegon] ?nivell ?EPOCA ?ESTIL vegetaria $?PROHS)))

  (bind ?PO (find-instance ((?x ONTO::Plat))
              (and (REFINAMENT::es-tipus? ?x [PlatPostre])
                   (REFINAMENT::plat-apte? ?x vegetaria)
                   (REFINAMENT::public-nens? ?x)
                   (not (REFINAMENT::prohibit? (send ?x get-ingredient-principal) $?PROHS)))))
  (if (not ?PO)
    then (bind ?PO (REFINAMENT::pick-una-per-tipus-restriccio [PlatPostre] ?nivell ?EPOCA ?ESTIL vegetaria $?PROHS)))

  (bind $?BG (REFINAMENT::beguda-general$-infantil))

  (make-instance [MenuInfantilVegetaria] of Menu
    (entrantsMenu $?E)
    (primerMenu ?P)
    (segonMenu ?S)
    (postresMenu ?PO)
    (begudaGeneralMenu $?BG))

  (assert (menu-opcio-preu (qui [MenuInfantilVegetaria]) (opcio InfantilVegetaria) (percap ?target))))

;;; MENÚ VEGÀ
(defrule REFINAMENT::construir-menu-vega
  (estil-inst (inst ?ESTIL))
  (epoca-inst (inst ?EPOCA))
  (entrada (num_entrants ?NE) (ingredients_prohibits $?PROHS) (num_vegans ?nVG))
  (preu-objectiu (opcio O2) (percap ?target))
  (test (> ?nVG 0))
  (not (menu-creat vega))
  =>
  (bind ?nivell Mitjana)
  (bind $?allE (find-all-instances ((?x ONTO::Plat))
                  (and (REFINAMENT::es-tipus? ?x [PlatEntrants])
                       (REFINAMENT::plat-apte? ?x vega))))
  (if (> ?NE 0)
      then (bind $?E (REFINAMENT::take$ ?NE $?allE))
      else (bind $?E (create$)))

  (bind ?P  (REFINAMENT::pick-una-per-tipus-restriccio [PlatPrimer] ?nivell ?EPOCA ?ESTIL vega $?PROHS))
  (bind ?S  (REFINAMENT::pick-una-per-tipus-restriccio [PlatSegon]  ?nivell ?EPOCA ?ESTIL vega $?PROHS))
  (bind ?PO (REFINAMENT::pick-una-per-tipus-restriccio [PlatPostre] ?nivell ?EPOCA ?ESTIL vega $?PROHS))

  (if (not ?P)  then (bind ?P  (REFINAMENT::pick-una-per-tipus [PlatPrimer] ?nivell ?EPOCA ?ESTIL $?PROHS)))
  (if (not ?S)  then (bind ?S  (REFINAMENT::pick-una-per-tipus [PlatSegon]  ?nivell ?EPOCA ?ESTIL $?PROHS)))
  (if (not ?PO) then (bind ?PO (REFINAMENT::pick-una-per-tipus [PlatPostre] ?nivell ?EPOCA ?ESTIL $?PROHS)))

  (bind $?BG (REFINAMENT::begudes-general$-per-nivell ?nivell))

  (make-instance [MenuVega] of Menu
    (entrantsMenu $?E)
    (primerMenu ?P)
    (segonMenu ?S)
    (postresMenu ?PO)
    (begudaGeneralMenu $?BG))

  (assert (menu-opcio-preu (qui [MenuVega]) (opcio Vega) (percap ?target)))
  (assert (menu-creat vega)))

(defrule REFINAMENT::construir-menu-infantil-vega
  (abstracte (necessita-menu-infantil si))
  (entrada (menu_infantil_vega si)
           (num_entrants ?NE)
           (ingredients_prohibits $?PROHS))
  (estil-inst (inst ?ESTIL))
  (epoca-inst (inst ?EPOCA))
  (preu-objectiu-infantil (opcio O2) (percap ?target))
  (not (object (name [MenuInfantilVega])))
  =>
  (bind ?nivell Mitjana)
  (bind $?E (find-all-instances ((?x ONTO::Plat))
              (and (REFINAMENT::es-tipus? ?x [PlatEntrants])
                   (REFINAMENT::public-nens? ?x)
                   (REFINAMENT::plat-apte? ?x vega)
                   (not (REFINAMENT::prohibit? (send ?x get-ingredient-principal) $?PROHS)))))

  (if (< (length$ $?E) ?NE) then
    (bind $?extra (find-all-instances ((?y ONTO::Plat))
                    (and (REFINAMENT::es-tipus? ?y [PlatEntrants])
                         (REFINAMENT::plat-apte? ?y vega)
                         (not (REFINAMENT::prohibit? (send ?y get-ingredient-principal) $?PROHS)))))
    (bind $?E (create$ $?E (subseq$ $?extra 1 (min ?NE (length$ $?extra))))))
  (bind $?E (REFINAMENT::take$ ?NE $?E))

  (bind ?P (find-instance ((?x ONTO::Plat))
             (and (REFINAMENT::es-tipus? ?x [PlatPrimer])
                  (REFINAMENT::plat-apte? ?x vega)
                  (REFINAMENT::public-nens? ?x)
                  (not (REFINAMENT::prohibit? (send ?x get-ingredient-principal) $?PROHS)))))
  (if (not ?P)
    then (bind ?P (REFINAMENT::pick-una-per-tipus-restriccio [PlatPrimer] ?nivell ?EPOCA ?ESTIL vega $?PROHS)))

  (bind ?S (find-instance ((?x ONTO::Plat))
             (and (REFINAMENT::es-tipus? ?x [PlatSegon])
                  (REFINAMENT::plat-apte? ?x vega)
                  (REFINAMENT::public-nens? ?x)
                  (not (REFINAMENT::prohibit? (send ?x get-ingredient-principal) $?PROHS)))))
  (if (not ?S)
    then (bind ?S (REFINAMENT::pick-una-per-tipus-restriccio [PlatSegon] ?nivell ?EPOCA ?ESTIL vega $?PROHS)))

  (bind ?PO (find-instance ((?x ONTO::Plat))
              (and (REFINAMENT::es-tipus? ?x [PlatPostre])
                   (REFINAMENT::plat-apte? ?x vega)
                   (REFINAMENT::public-nens? ?x)
                   (not (REFINAMENT::prohibit? (send ?x get-ingredient-principal) $?PROHS)))))
  (if (not ?PO)
    then (bind ?PO (REFINAMENT::pick-una-per-tipus-restriccio [PlatPostre] ?nivell ?EPOCA ?ESTIL vega $?PROHS)))

  (bind $?BG (REFINAMENT::beguda-general$-infantil))

  (make-instance [MenuInfantilVega] of Menu
    (entrantsMenu $?E)
    (primerMenu ?P)
    (segonMenu ?S)
    (postresMenu ?PO)
    (begudaGeneralMenu $?BG))

  (assert (menu-opcio-preu (qui [MenuInfantilVega]) (opcio InfantilVega) (percap ?target))))

;;; MENÚ SENSE FRUITS SECS
(defrule REFINAMENT::construir-menu-fruitsecs
  (estil-inst (inst ?ESTIL))
  (epoca-inst (inst ?EPOCA))
  (entrada (num_entrants ?NE) (ingredients_prohibits $?PROHS) (num_fruitsecs ?nF))
  (preu-objectiu (opcio O2) (percap ?target))
  (test (> ?nF 0))
  (not (menu-creat fruitsecs))
  =>
  (bind ?nivell Mitjana)
  (bind $?allE (find-all-instances ((?x ONTO::Plat))
                  (and (REFINAMENT::es-tipus? ?x [PlatEntrants])
                       (REFINAMENT::plat-apte? ?x fruitsecs))))
  (if (> ?NE 0)
      then (bind $?E (REFINAMENT::take$ ?NE $?allE))
      else (bind $?E (create$)))

  (bind ?P  (REFINAMENT::pick-una-per-tipus-restriccio [PlatPrimer] ?nivell ?EPOCA ?ESTIL fruitsecs $?PROHS))
  (bind ?S  (REFINAMENT::pick-una-per-tipus-restriccio [PlatSegon]  ?nivell ?EPOCA ?ESTIL fruitsecs $?PROHS))
  (bind ?PO (REFINAMENT::pick-una-per-tipus-restriccio [PlatPostre] ?nivell ?EPOCA ?ESTIL fruitsecs $?PROHS))

  (if (not ?P)  then (bind ?P  (REFINAMENT::pick-una-per-tipus [PlatPrimer] ?nivell ?EPOCA ?ESTIL $?PROHS)))
  (if (not ?S)  then (bind ?S  (REFINAMENT::pick-una-per-tipus [PlatSegon]  ?nivell ?EPOCA ?ESTIL $?PROHS)))
  (if (not ?PO) then (bind ?PO (REFINAMENT::pick-una-per-tipus [PlatPostre] ?nivell ?EPOCA ?ESTIL $?PROHS)))

  (bind $?BG (REFINAMENT::begudes-general$-per-nivell ?nivell))

  (make-instance [MenuFruitsecs] of Menu
    (entrantsMenu $?E)
    (primerMenu ?P)
    (segonMenu ?S)
    (postresMenu ?PO)
    (begudaGeneralMenu $?BG))

  (assert (menu-opcio-preu (qui [MenuFruitsecs]) (opcio Fruitsecs) (percap ?target)))
  (assert (menu-creat fruitsecs)))

(defrule REFINAMENT::construir-menu-infantil-fruitsecs
  (abstracte (necessita-menu-infantil si))
  (entrada (menu_infantil_fruits_secs si)
           (num_entrants ?NE)
           (ingredients_prohibits $?PROHS))
  (estil-inst (inst ?ESTIL))
  (epoca-inst (inst ?EPOCA))
  (preu-objectiu-infantil (opcio O2) (percap ?target))
  (not (object (name [MenuInfantilFruitsecs])))
  =>
  (bind ?nivell Mitjana)
  (bind $?E (find-all-instances ((?x ONTO::Plat))
              (and (REFINAMENT::es-tipus? ?x [PlatEntrants])
                   (REFINAMENT::public-nens? ?x)
                   (REFINAMENT::plat-apte? ?x fruitsecs)
                   (not (REFINAMENT::prohibit? (send ?x get-ingredient-principal) $?PROHS)))))

  (if (< (length$ $?E) ?NE) then
    (bind $?extra (find-all-instances ((?y ONTO::Plat))
                    (and (REFINAMENT::es-tipus? ?y [PlatEntrants])
                         (REFINAMENT::plat-apte? ?y fruitsecs)
                         (not (REFINAMENT::prohibit? (send ?y get-ingredient-principal) $?PROHS)))))
    (bind $?E (create$ $?E (subseq$ $?extra 1 (min ?NE (length$ $?extra))))))
  (bind $?E (REFINAMENT::take$ ?NE $?E))

  (bind ?P (find-instance ((?x ONTO::Plat))
             (and (REFINAMENT::es-tipus? ?x [PlatPrimer])
                  (REFINAMENT::plat-apte? ?x fruitsecs)
                  (REFINAMENT::public-nens? ?x)
                  (not (REFINAMENT::prohibit? (send ?x get-ingredient-principal) $?PROHS)))))
  (if (not ?P)
    then (bind ?P (REFINAMENT::pick-una-per-tipus-restriccio [PlatPrimer] ?nivell ?EPOCA ?ESTIL fruitsecs $?PROHS)))

  (bind ?S (find-instance ((?x ONTO::Plat))
             (and (REFINAMENT::es-tipus? ?x [PlatSegon])
                  (REFINAMENT::plat-apte? ?x fruitsecs)
                  (REFINAMENT::public-nens? ?x)
                  (not (REFINAMENT::prohibit? (send ?x get-ingredient-principal) $?PROHS)))))
  (if (not ?S)
    then (bind ?S (REFINAMENT::pick-una-per-tipus-restriccio [PlatSegon] ?nivell ?EPOCA ?ESTIL fruitsecs $?PROHS)))

  (bind ?PO (find-instance ((?x ONTO::Plat))
              (and (REFINAMENT::es-tipus? ?x [PlatPostre])
                   (REFINAMENT::plat-apte? ?x fruitsecs)
                   (REFINAMENT::public-nens? ?x)
                   (not (REFINAMENT::prohibit? (send ?x get-ingredient-principal) $?PROHS)))))
  (if (not ?PO)
    then (bind ?PO (REFINAMENT::pick-una-per-tipus-restriccio [PlatPostre] ?nivell ?EPOCA ?ESTIL fruitsecs $?PROHS)))

  (bind $?BG (REFINAMENT::beguda-general$-infantil))

  (make-instance [MenuInfantilFruitsecs] of Menu
    (entrantsMenu $?E)
    (primerMenu ?P)
    (segonMenu ?S)
    (postresMenu ?PO)
    (begudaGeneralMenu $?BG))

  (assert (menu-opcio-preu (qui [MenuInfantilFruitsecs]) (opcio InfantilFruitsecs) (percap ?target))))

;; Impressió + justificació
(deffunction REFINAMENT::print-list+preu (?label $?xs)
  (printout t ?label crlf)
  (foreach ?x $?xs
    (printout t "  • " (REFINAMENT::name-of ?x)
                "  (" (format nil "%.2f" (REFINAMENT::preu-of ?x)) "€)" crlf)))

(deftemplate REFINAMENT::preu-real
  (slot opcio (allowed-symbols O1 O2 O3))
  (slot percap (type FLOAT)))

(deffunction REFINAMENT::eq-string (?a ?b)
  (bind ?A (lowcase (str-cat ?a)))
  (bind ?B (lowcase (str-cat ?b)))
  (return (eq ?A ?B)))

(deffunction REFINAMENT::print-origen-si-regional (?ESTIL ?orig ?indent)
   "Imprimeix l'origen si l'usuari ha triat estil regional."
   (bind ?estilUsuariNom (REFINAMENT::nom-estil-net ?ESTIL))
   (if (eq ?estilUsuariNom "regional") then
      (bind ?nomOrig (REFINAMENT::nom-origen-net ?orig))
      (bind ?origUsuari "-")
      (do-for-fact ((?eentrada entrada)) TRUE
         (if (neq (lowcase (str-cat ?eentrada:origen_regional)) "-")
            then (bind ?origUsuari (lowcase (str-cat ?eentrada:origen_regional)))))
      (bind ?nomOrig (lowcase ?nomOrig))
      (bind ?origUsuari (lowcase ?origUsuari))
      (printout t ?indent "Origen: " ?nomOrig " → "
         (if (REFINAMENT::eq-string ?nomOrig ?origUsuari)
             then "exacte" else "alternatiu") crlf)))

(defrule REFINAMENT::imprimir-menu1
  (menus-creats)
  (not (impressio-feta (opcio O1)))
  (menu-opcio-preu (qui [MenuOpcio1]) (percap ?pp))
  (object (name [MenuOpcio1]) (is-a Menu)
          (entrantsMenu $?E)
          (primerMenu ?P)
          (segonMenu ?S)
          (postresMenu ?PO)
          (begudaGeneralMenu $?BG)
          (begudaPrimerMenu $?BPR)
          (begudaSegonMenu $?BSE)
          (begudaPostreMenu $?BPO))
  (epoca-inst (inst ?EPOCA))
  (estil-inst (inst ?ESTIL))
  =>
  (printout t crlf
    "----------------------- OPCIÓ DE MENÚ 1 -----------------------" crlf)

  ;; ENTRANTS
  (printout t "Entrants:" crlf)
  (foreach ?e $?E
    (printout t "  • " (REFINAMENT::name-of ?e)
                " (" (format nil "%.2f" (REFINAMENT::preu-of ?e)) "€)" crlf)
    (bind ?temp (send ?e get-temporada))
    (bind ?estil (send ?e get-estil_plat))
    (bind ?orig (send ?e get-origen))
    (bind ?epocaNom (REFINAMENT::nom-epoca-net ?temp))
    (bind ?estilNom (REFINAMENT::nom-estil-net ?estil))
    (bind ?epocaUsuari (REFINAMENT::nom-epoca-net ?EPOCA))
    (bind ?estilUsuari (REFINAMENT::nom-estil-net ?ESTIL))

    (printout t "     Època: " ?epocaNom " → "
              (if (REFINAMENT::eq-string ?epocaNom ?epocaUsuari)
                  then "exacta" else "alternativa") crlf)
    (printout t "     Estil: " ?estilNom " → "
              (if (REFINAMENT::eq-string ?estilNom ?estilUsuari)
                  then "exacte" else "alternatiu") crlf)
    (REFINAMENT::print-origen-si-regional ?ESTIL ?orig "     ")
  )

  ;; PRIMER
  (printout t "Primer  : " (REFINAMENT::name-of ?P)
              " (" (format nil "%.2f" (REFINAMENT::preu-of ?P)) "€)" crlf)
  (bind ?tempP (send ?P get-temporada))
  (bind ?estilP (send ?P get-estil_plat))
  (bind ?origP (send ?P get-origen))
  (bind ?epocaNomP (REFINAMENT::nom-epoca-net ?tempP))
  (bind ?estilNomP (REFINAMENT::nom-estil-net ?estilP))
  (bind ?epocaUsuari (REFINAMENT::nom-epoca-net ?EPOCA))
  (bind ?estilUsuari (REFINAMENT::nom-estil-net ?ESTIL))
  (printout t "          Època: " ?epocaNomP " → "
    (if (REFINAMENT::eq-string ?epocaNomP ?epocaUsuari)
        then "exacta" else "alternativa") crlf)
  (printout t "          Estil: " ?estilNomP " → "
    (if (REFINAMENT::eq-string ?estilNomP ?estilUsuari)
        then "exacte" else "alternatiu") crlf)
  (REFINAMENT::print-origen-si-regional ?ESTIL ?origP "          ")

  ;; SEGON 
  (printout t "Segon   : " (REFINAMENT::name-of ?S)
              " (" (format nil "%.2f" (REFINAMENT::preu-of ?S)) "€)" crlf)
  (bind ?tempS (send ?S get-temporada))
  (bind ?estilS (send ?S get-estil_plat))
  (bind ?origS (send ?S get-origen))
  (bind ?epocaNomS (REFINAMENT::nom-epoca-net ?tempS))
  (bind ?estilNomS (REFINAMENT::nom-estil-net ?estilS))
  (printout t "          Època: " ?epocaNomS " → "
    (if (REFINAMENT::eq-string ?epocaNomS ?epocaUsuari)
        then "exacta" else "alternativa") crlf)
  (printout t "          Estil: " ?estilNomS " → "
    (if (REFINAMENT::eq-string ?estilNomS ?estilUsuari)
        then "exacte" else "alternatiu") crlf)
  (REFINAMENT::print-origen-si-regional ?ESTIL ?origS "          ")

  ;; POSTRE
  (printout t "Postre  : " (REFINAMENT::name-of ?PO)
              " (" (format nil "%.2f" (REFINAMENT::preu-of ?PO)) "€)" crlf)
  (bind ?tempPO (send ?PO get-temporada))
  (bind ?estilPO (send ?PO get-estil_plat))
  (bind ?origPO (send ?PO get-origen))
  (bind ?epocaNomPO (REFINAMENT::nom-epoca-net ?tempPO))
  (bind ?estilNomPO (REFINAMENT::nom-estil-net ?estilPO))
  (printout t "          Època: " ?epocaNomPO " → "
    (if (REFINAMENT::eq-string ?epocaNomPO ?epocaUsuari)
        then "exacta" else "alternativa") crlf)
  (printout t "          Estil: " ?estilNomPO " → "
    (if (REFINAMENT::eq-string ?estilNomPO ?estilUsuari)
        then "exacte" else "alternatiu") crlf)
  (REFINAMENT::print-origen-si-regional ?ESTIL ?origPO "          ")

  ;; COST
  (bind ?cost
    (REFINAMENT::preu-menu-percap-des-de-binds
      ?P ?S ?PO $?E $?BG $?BPR $?BSE $?BPO))
  (printout t "----------------------------------------------------------------" crlf)
  (assert (preu-real (opcio O1) (percap ?cost)))
  (assert (impressio-feta (opcio O1)))
)

(defrule REFINAMENT::imprimir-menu2
  (menus-creats)
  (not (impressio-feta (opcio O2)))
  (menu-opcio-preu (qui [MenuOpcio2]) (percap ?pp))
  (object (name [MenuOpcio2]) (is-a Menu)
          (entrantsMenu $?E) (primerMenu ?P) (segonMenu ?S) (postresMenu ?PO)
          (begudaGeneralMenu $?BG) (begudaPrimerMenu $?BPR)
          (begudaSegonMenu $?BSE) (begudaPostreMenu $?BPO))
  (epoca-inst (inst ?EPOCA))
  (estil-inst (inst ?ESTIL))
  =>
  (printout t crlf "----------------------- OPCIÓ DE MENÚ 2 -----------------------" crlf)

  ;; ENTRANTS
  (printout t "Entrants:" crlf)
  (foreach ?e $?E
    (printout t "  • " (REFINAMENT::name-of ?e)
                " (" (format nil "%.2f" (REFINAMENT::preu-of ?e)) "€)" crlf)
    (bind ?temp (send ?e get-temporada))
    (bind ?estil (send ?e get-estil_plat))
    (bind ?orig (send ?e get-origen))
    (bind ?epocaNom (REFINAMENT::nom-epoca-net ?temp))
    (bind ?estilNom (REFINAMENT::nom-estil-net ?estil))
    (bind ?epocaUsuari (REFINAMENT::nom-epoca-net ?EPOCA))
    (bind ?estilUsuari (REFINAMENT::nom-estil-net ?ESTIL))
    (printout t "     Època: " ?epocaNom " → "
              (if (REFINAMENT::eq-string ?epocaNom ?epocaUsuari)
                  then "exacta" else "alternativa") crlf)
    (printout t "     Estil: " ?estilNom " → "
              (if (REFINAMENT::eq-string ?estilNom ?estilUsuari)
                  then "exacte" else "alternatiu") crlf)
    (REFINAMENT::print-origen-si-regional ?ESTIL ?orig "     ")
  )

  ;; PRIMER
  (printout t "Primer  : " (REFINAMENT::name-of ?P)
              " (" (format nil "%.2f" (REFINAMENT::preu-of ?P)) "€)" crlf)
  (bind ?tempP (send ?P get-temporada))
  (bind ?estilP (send ?P get-estil_plat))
  (bind ?origP (send ?P get-origen))
  (bind ?epocaNomP (REFINAMENT::nom-epoca-net ?tempP))
  (bind ?estilNomP (REFINAMENT::nom-estil-net ?estilP))
  (bind ?epocaUsuari (REFINAMENT::nom-epoca-net ?EPOCA))
  (bind ?estilUsuari (REFINAMENT::nom-estil-net ?ESTIL))
  (printout t "          Època: " ?epocaNomP " → "
            (if (REFINAMENT::eq-string ?epocaNomP ?epocaUsuari)
                then "exacta" else "alternativa") crlf)
  (printout t "          Estil: " ?estilNomP " → "
            (if (REFINAMENT::eq-string ?estilNomP ?estilUsuari)
                then "exacte" else "alternatiu") crlf)
  (REFINAMENT::print-origen-si-regional ?ESTIL ?origP "          ")

  ;; SEGON
  (printout t "Segon   : " (REFINAMENT::name-of ?S)
              " (" (format nil "%.2f" (REFINAMENT::preu-of ?S)) "€)" crlf)
  (bind ?tempS (send ?S get-temporada))
  (bind ?estilS (send ?S get-estil_plat))
  (bind ?origS (send ?S get-origen))
  (bind ?epocaNomS (REFINAMENT::nom-epoca-net ?tempS))
  (bind ?estilNomS (REFINAMENT::nom-estil-net ?estilS))
  (printout t "          Època: " ?epocaNomS " → "
            (if (REFINAMENT::eq-string ?epocaNomS ?epocaUsuari)
                then "exacta" else "alternativa") crlf)
  (printout t "          Estil: " ?estilNomS " → "
            (if (REFINAMENT::eq-string ?estilNomS ?estilUsuari)
                then "exacte" else "alternatiu") crlf)
  (REFINAMENT::print-origen-si-regional ?ESTIL ?origS "          ")

  ;; POSTRE
  (printout t "Postre  : " (REFINAMENT::name-of ?PO)
              " (" (format nil "%.2f" (REFINAMENT::preu-of ?PO)) "€)" crlf)
  (bind ?tempPO (send ?PO get-temporada))
  (bind ?estilPO (send ?PO get-estil_plat))
  (bind ?origPO (send ?PO get-origen))
  (bind ?epocaNomPO (REFINAMENT::nom-epoca-net ?tempPO))
  (bind ?estilNomPO (REFINAMENT::nom-estil-net ?estilPO))
  (printout t "          Època: " ?epocaNomPO " → "
            (if (REFINAMENT::eq-string ?epocaNomPO ?epocaUsuari)
                then "exacta" else "alternativa") crlf)
  (printout t "          Estil: " ?estilNomPO " → "
            (if (REFINAMENT::eq-string ?estilNomPO ?estilUsuari)
                then "exacte" else "alternatiu") crlf)
  (REFINAMENT::print-origen-si-regional ?ESTIL ?origPO "          ")

  ;; COST
  (bind ?cost (REFINAMENT::preu-menu-percap-des-de-binds ?P ?S ?PO $?E $?BG $?BPR $?BSE $?BPO))
  (printout t "----------------------------------------------------------------" crlf)
  (assert (preu-real (opcio O2) (percap ?cost)))
  (assert (impressio-feta (opcio O2))))

(defrule REFINAMENT::imprimir-menu3
  (menus-creats)
  (not (impressio-feta (opcio O3)))
  (menu-opcio-preu (qui [MenuOpcio3]) (percap ?pp))
  (object (name [MenuOpcio3]) (is-a Menu)
          (entrantsMenu $?E) (primerMenu ?P) (segonMenu ?S) (postresMenu ?PO)
          (begudaGeneralMenu $?BG) (begudaPrimerMenu $?BPR)
          (begudaSegonMenu $?BSE) (begudaPostreMenu $?BPO))
  (epoca-inst (inst ?EPOCA))
  (estil-inst (inst ?ESTIL))
  =>
  (printout t crlf "----------------------- OPCIÓ DE MENÚ 3 -----------------------" crlf)

  ;; ENTRANTS
  (printout t "Entrants:" crlf)
  (foreach ?e $?E
    (printout t "  • " (REFINAMENT::name-of ?e)
                " (" (format nil "%.2f" (REFINAMENT::preu-of ?e)) "€)" crlf)
    (bind ?temp (send ?e get-temporada))
    (bind ?estil (send ?e get-estil_plat))
    (bind ?orig (send ?e get-origen))
    (bind ?epocaNom (REFINAMENT::nom-epoca-net ?temp))
    (bind ?estilNom (REFINAMENT::nom-estil-net ?estil))
    (bind ?epocaUsuari (REFINAMENT::nom-epoca-net ?EPOCA))
    (bind ?estilUsuari (REFINAMENT::nom-estil-net ?ESTIL))
    (printout t "     Època: " ?epocaNom " → "
              (if (REFINAMENT::eq-string ?epocaNom ?epocaUsuari)
                  then "exacta" else "alternativa") crlf)
    (printout t "     Estil: " ?estilNom " → "
              (if (REFINAMENT::eq-string ?estilNom ?estilUsuari)
                  then "exacte" else "alternatiu") crlf)
    (REFINAMENT::print-origen-si-regional ?ESTIL ?orig "     ")
  )

  ;; PRIMER
  (printout t "Primer  : " (REFINAMENT::name-of ?P)
              " (" (format nil "%.2f" (REFINAMENT::preu-of ?P)) "€)" crlf)
  (bind ?tempP (send ?P get-temporada))
  (bind ?estilP (send ?P get-estil_plat))
  (bind ?origP (send ?P get-origen))
  (bind ?epocaNomP (REFINAMENT::nom-epoca-net ?tempP))
  (bind ?estilNomP (REFINAMENT::nom-estil-net ?estilP))
  (bind ?epocaUsuari (REFINAMENT::nom-epoca-net ?EPOCA))
  (bind ?estilUsuari (REFINAMENT::nom-estil-net ?ESTIL))
  (printout t "          Època: " ?epocaNomP " → "
            (if (REFINAMENT::eq-string ?epocaNomP ?epocaUsuari)
                then "exacta" else "alternativa") crlf)
  (printout t "          Estil: " ?estilNomP " → "
            (if (REFINAMENT::eq-string ?estilNomP ?estilUsuari)
                then "exacte" else "alternatiu") crlf)
  (REFINAMENT::print-origen-si-regional ?ESTIL ?origP "          ")

  ;; SEGON
  (printout t "Segon   : " (REFINAMENT::name-of ?S)
              " (" (format nil "%.2f" (REFINAMENT::preu-of ?S)) "€)" crlf)
  (bind ?tempS (send ?S get-temporada))
  (bind ?estilS (send ?S get-estil_plat))
  (bind ?origS (send ?S get-origen))
  (bind ?epocaNomS (REFINAMENT::nom-epoca-net ?tempS))
  (bind ?estilNomS (REFINAMENT::nom-estil-net ?estilS))
  (printout t "          Època: " ?epocaNomS " → "
            (if (REFINAMENT::eq-string ?epocaNomS ?epocaUsuari)
                then "exacta" else "alternativa") crlf)
  (printout t "          Estil: " ?estilNomS " → "
            (if (REFINAMENT::eq-string ?estilNomS ?estilUsuari)
                then "exacte" else "alternatiu") crlf)
  (REFINAMENT::print-origen-si-regional ?ESTIL ?origS "          ")

  ;; POSTRE
  (printout t "Postre  : " (REFINAMENT::name-of ?PO)
              " (" (format nil "%.2f" (REFINAMENT::preu-of ?PO)) "€)" crlf)
  (bind ?tempPO (send ?PO get-temporada))
  (bind ?estilPO (send ?PO get-estil_plat))
  (bind ?origPO (send ?PO get-origen))
  (bind ?epocaNomPO (REFINAMENT::nom-epoca-net ?tempPO))
  (bind ?estilNomPO (REFINAMENT::nom-estil-net ?estilPO))
  (printout t "          Època: " ?epocaNomPO " → "
            (if (REFINAMENT::eq-string ?epocaNomPO ?epocaUsuari)
                then "exacta" else "alternativa") crlf)
  (printout t "          Estil: " ?estilNomPO " → "
            (if (REFINAMENT::eq-string ?estilNomPO ?estilUsuari)
                then "exacte" else "alternatiu") crlf)
  (REFINAMENT::print-origen-si-regional ?ESTIL ?origPO "          ")

  ;; COST
  (bind ?cost (REFINAMENT::preu-menu-percap-des-de-binds ?P ?S ?PO $?E $?BG $?BPR $?BSE $?BPO))
  (printout t "----------------------------------------------------------------" crlf)
  (assert (preu-real (opcio O3) (percap ?cost)))
  (assert (impressio-feta (opcio O3))))

(deffunction REFINAMENT::by-preu-asc (?a ?b)
  (return (< ?a ?b)))

(deffunction REFINAMENT::ordena-preus-infantil ()
  (bind ?p1 0.0)
  (bind ?p2 0.0)
  (bind ?p3 0.0)

  (do-for-fact ((?f preu-real-infantil)) TRUE
    (if (eq ?f:opcio InfantilO1) then (bind ?p1 ?f:percap))
    (if (eq ?f:opcio InfantilO2) then (bind ?p2 ?f:percap))
    (if (eq ?f:opcio InfantilO3) then (bind ?p3 ?f:percap)))

  (bind $?opcions (create$ InfantilO1 InfantilO2 InfantilO3))
  (bind $?preus   (create$ ?p1 ?p2 ?p3))

  (bind ?n (length$ $?preus))
  (bind ?i 1)
  (while (< ?i ?n) do
    (bind ?j (+ ?i 1))
    (while (<= ?j ?n) do
      (if (> (nth$ ?i $?preus) (nth$ ?j $?preus)) then
        (bind ?tmpP (nth$ ?i $?preus))
        (bind ?tmpO (nth$ ?i $?opcions))
        (bind $?preus (replace$ $?preus ?i ?i (nth$ ?j $?preus)))
        (bind $?preus (replace$ $?preus ?j ?j ?tmpP))
        (bind $?opcions (replace$ $?opcions ?i ?i (nth$ ?j $?opcions)))
        (bind $?opcions (replace$ $?opcions ?j ?j ?tmpO)))
      (bind ?j (+ ?j 1)))
    (bind ?i (+ ?i 1)))
  (return (create$ $?opcions $?preus)))

(defrule REFINAMENT::resum-menus-ordenats
  (preu-real (opcio O1) (percap ?p1))
  (preu-real (opcio O2) (percap ?p2))
  (preu-real (opcio O3) (percap ?p3))
  (impressio-feta (opcio O1))
  (impressio-feta (opcio O2))
  (impressio-feta (opcio O3))
  =>
  (printout t crlf "--------------- ORDRE FINAL SEGONS COST ESTIMAT ---------------" crlf)

  (bind $?opcions (create$ O1 O2 O3))
  (bind $?preus (create$ (+ ?p1 ?*cost-begudes-generals*)
                         (+ ?p2 ?*cost-begudes-generals*)
                         (+ ?p3 ?*cost-begudes-generals*)))

  (bind ?n (length$ $?preus))
  (bind ?i 1)
  (while (< ?i ?n) do
    (bind ?j (+ ?i 1))
    (while (<= ?j ?n) do
      (if (> (nth$ ?i $?preus) (nth$ ?j $?preus)) then
        (bind ?tmpP (nth$ ?i $?preus))
        (bind ?tmpO (nth$ ?i $?opcions))
        (bind $?preus (replace$ $?preus ?i ?i (nth$ ?j $?preus)))
        (bind $?preus (replace$ $?preus ?j ?j ?tmpP))
        (bind $?opcions (replace$ $?opcions ?i ?i (nth$ ?j $?opcions)))
        (bind $?opcions (replace$ $?opcions ?j ?j ?tmpO))
      )
      (bind ?j (+ ?j 1))
    )
    (bind ?i (+ ?i 1))
  )

  (bind ?k 1)
  (while (<= ?k ?n) do
    (printout t (nth$ ?k $?opcions) ": "
               (format nil "%.2f" (nth$ ?k $?preus)) " €/pax" crlf)
    (bind ?k (+ ?k 1))
  )
  (printout t "----------------------------------------------------------------" crlf)
)

(defrule REFINAMENT::imprimir-begudes-generals-inicials
  "Imprimeix les begudes generals incloses en tots els menús adults, abans de la recomanació de vins."
  (declare (salience 50))
  (menus-creats)
  (impressio-feta (opcio O1))
  (impressio-feta (opcio O2))
  (impressio-feta (opcio O3))
  (not (begudes-generals-impreses))
  =>
  (printout t crlf "----------------------- BEGUDES GENERALS -----------------------" crlf)
  (printout t "Aquestes són les begudes disponibles per a tots els comensals," crlf)
  (printout t "independentment del menú escollit:" crlf)
  (printout t "  • Aigua mineral (amb i sense gas)" crlf)
  (printout t "  • Refrescos (cola, llimona, taronja)" crlf)
  (printout t "  • Cervesa (amb i sense alcohol)" crlf)
  (printout t "  • Sucs de fruita variats" crlf crlf)
  (printout t "Cost fix per persona: "
               (format nil "%.2f" ?*cost-begudes-generals*) " €" crlf)
  (printout t "----------------------------------------------------------------" crlf)
  (assert (begudes-generals-impreses)))


;; Recomanació de Vins segons la complexitat (només adults)
(deftemplate REFINAMENT::cost-vins
  (slot percap (type NUMBER))
)

(deffunction REFINAMENT::mostrar-recomanacio-vins (?cx)
  (printout t crlf "----------------------- RECOMANACIÓ DE VINS -----------------------" crlf)
  (printout t "Complexitat seleccionada: " ?cx crlf)

  (if (eq ?cx Baixa) then
    (printout t "Per a un menú de complexitat baixa, recomanem vins suaus i frescos:" crlf
               "  • Vi blanc jove (DO Catalunya) ................. 10.00€" crlf
               "  • Vi negre jove (DO Terra Alta) ................ 12.00€" crlf
               "  • Vi dolç o moscatell per als postres .......... 8.50€" crlf)
    (return (create$ "Vi blanc jove" 10.00 "Vi negre jove" 12.00 "Vi dolç" 8.50))
  )

  (if (eq ?cx Mitjana) then
    (printout t "Per a un menú de complexitat mitjana, recomanem vins equilibrats i aromàtics:" crlf
               "  • Vi blanc afruitat (DO Penedès) ............... 15.00€" crlf
               "  • Vi negre criança (DO Rioja) .................. 18.00€" crlf
               "  • Cava brut nature per als postres ............. 14.00€" crlf)
    (return (create$ "Vi blanc afruitat" 15.00 "Vi negre criança" 18.00 "Cava brut nature" 14.00))
  )

  (if (eq ?cx Alta) then
    (printout t "Per a un menú de complexitat alta, recomanem vins més sofisticats i amb cos:" crlf
               "  • Vi blanc reserva (DO Rueda) .................. 20.00€" crlf
               "  • Vi negre reserva (DO Priorat) ................ 24.00€" crlf
               "  • Vi ranci o cava gran reserva per al final .... 22.00€" crlf)
    (return (create$ "Vi blanc reserva" 20.00 "Vi negre reserva" 24.00 "Cava gran reserva" 22.00))
  )
)

(defrule REFINAMENT::afegir-recomanacio-vins
  (entrada (preferencia_beguda Vol-vi)
           (preferencia_complexitat ?cx)
           (num_adults ?a))
  (preu-real (opcio O1) (percap ?p1))
  (preu-real (opcio O2) (percap ?p2))
  (preu-real (opcio O3) (percap ?p3))
  (impressio-feta (opcio O1))
  (impressio-feta (opcio O2))
  (impressio-feta (opcio O3))
  (not (impressio-feta (opcio BegudaRecomanada)))
  =>
  (bind $?vins (REFINAMENT::mostrar-recomanacio-vins ?cx))
  (bind ?preu-total (+ (nth$ 2 $?vins) (nth$ 4 $?vins) (nth$ 6 $?vins)))

  (bind ?ampolles (/ ?a 4.0))
  (bind ?cost-per-cap (/ ?preu-total ?ampolles))

  (printout t crlf "*** Recomanació segons la complexitat (" ?cx "):" crlf)
  (printout t "Cost total estimat dels vins (3 ampolles): " (format nil "%.2f" ?preu-total) "€" crlf)
  (printout t "Adults: " ?a ", estimat 1 ampolla per cada 4 → " (format nil "%.2f" ?ampolles) " ampolles" crlf)
  (printout t "Cost per persona (només adults): " (format nil "%.2f" ?cost-per-cap) "€" crlf)

  (bind ?p1_total (+ ?p1 ?cost-per-cap))
  (bind ?p2_total (+ ?p2 ?cost-per-cap))
  (bind ?p3_total (+ ?p3 ?cost-per-cap))

  (assert (cost-vins (percap ?cost-per-cap)))
  (assert (impressio-feta (opcio BegudaRecomanada))))

;; Recomanació de Maridatge (Vol-maridatge)
(deffunction REFINAMENT::mostrar-maridatge ()
  (printout t crlf "----------------------- MARIDATGE PERSONALITZAT -----------------------" crlf)
  (printout t "Com has seleccionat l'opció 'Vol-maridatge', et proposem un conjunt" crlf)
  (printout t "de vins i begudes per combinar amb els diferents tipus de plats." crlf crlf)

  (printout t "Entrants:" crlf)
  (printout t "  • Còctel suau de fruites o vermut blanc ............... 8.00€" crlf)
  (printout t "  • Cava brut rosat per començar amb frescor ............ 12.00€" crlf crlf)

  (printout t "Plats de carn:" crlf)
  (printout t "  • Vi negre reserva (DO Priorat) ....................... 23.00€" crlf)
  (printout t "  • Vi negre criança (DO Rioja) .......................... 26.00€" crlf crlf)

  (printout t "Plats de peix:" crlf)
  (printout t "  • Vi blanc afruitat (DO Penedès) ....................... 18.00€" crlf)
  (printout t "  • Vi blanc sec (DO Rías Baixas) ........................ 22.00€" crlf crlf)

  (printout t "Plats d’estofat o guisat:" crlf)
  (printout t "  • Vi negre jove (DO Terra Alta) ........................ 14.00€" crlf crlf)

  (printout t "Postres:" crlf)
  (printout t "  • Vi dolç moscatell o mistela ......................... 12.00€" crlf)
  (printout t "  • Cava semisec per finalitzar amb elegància ........... 15.00€" crlf crlf)

  (printout t "------------------------------------------------------------------" crlf)
  (printout t "Cost total estimat del maridatge complet: 150.00€" crlf)
  (printout t "Cost total estimat del maridatge per persona: 15.00€" crlf)
  (printout t "(Aquests conjunt de begudes cada 10 persones)" crlf)
  (printout t "------------------------------------------------------------------" crlf)
  (printout t "Aquests vins poden ajustar-se segons la temporada i la complexitat" crlf)
  (printout t "del menú triat. El maridatge està pensat per ressaltar els sabors" crlf)
  (printout t "i oferir una experiència gastronòmica completa." crlf)
  (printout t "------------------------------------------------------------------" crlf)
)

(defrule REFINAMENT::mostrar-recomanacio-maridatge
  "Aplica el cost fix de maridatge (15 €/pax) sense imprimir càlculs"
  (entrada (preferencia_beguda Vol-maridatge))
  (preu-real (opcio O1) (percap ?p1))
  (preu-real (opcio O2) (percap ?p2))
  (preu-real (opcio O3) (percap ?p3))
  (impressio-feta (opcio O1))
  (impressio-feta (opcio O2))
  (impressio-feta (opcio O3))
  (not (impressio-feta (opcio BegudaRecomanada)))
  =>
  (REFINAMENT::mostrar-maridatge)

  ;; Cost fix del maridatge
  (bind ?cost-per-cap 15.00)

  (bind ?p1_total (+ ?p1 ?cost-per-cap))
  (bind ?p2_total (+ ?p2 ?cost-per-cap))
  (bind ?p3_total (+ ?p3 ?cost-per-cap))

  (assert (cost-vins (percap ?cost-per-cap)))
  (assert (impressio-feta (opcio BegudaRecomanada)))
)

;; Pastissos de Boda
(deffunction REFINAMENT::mostrar-pastissos-boda ()
  (printout t crlf "--------------------- OPCIONS DE PASTÍS DE BODA ---------------------" crlf)
  (printout t "1. Pastís de boda de tres pisos amb fondant ................. 485.63€" crlf)
  (printout t "2. Pastís de boda amb nata vegetal i fruites vermelles ...... 241.50€" crlf)
  (printout t "3. Mini pastís de boda amb cobertura de xocolata negra ...... 126.00€" crlf)
  (printout t "---------------------------------------------------------------------" crlf))

(defrule REFINAMENT::afegir-pastis-boda
  (entrada (tipus Boda)
           (preferencia_complexitat ?cx)
           (comensals ?n))
  (cost-vins (percap ?cv))  
  (preu-real (opcio O1) (percap ?p1))
  (preu-real (opcio O2) (percap ?p2))
  (preu-real (opcio O3) (percap ?p3))
  (impressio-feta (opcio BegudaRecomanada))
  (not (impressio-feta (opcio PastisBoda)))
  =>
  (REFINAMENT::mostrar-pastissos-boda)
  (bind ?pastis "")
  (bind ?preu 0.0)

  (if (eq ?cx Baixa) then
    (bind ?pastis "Mini pastís de boda amb cobertura de xocolata negra")
    (bind ?preu 126.00))
  (if (eq ?cx Mitjana) then
    (bind ?pastis "Pastís de boda amb nata vegetal i fruites vermelles")
    (bind ?preu 241.50))
  (if (eq ?cx Alta) then
    (bind ?pastis "Pastís de boda de tres pisos amb fondant")
    (bind ?preu 485.63))

  (bind ?costPerCap (/ ?preu ?n))

  (printout t crlf "*** Recomanació per a la teva boda segons la complexitat (" ?cx "):" crlf)
  (printout t "→ " ?pastis " — " (format nil "%.2f" ?preu) "€ (total)" crlf)
  (printout t "   Cost addicional per persona: " (format nil "%.2f" ?costPerCap) "€" crlf)

  (bind ?p1_total (+ ?p1 ?cv ?costPerCap))
  (bind ?p2_total (+ ?p2 ?cv ?costPerCap))
  (bind ?p3_total (+ ?p3 ?cv ?costPerCap))

  (assert (impressio-feta (opcio PastisBoda)))
)

;; Infantils
(defrule REFINAMENT::imprimir-menu-infantil-O1
  (menus-infantils-creats)
  (not (impressio-feta (opcio InfantilO1)))
  (menu-opcio-preu (qui [MenuInfantilO1]) (percap ?pp))
  (object (name [MenuInfantilO1]) (is-a Menu)
          (entrantsMenu $?E) (primerMenu ?P)
          (segonMenu ?S) (postresMenu ?PO)
          (begudaGeneralMenu $?BG))
  =>
  (printout t crlf "----------------------- MENÚ INFANTIL 1 -----------------------" crlf)
  (REFINAMENT::print-list+preu "Entrants:" $?E)
  (printout t "Primer  : " (REFINAMENT::name-of ?P)
               " (" (format nil "%.2f" (REFINAMENT::preu-of ?P)) "€)" crlf)
  (printout t "Segon   : " (REFINAMENT::name-of ?S)
               " (" (format nil "%.2f" (REFINAMENT::preu-of ?S)) "€)" crlf)
  (printout t "Postre  : " (REFINAMENT::name-of ?PO)
               " (" (format nil "%.2f" (REFINAMENT::preu-of ?PO)) "€)" crlf)
  (bind ?cost (REFINAMENT::preu-menu-percap-des-de-binds ?P ?S ?PO $?E $?BG (create$) (create$) (create$)))
  (printout t "----------------------------------------------------------------" crlf)
  (assert (preu-real-infantil (opcio InfantilO1) (percap ?cost)))
  (assert (impressio-feta (opcio InfantilO1))))

(defrule REFINAMENT::imprimir-menu-infantil-O2
  (menus-infantils-creats)
  (not (impressio-feta (opcio InfantilO2)))
  (menu-opcio-preu (qui [MenuInfantilO2]) (percap ?pp))
  (object (name [MenuInfantilO2]) (is-a Menu)
          (entrantsMenu $?E) (primerMenu ?P)
          (segonMenu ?S) (postresMenu ?PO)
          (begudaGeneralMenu $?BG))
  =>
  (printout t crlf "----------------------- MENÚ INFANTIL 2 -----------------------" crlf)
  (REFINAMENT::print-list+preu "Entrants:" $?E)
  (printout t "Primer  : " (REFINAMENT::name-of ?P)
               " (" (format nil "%.2f" (REFINAMENT::preu-of ?P)) "€)" crlf)
  (printout t "Segon   : " (REFINAMENT::name-of ?S)
               " (" (format nil "%.2f" (REFINAMENT::preu-of ?S)) "€)" crlf)
  (printout t "Postre  : " (REFINAMENT::name-of ?PO)
               " (" (format nil "%.2f" (REFINAMENT::preu-of ?PO)) "€)" crlf)
  (bind ?cost (REFINAMENT::preu-menu-percap-des-de-binds ?P ?S ?PO $?E $?BG (create$) (create$) (create$)))
  (printout t "----------------------------------------------------------------" crlf)
  (assert (preu-real-infantil (opcio InfantilO2) (percap ?cost)))
  (assert (impressio-feta (opcio InfantilO2))))

(defrule REFINAMENT::imprimir-menu-infantil-O3
  (menus-infantils-creats)
  (not (impressio-feta (opcio InfantilO3)))
  (menu-opcio-preu (qui [MenuInfantilO3]) (percap ?pp))
  (object (name [MenuInfantilO3]) (is-a Menu)
          (entrantsMenu $?E) (primerMenu ?P)
          (segonMenu ?S) (postresMenu ?PO)
          (begudaGeneralMenu $?BG))
  =>
  (printout t crlf "----------------------- MENÚ INFANTIL 3 -----------------------" crlf)
  (REFINAMENT::print-list+preu "Entrants:" $?E)
  (printout t "Primer  : " (REFINAMENT::name-of ?P)
               " (" (format nil "%.2f" (REFINAMENT::preu-of ?P)) "€)" crlf)
  (printout t "Segon   : " (REFINAMENT::name-of ?S)
               " (" (format nil "%.2f" (REFINAMENT::preu-of ?S)) "€)" crlf)
  (printout t "Postre  : " (REFINAMENT::name-of ?PO)
               " (" (format nil "%.2f" (REFINAMENT::preu-of ?PO)) "€)" crlf)
  (bind ?cost (REFINAMENT::preu-menu-percap-des-de-binds ?P ?S ?PO $?E $?BG (create$) (create$) (create$)))
  (printout t "----------------------------------------------------------------" crlf)
  (assert (preu-real-infantil (opcio InfantilO3) (percap ?cost)))
  (assert (impressio-feta (opcio InfantilO3))))

(defrule REFINAMENT::resum-menus-infantils-ordenats
  (preu-real-infantil (opcio InfantilO1) (percap ?p1))
  (preu-real-infantil (opcio InfantilO2) (percap ?p2))
  (preu-real-infantil (opcio InfantilO3) (percap ?p3))
  (impressio-feta (opcio InfantilO1))
  (impressio-feta (opcio InfantilO2))
  (impressio-feta (opcio InfantilO3))
  =>
  (printout t crlf "--------------- ORDRE FINAL SEGONS COST ESTIMAT (INFANTIL) ---------------" crlf)

  (bind $?opcions (create$ InfantilO1 InfantilO2 InfantilO3))
  (bind $?preus   (create$ ?p1 ?p2 ?p3))

  (bind ?n (length$ $?preus))
  (bind ?i 1)
  (while (< ?i ?n) do
    (bind ?j (+ ?i 1))
    (while (<= ?j ?n) do
      (if (> (nth$ ?i $?preus) (nth$ ?j $?preus)) then
        (bind ?tmpP (nth$ ?i $?preus))
        (bind ?tmpO (nth$ ?i $?opcions))
        (bind $?preus (replace$ $?preus ?i ?i (nth$ ?j $?preus)))
        (bind $?preus (replace$ $?preus ?j ?j ?tmpP))
        (bind $?opcions (replace$ $?opcions ?i ?i (nth$ ?j $?opcions)))
        (bind $?opcions (replace$ $?opcions ?j ?j ?tmpO)))
      (bind ?j (+ ?j 1)))
    (bind ?i (+ ?i 1)))

  (bind ?k 1)
  (while (<= ?k ?n) do
    (printout t (nth$ ?k $?opcions) ": " (format nil "%.2f" (nth$ ?k $?preus)) " €/pax" crlf)
    (bind ?k (+ ?k 1)))
  (printout t "----------------------------------------------------------------" crlf))


(defrule REFINAMENT::imprimir-menu-celiac
  (not (impressio-feta (opcio Celiac)))
  (menu-opcio-preu (qui [MenuCeliac]) (percap ?pp))
  (object (name [MenuCeliac]) (is-a Menu)
          (entrantsMenu $?E) (primerMenu ?P)
          (segonMenu ?S) (postresMenu ?PO)
          (begudaGeneralMenu $?BG))
  (epoca-inst (inst ?EPOCA))
  =>
  (printout t crlf "----------------------- MENÚ CELÍAC (apte) -----------------------" crlf)
  (REFINAMENT::print-list+preu "Entrants:" $?E)
  (printout t "Primer  : " (REFINAMENT::name-of ?P)
               " (" (format nil "%.2f" (REFINAMENT::preu-of ?P)) "€)" crlf)
  (printout t "Segon   : " (REFINAMENT::name-of ?S)
               " (" (format nil "%.2f" (REFINAMENT::preu-of ?S)) "€)" crlf)
  (printout t "Postre  : " (REFINAMENT::name-of ?PO)
               " (" (format nil "%.2f" (REFINAMENT::preu-of ?PO)) "€)" crlf)

  (bind ?cost (REFINAMENT::preu-menu-percap-des-de-binds ?P ?S ?PO $?E $?BG (create$) (create$) (create$)))
  (printout t "Cost estimat (€/pax): " (format nil "%.2f" ?cost) crlf)
  (printout t "----------------------------------------------------------------" crlf)

  (assert (impressio-feta (opcio Celiac))))

(defrule REFINAMENT::imprimir-menu-infantil-celiac
  "Imprimeix el menú infantil per a celíacs amb preus i plats seleccionats."
  (not (impressio-feta (opcio InfantilCeliac)))
  (menu-opcio-preu (qui [MenuInfantilCeliac]) (percap ?pp))
  (object (name [MenuInfantilCeliac]) (is-a Menu)
          (entrantsMenu $?E)
          (primerMenu ?P)
          (segonMenu ?S)
          (postresMenu ?PO)
          (begudaGeneralMenu $?BG))
  =>
  (printout t crlf "------------------ MENÚ INFANTIL CELÍAC (apte) ------------------" crlf)
  (REFINAMENT::print-list+preu "Entrants:" $?E)
  (printout t "Primer  : " (REFINAMENT::name-of ?P)
               " (" (format nil "%.2f" (REFINAMENT::preu-of ?P)) "€)" crlf)
  (printout t "Segon   : " (REFINAMENT::name-of ?S)
               " (" (format nil "%.2f" (REFINAMENT::preu-of ?S)) "€)" crlf)
  (printout t "Postre  : " (REFINAMENT::name-of ?PO)
               " (" (format nil "%.2f" (REFINAMENT::preu-of ?PO)) "€)" crlf)
  (bind ?cost (REFINAMENT::preu-menu-percap-des-de-binds
                 ?P ?S ?PO $?E $?BG (create$) (create$) (create$)))
  (printout t "Cost estimat (€/pax): " (format nil "%.2f" ?cost) crlf)
  (printout t "------------------------------------------------------------------" crlf)
            
  (assert (impressio-feta (opcio InfantilCeliac))))

(defrule REFINAMENT::imprimir-menu-halal
  (not (impressio-feta (opcio Halal)))
  (menu-opcio-preu (qui [MenuHalal]) (percap ?pp))
  (object (name [MenuHalal]) (is-a Menu)
          (entrantsMenu $?E) (primerMenu ?P)
          (segonMenu ?S) (postresMenu ?PO)
          (begudaGeneralMenu $?BG))
  (epoca-inst (inst ?EPOCA))
  =>
  (printout t crlf "----------------------- MENÚ HALAL (apte) -----------------------" crlf)
  (REFINAMENT::print-list+preu "Entrants:" $?E)
  (printout t "Primer  : " (REFINAMENT::name-of ?P)
               " (" (format nil "%.2f" (REFINAMENT::preu-of ?P)) "€)" crlf)
  (printout t "Segon   : " (REFINAMENT::name-of ?S)
               " (" (format nil "%.2f" (REFINAMENT::preu-of ?S)) "€)" crlf)
  (printout t "Postre  : " (REFINAMENT::name-of ?PO)
               " (" (format nil "%.2f" (REFINAMENT::preu-of ?PO)) "€)" crlf)

  (bind ?cost (REFINAMENT::preu-menu-percap-des-de-binds ?P ?S ?PO $?E $?BG (create$) (create$) (create$)))
  (printout t "Cost estimat (€/pax): " (format nil "%.2f" ?cost) crlf)
  (printout t "----------------------------------------------------------------" crlf)

  (assert (impressio-feta (opcio Halal))))

(defrule REFINAMENT::imprimir-menu-infantil-halal
  (not (impressio-feta (opcio InfantilHalal)))
  (menu-opcio-preu (qui [MenuInfantilHalal]) (percap ?pp))
  (object (name [MenuInfantilHalal]) (is-a Menu)
          (entrantsMenu $?E)
          (primerMenu ?P)
          (segonMenu ?S)
          (postresMenu ?PO)
          (begudaGeneralMenu $?BG))
  =>
  (printout t crlf "------------------- MENÚ INFANTIL HALAL (apte) -------------------" crlf)
  (REFINAMENT::print-list+preu "Entrants:" $?E)
  (printout t "Primer  : " (REFINAMENT::name-of ?P)
               " (" (format nil "%.2f" (REFINAMENT::preu-of ?P)) "€)" crlf)
  (printout t "Segon   : " (REFINAMENT::name-of ?S)
               " (" (format nil "%.2f" (REFINAMENT::preu-of ?S)) "€)" crlf)
  (printout t "Postre  : " (REFINAMENT::name-of ?PO)
               " (" (format nil "%.2f" (REFINAMENT::preu-of ?PO)) "€)" crlf)
  (bind ?cost (REFINAMENT::preu-menu-percap-des-de-binds
                 ?P ?S ?PO $?E $?BG (create$) (create$) (create$)))
  (printout t "Cost estimat (€/pax): " (format nil "%.2f" ?cost) crlf)
  (printout t "----------------------------------------------------------------" crlf)

  (assert (impressio-feta (opcio InfantilHalal))))


(defrule REFINAMENT::imprimir-menu-lactosa
  (not (impressio-feta (opcio Lactosa)))
  (menu-opcio-preu (qui [MenuLactosa]) (percap ?pp))
  (object (name [MenuLactosa]) (is-a Menu)
          (entrantsMenu $?E)
          (primerMenu ?P)
          (segonMenu ?S)
          (postresMenu ?PO)
          (begudaGeneralMenu $?BG))
  (epoca-inst (inst ?EPOCA))
  =>
  (printout t crlf "------------------- MENÚ SENSE LACTOSA (apte) -------------------" crlf)
  (REFINAMENT::print-list+preu "Entrants:" $?E)
  (printout t "Primer  : " (REFINAMENT::name-of ?P)
               " (" (format nil "%.2f" (REFINAMENT::preu-of ?P)) "€)" crlf)
  (printout t "Segon   : " (REFINAMENT::name-of ?S)
               " (" (format nil "%.2f" (REFINAMENT::preu-of ?S)) "€)" crlf)
  (printout t "Postre  : " (REFINAMENT::name-of ?PO)
               " (" (format nil "%.2f" (REFINAMENT::preu-of ?PO)) "€)" crlf)

  (bind ?cost (REFINAMENT::preu-menu-percap-des-de-binds
                 ?P ?S ?PO $?E $?BG (create$) (create$) (create$)))
  (printout t "Cost estimat (€/pax): " (format nil "%.2f" ?cost) crlf)
  (printout t "----------------------------------------------------------------" crlf)

  (assert (impressio-feta (opcio Lactosa))))

(defrule REFINAMENT::imprimir-menu-infantil-lactosa
  (not (impressio-feta (opcio InfantilLactosa)))
  (menu-opcio-preu (qui [MenuInfantilLactosa]) (percap ?pp))
  (object (name [MenuInfantilLactosa]) (is-a Menu)
          (entrantsMenu $?E)
          (primerMenu ?P)
          (segonMenu ?S)
          (postresMenu ?PO)
          (begudaGeneralMenu $?BG))
  =>
  (printout t crlf "--------------- MENÚ INFANTIL SENSE LACTOSA (apte) ---------------" crlf)
  (REFINAMENT::print-list+preu "Entrants:" $?E)
  (printout t "Primer  : " (REFINAMENT::name-of ?P) crlf)
  (printout t "Segon   : " (REFINAMENT::name-of ?S) crlf)
  (printout t "Postre  : " (REFINAMENT::name-of ?PO) crlf)
  (bind ?cost (REFINAMENT::preu-menu-percap-des-de-binds ?P ?S ?PO $?E $?BG (create$) (create$) (create$)))
  (printout t "Cost estimat (€/pax): " (format nil "%.2f" ?cost) crlf)
  (assert (impressio-feta (opcio InfantilLactosa))))


(defrule REFINAMENT::imprimir-menu-vegetaria
  (not (impressio-feta (opcio Vegetaria)))
  (menu-opcio-preu (qui [MenuVegetaria]) (percap ?pp))
  (object (name [MenuVegetaria]) (is-a Menu)
          (entrantsMenu $?E) (primerMenu ?P)
          (segonMenu ?S) (postresMenu ?PO)
          (begudaGeneralMenu $?BG))
  (epoca-inst (inst ?EPOCA))
  =>
  (printout t crlf "----------------------- MENÚ VEGETARIÀ (apte) -----------------------" crlf)
  (REFINAMENT::print-list+preu "Entrants:" $?E)
  (printout t "Primer  : " (REFINAMENT::name-of ?P)
               " (" (format nil "%.2f" (REFINAMENT::preu-of ?P)) "€)" crlf)
  (printout t "Segon   : " (REFINAMENT::name-of ?S)
               " (" (format nil "%.2f" (REFINAMENT::preu-of ?S)) "€)" crlf)
  (printout t "Postre  : " (REFINAMENT::name-of ?PO)
               " (" (format nil "%.2f" (REFINAMENT::preu-of ?PO)) "€)" crlf)

  (bind ?cost (REFINAMENT::preu-menu-percap-des-de-binds ?P ?S ?PO $?E $?BG (create$) (create$) (create$)))
  (printout t "Cost estimat (€/pax): " (format nil "%.2f" ?cost) crlf)
  (printout t "----------------------------------------------------------------" crlf)

  (assert (impressio-feta (opcio Vegetaria))))

(defrule REFINAMENT::imprimir-menu-infantil-vegetaria
  (not (impressio-feta (opcio InfantilVegetaria)))
  (menu-opcio-preu (qui [MenuInfantilVegetaria]) (percap ?pp))
  (object (name [MenuInfantilVegetaria]) (is-a Menu)
          (entrantsMenu $?E) (primerMenu ?P)
          (segonMenu ?S) (postresMenu ?PO)
          (begudaGeneralMenu $?BG))
  =>
  (printout t crlf "----------------- MENÚ INFANTIL VEGETARIÀ (apte) -----------------" crlf)
  (REFINAMENT::print-list+preu "Entrants:" $?E)
  (printout t "Primer  : " (REFINAMENT::name-of ?P) crlf)
  (printout t "Segon   : " (REFINAMENT::name-of ?S) crlf)
  (printout t "Postre  : " (REFINAMENT::name-of ?PO) crlf)
  (bind ?cost (REFINAMENT::preu-menu-percap-des-de-binds ?P ?S ?PO $?E $?BG (create$) (create$) (create$)))
  (assert (impressio-feta (opcio InfantilVegetaria))))

(defrule REFINAMENT::imprimir-menu-vega
  (not (impressio-feta (opcio Vega)))
  (menu-opcio-preu (qui [MenuVega]) (percap ?pp))
  (object (name [MenuVega]) (is-a Menu)
          (entrantsMenu $?E) (primerMenu ?P)
          (segonMenu ?S) (postresMenu ?PO)
          (begudaGeneralMenu $?BG))
  (epoca-inst (inst ?EPOCA))
  =>
  (printout t crlf "----------------------- MENÚ VEGÀ (apte) -----------------------" crlf)
  (REFINAMENT::print-list+preu "Entrants:" $?E)
  (printout t "Primer  : " (REFINAMENT::name-of ?P)
               " (" (format nil "%.2f" (REFINAMENT::preu-of ?P)) "€)" crlf)
  (printout t "Segon   : " (REFINAMENT::name-of ?S)
               " (" (format nil "%.2f" (REFINAMENT::preu-of ?S)) "€)" crlf)
  (printout t "Postre  : " (REFINAMENT::name-of ?PO)
               " (" (format nil "%.2f" (REFINAMENT::preu-of ?PO)) "€)" crlf)

  (bind ?cost (REFINAMENT::preu-menu-percap-des-de-binds
                 ?P ?S ?PO $?E $?BG (create$) (create$) (create$)))
  (printout t "Cost estimat (€/pax): " (format nil "%.2f" ?cost) crlf)
  (printout t "----------------------------------------------------------------" crlf)

  (assert (impressio-feta (opcio Vega))))

(defrule REFINAMENT::imprimir-menu-infantil-vega
  (not (impressio-feta (opcio InfantilVega)))
  (menu-opcio-preu (qui [MenuInfantilVega]) (percap ?pp))
  (object (name [MenuInfantilVega]) (is-a Menu)
          (entrantsMenu $?E) (primerMenu ?P)
          (segonMenu ?S) (postresMenu ?PO)
          (begudaGeneralMenu $?BG))
  =>
  (printout t crlf "------------------- MENÚ INFANTIL VEGÀ (apte) -------------------" crlf)
  (REFINAMENT::print-list+preu "Entrants:" $?E)
  (printout t "Primer  : " (REFINAMENT::name-of ?P) crlf)
  (printout t "Segon   : " (REFINAMENT::name-of ?S) crlf)
  (printout t "Postre  : " (REFINAMENT::name-of ?PO) crlf)
  (bind ?cost (REFINAMENT::preu-menu-percap-des-de-binds ?P ?S ?PO $?E $?BG (create$) (create$) (create$)))
  (assert (impressio-feta (opcio InfantilVega))))

(defrule REFINAMENT::imprimir-menu-fruitsecs
  (not (impressio-feta (opcio Fruitsecs)))
  (menu-opcio-preu (qui [MenuFruitsecs]) (percap ?pp))
  (object (name [MenuFruitsecs]) (is-a Menu)
          (entrantsMenu $?E) (primerMenu ?P)
          (segonMenu ?S) (postresMenu ?PO)
          (begudaGeneralMenu $?BG))
  (epoca-inst (inst ?EPOCA))
  =>
  (printout t crlf "----------------- MENÚ SENSE FRUITS SECS (apte) -----------------" crlf)
  (REFINAMENT::print-list+preu "Entrants:" $?E)
  (printout t "Primer  : " (REFINAMENT::name-of ?P)
               " (" (format nil "%.2f" (REFINAMENT::preu-of ?P)) "€)" crlf)
  (printout t "Segon   : " (REFINAMENT::name-of ?S)
               " (" (format nil "%.2f" (REFINAMENT::preu-of ?S)) "€)" crlf)
  (printout t "Postre  : " (REFINAMENT::name-of ?PO)
               " (" (format nil "%.2f" (REFINAMENT::preu-of ?PO)) "€)" crlf)
  (bind ?cost (REFINAMENT::preu-menu-percap-des-de-binds
                 ?P ?S ?PO $?E $?BG (create$) (create$) (create$)))
  (printout t "Cost estimat (€/pax): " (format nil "%.2f" ?cost) crlf)
  (printout t "----------------------------------------------------------------" crlf)

  (assert (impressio-feta (opcio Fruitsecs))))

(defrule REFINAMENT::imprimir-menu-infantil-fruitsecs
  (not (impressio-feta (opcio InfantilFruitsecs)))
  (menu-opcio-preu (qui [MenuInfantilFruitsecs]) (percap ?pp))
  (object (name [MenuInfantilFruitsecs]) (is-a Menu)
          (entrantsMenu $?E) (primerMenu ?P)
          (segonMenu ?S) (postresMenu ?PO)
          (begudaGeneralMenu $?BG))
  =>
  (printout t crlf "------------- MENÚ INFANTIL SENSE FRUITS SECS (apte) -------------" crlf)
  (REFINAMENT::print-list+preu "Entrants:" $?E)
  (printout t "Primer  : " (REFINAMENT::name-of ?P) crlf)
  (printout t "Segon   : " (REFINAMENT::name-of ?S) crlf)
  (printout t "Postre  : " (REFINAMENT::name-of ?PO) crlf)
  (bind ?cost (REFINAMENT::preu-menu-percap-des-de-binds ?P ?S ?PO $?E $?BG (create$) (create$) (create$)))
  (assert (impressio-feta (opcio InfantilFruitsecs))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; IMPORTADOR
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmodule IMPORTADOR (import ONTO ?ALL) (import DB ?ALL) (export ?ALL))

(deffunction IMPORTADOR::pascalize (?sym)
  (bind ?s (lowcase (str-cat ?sym)))
  (bind ?out "") (bind ?cap TRUE)
  (bind ?len (str-length ?s)) (bind ?i 1)
  (while (<= ?i ?len) do
    (bind ?ch (sub-string ?i ?i ?s))
    (if (or (eq ?ch "_") (eq ?ch "-") (eq ?ch " "))
        then (bind ?cap TRUE)
        else
          (if ?cap then (bind ?out (str-cat ?out (upcase ?ch))) else (bind ?out (str-cat ?out ?ch)))
          (bind ?cap FALSE))
    (bind ?i (+ ?i 1)))
  (return ?out))

(deffunction IMPORTADOR::iname (?prefix ?sym)
  (return (symbol-to-instance-name (sym-cat ?prefix (IMPORTADOR::pascalize ?sym)))))

(deffunction IMPORTADOR::ensure-inst (?class ?sym ?prefix)
  (bind ?iname (IMPORTADOR::iname ?prefix ?sym))
  (if (instance-existp ?iname)
    then (return ?iname)
    else (return (make-instance ?iname of ?class (nom ?sym)))))

(deffunction IMPORTADOR::map-temporada (?t)
  (if (eq ?t anual)      then (return [TemporadaAnual]))
  (if (eq ?t primavera)  then (return [TemporadaPrimavera]))
  (if (eq ?t estiu)      then (return [TemporadaEstiu]))
  (if (eq ?t tardor)     then (return [TemporadaTardor]))
  (if (eq ?t hivern)     then (return [TemporadaHivern]))
  (return [TemporadaAnual]))

(deffunction IMPORTADOR::class-per-tipus-plat (?tp)
  (if (eq ?tp beguda) then (return ONTO::Beguda))
  (return ONTO::Plat))

(deffunction IMPORTADOR::tipus-plat-inst (?tp)
  (if (eq ?tp entrant)   then (return [PlatEntrants]))
  (if (eq ?tp primer)    then (return [PlatPrimer]))
  (if (eq ?tp segon)     then (return [PlatSegon]))
  (if (eq ?tp principal) then (return [PlatPrimer]))
  (if (eq ?tp postres)   then (return [PlatPostre]))
  (return [PlatPrimer]))

(defrule IMPORTADOR::row-a-ontologia
  (declare (salience 100))
  ?r <- (row
          (nom_plat ?nom)
          (tipus_plat ?tp)
          (ingredient_principal ?ing)
          (temporada ?temp)
          (preu_plat ?preu)
          (complexitat ?cx)
          (temperatura ?tplat)
          (dieta ?diet)
          (familia ?fam)
          (origen_plat ?org)
          (tipus_public ?tpubl)
          (estil_plat ?estil))
  (test (not (instance-existp (IMPORTADOR::iname "" ?nom))))
  =>
  (bind ?CLASS  (IMPORTADOR::class-per-tipus-plat ?tp))
  (bind ?TPINST (IMPORTADOR::tipus-plat-inst ?tp))
  (bind ?TEMP   (IMPORTADOR::map-temporada ?temp))

  (bind ?ING  (IMPORTADOR::ensure-inst ONTO::Ingredient-principal ?ing "Ing:"))
  (bind ?DIET (IMPORTADOR::ensure-inst ONTO::Dieta   ?diet "Diet:"))
  (bind ?FAM  (IMPORTADOR::ensure-inst ONTO::Familia ?fam  "Fam:"))

  (bind ?ORIGCLASS (if (eq ?CLASS ONTO::Beguda) then ONTO::Origen-beguda else ONTO::Origen-plat))
  (bind ?ORG (IMPORTADOR::ensure-inst ?ORIGCLASS ?org "Org:"))

  (bind ?NAME (IMPORTADOR::iname "" ?nom))

  (make-instance ?NAME of ?CLASS
    (nom ?nom)
    (ingredient-principal ?ING)
    (temporada ?TEMP)
    (dieta ?DIET)
    (familia ?FAM)
    (origen ?ORG)
    (preu ?preu)
    (complexitat ?cx)
    (temperatura ?tplat)
    (tipus-plat ?TPINST)
    (tipus_public ?tpubl)
    (estil_plat ?estil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MAIN
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmodule MAIN (import PREGUNTES ?ALL) (import ABSTRACCIO ?ALL)
                 (import ASSOCIACIO ?ALL) (import ONTOBRIDGE ?ALL)
                 (import REFINAMENT ?ALL))

(defrule MAIN::init
  (declare (salience 200))
  =>
  (printout t crlf "RicoRico — Assistent de Menús personalitzats" crlf)
  (printout t "----------------------------------------------------------------" crlf)
  (focus IMPORTADOR PREGUNTES ABSTRACCIO ASSOCIACIO ONTOBRIDGE REFINAMENT))