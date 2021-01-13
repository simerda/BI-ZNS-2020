(defglobal ?*suma* = 0 )

(deffunction dialog (?dotaz)
  (printout t ?dotaz crlf)
  (bind ?odpoved (read))
  ?odpoved)

; default rule - otazka jestli mam figurku ve hre & co jsem naposled hodil
(defrule R0
  (declare (salience 10))
  (initial-fact)
=>
  (bind ?x (dialog "Mas figurku ve hre (ano/ne)?" ))
  (assert (figurka-ve-hre ?x))
  (bind ?x (dialog "Co jsi naposled hodil (1/2/3/4/5/6)?"))
  (assert (posledni-hod ?x ))
  (bind ?*suma* (integer ?x))
)

; pokud nemam figurku ve hre ale hodil jsem sestku
; zaregistruje novej hod
(defrule T3
  (figurka-ve-hre ne)
  ?x <- (figurka-ve-hre ne)
  ?y <- (posledni-hod 6)
  (posledni-hod 6)
=>
  (printout t "Nasad novou figurku do hry." crlf )
  (printout t "Hazej znovu!" crlf )
  (retract ?x)
  (retract ?y)
  
  (bind ?x (dialog "Co jsi naposled hodil (1/2/3/4/5/6)?"))
  (assert (posledni-hod ?x ))

  (bind ?*suma* (+ ?*suma* -6)) 
  (bind ?*suma* (+ ?*suma* (integer ?x))) 
  (assert (figurka-ve-hre ano)))

; pokud nemam figurku ani nepadla sestka, tak nemuzu hrat dal
(defrule T4
  (figurka-ve-hre ne)
  (not (posledni-hod 6))
=>
  (printout t "Nelze tahnout, cekej dalsi kolo." crlf ) )
; --------------------------------------------------------------------------------------

; naposledy padla sestka --> hazej znovu
(defrule T5
  (figurka-ve-hre ano)
  (posledni-hod 6)
  ?y <- (posledni-hod 6)
  (not (predposledni-hod 6))
=>
  (bind ?x (dialog "Hazej znovu, kolik padlo (1/2/3/4/5/6)?" ))
  (retract ?y)
  (assert (predposledni-hod 6))
  (assert (posledni-hod ?x))
  (bind ?*suma* (+ ?*suma* (integer ?x)) ) )


; pokud padla sestka predtim, zeptej se, jestli chce nasadit novou figurku
(defrule T6
  (predposledni-hod 6)
=>
  (bind ?x (dialog "Chces nasadit (mas-li k dispozici) novou figurku (ano/ne)?"))
  (assert (chce-nasadit ?x))
  (bind ?y (dialog "Je na tvem domecku volno/cizi figurka (ano/ne)?"))
  (assert (je-v-domecku-volno ?y))
  (if (and (eq ?x ano)
             (eq ?y ano))
    then
      (printout t "Nasad novou figurku." crlf )
      (bind ?*suma* (+ ?*suma* -6))
    else
      (printout t "Pokracujeme v dalsi vzdalene hre." crlf )
    ))

; vsechny predesla pravidla prosla --> main loop
; pocet "chodu" je v globalni promenne ?*suma*
; (defrule T7
;   (not (figurka-ve-hre ne))
; =>
;   (printout t "Hrajeme dal! -> " ?*suma* crlf ))

; otazky jestli mam N poli pred sebou domecek nebo cizi figurku
; odpovedi ulozeny
(defrule T8
  (figurka-ve-hre ano)
=>
  (bind ?x (dialog "Mas nejakou figurku, ktere zbyva N poli do domecku? (ano / ne)"))
  (assert (n-do-domecku ?x))

  (bind ?x (dialog "Mas nejakou figurku, ktera ma N poli pred sebou cizi figurku? (ano / ne)"))
  (assert (figurka-pred ?x))

)

; Pokud jsou obe odpovedi z T8 "ano", system doporuci tahnout do domecku -> konec hry.
(defrule T10-1
  (and
    (figurka-pred ano)
    (n-do-domecku ano))
=>
  (printout t "Tahni figurkou, ktera skonci v domecku." crlf)
  (halt)
)

; system doporuci tahnout danou figurkou -> konec hry
(defrule T10
  (or
    (figurka-pred ano)
    (n-do-domecku ano)
    (vice-nez-jedna ne)
  )
=>
  (printout t "Tahni tou figurkou." crlf)
  (halt)
)

; Pokud pred sebou nemam zadne privetive cile -> otazka jestli je moje figurka ohrozena.
(defrule T11
  (figurka-ve-hre ano)
=>
  (bind ?x (dialog "Máš nějakou figurku, která má 6 a méně polí za sebou cizí figurku?"))
  (assert (cizi-figurka-za-hracem ?x))
)

; otazka, zda-li je ohrozena vice nez jedna hracova figurka.
(defrule T12
  (cizi-figurka-za-hracem ano)
=>
  (bind ?x (dialog "Je jich vice nez 1? (ano / ne)"))
  (assert (vice-nez-jedna ?x))
)

; otazka, zda-li vubec muze hrac tahnout -> pokud ne, konec hry
(defrule T13
  (cizi-figurka-za-hracem ne)
=>
  (bind ?x (dialog "Mas nejakou figurku, kterou muzes tahnout? (ano / ne)"))
  (assert (muze-hrac-tahnout ?x))
  (if (eq ?x ne)
    then
    (printout t "Konec tahu, nejde tahnout." crlf) 
  )
)

; system poradi hraci tahnout figurkou nejvzdalenejsi od startu.
(defrule T14
  (or
    (vice-nez-jedna ano)
    (muze-hrac-tahnout ano))
=>
  (printout t "Tahni tou, ktera je nejdal od startu." crlf)
)
