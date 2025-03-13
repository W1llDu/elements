#lang racket

(provide make-character
         make-weapon
         make-skill
         make-attack
         make-artifact
         make-triggered-buff
         make-unconditional-buff
         make-applied-buff
         make-attack-sequence
         make-attribute
         make-percent
         make-enemy
         make-resistances
         calc-dmg)

(define-struct character [hp atk def critr critd em attacks weapon skill burst artifacts])
(define-struct weapon [atk substat buffs])
(define-struct skill [cd attr duration type buffs])
(define-struct attack [attr duration type]) ; split attr
(define-struct artifact [set-name main-stat substats])
(define-struct triggered-buff [effect trigger limit party-wide duration])
(define-struct unconditional-buff [effect party-wide])
(define-struct applied-buff [effect limit party-wide duration])
(define-struct attack-sequence [normals charged plunge])
(define-struct attribute [attr modifier]) ; amount is either number or percent (what is valid depends on attr)
(define-struct percent [attr p])
(define-struct enemy [def res reduction])
(define-struct resistances [pyro hydro electro cryo geo anemo dendro physical])
; inline this into skill data
#;(define-struct damage [attr duration elem-type])

; internal
(define-struct flat-char [hp atk def critr critd em attacks skill burst unconditional-buffs trigger-buffs])
(define-struct stat-info [hp atk def critr critd em])
(define-struct damage-info [base-dmg base-dmg-mult base-add
                                     dmg-mult
                                     def-mult
                                     res
                                     amp-mult
                                     critr
                                     critd])
; base-dmg-mult: talent/constellation
; base-add: talent/constellation/weapon/artifact (increase by scaling amount (def% 56))

; final output: dmg, time, dmg/s

(define (calc-dmg team enemy attack-string)
  ; pull out party-wide uncond buffs into active-buffs
  (calc-dmg/acc (map flatten-char team) enemy attack-string 1 empty 0 0))

(define (flatten-char char)
  (make-flat-char (character-hp char)
                  (+ (character-atk char) (weapon-atk (character-weapon char)))
                  (character-def char)
                  (character-critr char)
                  (character-critd char)
                  (character-em char)
                  (character-attacks char)
                  (character-skill char)
                  (character-burst char)
                  empty #;(append weapon-uncond-buffs artifact-uncond-buff)
                  empty #;(append weapon-trigger-buffs artifact-trigger-buff)
                  ))

(define (calc-dmg/acc team enemy attack-string nc active-buffs dmg time) ; more acc args like buffs (later)
  (cond [(empty? attack-string) (list dmg time)]
        [(cons? attack-string) (cond [(symbol=? 'S (first attack-string)) (calc-dmg/acc (foldr cons (list (first team)) (rest team))
                                                                                        enemy
                                                                                        (rest attack-string)
                                                                                        active-buffs
                                                                                        dmg
                                                                                        (+ time 1))]
                                     ; apply buffs
                                     ; attack is done, then buffs
                                     [else (let ([nc* (if (symbol=? 'N (first attack-string))
                                                          (if (= nc (length (attack-sequence-normals (flat-char-attacks (first team)))))
                                                              1
                                                              (+ 1 nc))
                                                          1)])
                                             (calc-dmg/acc team
                                                           enemy
                                                           (rest attack-string)
                                                           nc*
                                                           active-buffs
                                                           (+ dmg
                                                              (calc-single-dmg (generate-dmg-info (first team)
                                                                                                  enemy
                                                                                                  (first attack-string)
                                                                                                  nc)))
                                                           (+ time (calc-duration (flat-char-attacks (first team)) (first attack-string) nc))))])]))

(define (calc-duration attacks attack nc)
  ; n/c/e/q/nj/nd/cj/cd/ep/jp
  (cond [(symbol=? 'N attack) (attack-duration (list-ref (attack-sequence-normals attacks) (sub1 nc)))]
        ))

(define (generate-dmg-info char enemy attack nc)
  (let ([stats (calc-total-stats char)])
    ; un-lambda the attrs (should be runtime job)
    ; n/c/e/q/nj/nd/cj/cd/ep/jp
    (cond [(symbol=? 'N attack) (make-damage-info (calc-attr (attack-attr (list-ref (attack-sequence-normals (flat-char-attacks char)) (sub1 nc))) stats)
                                                  ; should be in terms of total atk, not base
                                                  1 ; base-dmg-mult
                                                  0 ; base-add
                                                  1 ; dmg-mult
                                                  1 ; def-mult ; TODO
                                                  0 ; actual enemy res, figure out attack attr
                                                  1 ; amp-mult (reaction)
                                                  (stat-info-critr stats)
                                                  (stat-info-critd stats))]
          )))

(define (calc-attr attr stats)
  (cond
    ; must be %?
    [(symbol=? (attribute-attr attr) 'atk) (calc-modifier (attribute-modifier attr) stats)]
    ))

(define (calc-modifier modifier stats)
  (if (number? modifier)
      modifier
      (* (percent-p modifier) 0.01 (lookup stats (percent-attr modifier)))))

(define (lookup stats attr)
  (cond
    [(symbol=? attr 'atk%) (stat-info-atk stats)]
    ))

; do a tree traversal (abstract out, use symbols?) flat incr vs incr by other amt (atk (def% 50)) increases atk by 50% of base def
(define (calc-total-stats char)
  ; remember to scale % to decimal
  (make-stat-info (+ (* (flat-char-hp char) ; base hp
                        (+ 1 #;hp%))
                     #;flat-hp)
                  (+ (* (flat-char-atk char) ; base atk
                        (+ 1 #;atk%))
                     #;flat-atk)
                  (+ (* (flat-char-def char) ; base def
                        (+ 1 #;def%))
                     #;flat-def)
                  100 #;cr-sum
                  100 #;cd-sum
                  0 #;flat-em-sum))

(define (calc-single-dmg dmg)
  (* (+ (* (damage-info-base-dmg dmg) (damage-info-base-dmg-mult dmg)) (damage-info-base-add dmg))
     (damage-info-dmg-mult dmg)
     (damage-info-def-mult dmg)
     (calc-res (damage-info-res dmg))
     (damage-info-amp-mult dmg)
     (calc-crit (damage-info-critr dmg) (damage-info-critd dmg))))

(define (calc-res res)
  (cond [(< res 0) (- 1 (/ res 2))]
        [(>= res 0.75) (/ 1 (+ (* 4 res) 1))]
        [else (- 1 res)]))

(define (calc-crit crit-rate crit-dmg)
  (+ (- 1 (/ crit-rate 100))
     (* (/ crit-rate 100)
        (+ 1 (/ crit-dmg 100)))))