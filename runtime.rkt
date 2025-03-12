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
(define-struct stat-info [hp atk def em critr critd])
(define-struct damage-info [base-dmg base-dmg-mult base-add
                                     dmg-mult
                                     def-mult
                                     res
                                     amp-mult
                                     crit-rate
                                     crit-dmg])
; base-dmg-mult: talent/constellation
; base-add: talent/constellation/weapon/artifact (increase by scaling amount (def% 56))

; final output: dmg, time, dmg/s

(define (calc-dmg team enemy attack-string)
  (calc-dmg/acc team enemy attack-string 1 0 0))

(define (calc-dmg/acc team enemy attack-string nc dmg time) ; more acc args like buffs (later)
  (cond [(empty? attack-string) (list dmg time)]
        [(cons? attack-string) (cond [(symbol=? 'S (first attack-string)) (calc-dmg/acc (foldr cons (list (first team)) (rest team))
                                                                                        enemy
                                                                                        (rest attack-string)
                                                                                        1
                                                                                        (+ time 1))]
                                     [else (let ([nc* (if (symbol=? 'N (first attack-string))
                                                          (if (= nc (length (attack-sequence-normals (character-attacks (first team)))))
                                                              1
                                                              (+ 1 nc))
                                                          1)])
                                             (calc-dmg/acc team
                                                           enemy
                                                           (rest attack-string)
                                                           nc*
                                                           (+ dmg
                                                              (calc-single-dmg (generate-dmg-info (first team)
                                                                                                  enemy
                                                                                                  (first attack-string)
                                                                                                  nc)))
                                                           (+ time (calc-duration (character-attacks (first team)) (first attack-string) nc))))])]))

(define (calc-duration attacks attack nc)
  ; n/c/e/q/nj/nd/cj/cd/ep/jp
  (cond [(symbol=? 'N attack) (attack-duration (list-ref (attack-sequence-normals attacks) (sub1 nc)))]
        ))

(define (generate-dmg-info char enemy attack nc)
  (let ([stats (calc-total-stats char)])
    ; un-lambda the attrs (should be runtime job)
    ; n/c/e/q/nj/nd/cj/cd/ep/jp
    (cond [(symbol=? 'N attack) (make-damage-info (calc-attr (attack-attr (list-ref (attack-sequence-normals (character-attacks char)) (sub1 nc))) stats)
                                                  ; should be in terms of total atk, not base
                                                  1 ; base-dmg-mult
                                                  0 ; base-add
                                                  1 ; dmg-mult
                                                  1 ; def-mult ; TODO
                                                  0.5 ; actual enemy res, figure out attack attr
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
      (* (percent-p modifier) (lookup stats (percent-attr modifier)))))

(define (lookup stats attr)
  (cond
    [(symbol=? attr 'atk%) (stat-info-atk stats)]
    ))

; do a tree traversal (abstract out, use symbols?) flat incr vs incr by other amt (atk (def% 50)) increases atk by 50% of base def
(define (calc-total-stats char)
  (make-stat-info (+ (* (character-hp char) ; base hp
                        (+ 1 #;hp%))
                     #;flat-hp)
                  (+ (* (+ (character-atk char) (weapon-atk (character-weapon char))) ; base atk
                        (+ 1 #;atk%))
                     #;flat-atk)
                  (+ (* (character-def char) ; base def
                        (+ 1 #;def%))
                     #;flat-def)
                  0 #;flat-em-sum
                  5 #;cr-sum
                  50 #;cd-sum))

(define (calc-single-dmg dmg)
  (* (+ (* (damage-info-base-dmg dmg) (damage-info-base-dmg-mult dmg)) (damage-info-base-add dmg))
     (damage-info-dmg-mult dmg)
     (damage-info-def-mult dmg)
     (calc-res (damage-info-res dmg))
     (damage-info-amp-mult dmg)
     (calc-crit (damage-info-amp-mult dmg) (damage-info-amp-mult dmg))))

(define (calc-res res)
  (cond [(< res 0) (- 1 (/ res 2))]
        [(>= res 0.75) (/ 1 (+ (* 4 res) 1))]
        [else (- 1 res)]))

(define (calc-crit crit-rate crit-dmg)
  (if (< (random) crit-rate)
      crit-dmg
      1))
