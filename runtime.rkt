#lang racket

(define-struct character [hp atk def critr critd em attacks weapon skill burst artifacts])
(define-struct state-handler [character active-buffs time])
(define-struct weapon [damage substat buffs])
(define-struct skill [cd duration buffs])
(define-struct attack [attr duration])
(define-struct artifact [set-name main-stat substats])
(define-struct triggered-buff [effect trigger limit party-wide duration])
(define-struct unconditional-buff [effect party-wide])
(define-struct applied-buff [effect limit party-wide duration])
(define-struct attack-sequence [normals charged plunge])
(define-struct attribute [attr-func value])
(define-struct damage [attr duration elem-type])

(define-struct stat-info [hp def atk em critr critd])
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

(define (calc-dmg/acc team enemy attack-string nc dmg time)
  (cond [(empty? attack-string) (list dmg time)]
        [(cons? attack-string) (cond [(symbol=? 'S (first attack-string)) (calc-dmg/acc (foldr cons (list (first team)) (rest team))
                                                                                        enemy
                                                                                        (rest attack-string)
                                                                                        1
                                                                                        (+ time 1))]
                                     ; n/c/e/q/nj/nd/cj/cd/ep/jp
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
                                                              (calc-single (generate-dmg-info (first team)
                                                                                              enemy
                                                                                              (first attack-string)
                                                                                              nc)))
                                                           (+ time (calc-duration (character-attacks (first team)) (first attack-string) nc))))])]))

(define (calc-duration attacks attack nc)
  (cond [(symbol=? 'N attack) (attack-duration (list-ref (attack-sequence-normals attacks) nc))]
        ))

(define (generate-dmg-info char enemy attack nc)
  (let ([stats (calc-total-stats char)])
    ; un-lambda the attrs (should be runtime job)
    (cond [(symbol=? 'N attack) (make-damage-info ((attack-attr (list-ref (attack-sequence-normals (character-attacks char)) nc)) stats)
                                                  ; should be in terms of total atk, not base
                                                  1 ; base-dmg-mult
                                                  0 ; base-add
                                                  1 ; dmg-mult
                                                  1 ; def-mult ; TODO
                                                  1 ; enemy res, use attack-
                                                  1 ; amp-mult (reaction)
                                                  (stat-info-critr stats)
                                                  (stat-info-critd stats))]
          )))

; do a tree traversal (abstract out, use symbols?) flat incr vs incr by other amt (atk (def% 50)) increases atk by 50% of base def
(define (calc-total-stats char)
  (make-stat-info (+ (* char ; base hp
                        (+ 1 #;hp%))
                     #;flat-hp)
                  (+ (* char ; base def
                        (+ 1 #;def%))
                     #;flat-def)
                  (+ (* (+ char weapon) ; base atk
                        (+ 1 #;atk%))
                     #;flat-atk)
                  0 #;flat-em-sum
                  5
                  50))

(define (calc-single dmg)
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