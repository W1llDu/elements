#lang racket

(define-struct damage-info [base-dmg base-dmg-mult base-add
                                     dmg-mult
                                     def-mult
                                     res
                                     amp-mult
                                     crit-rate
                                     crit-dmg])
; base-dmg-mult: talent/constellation
; base-add: talent/constellation/weapon/artifact

; final output: dmg, time, dmg/s

(define (calc-dmg team enemy attack-string)
  (calc-dmg/acc team enemy attack-string 1 0))

(define (calc-dmg/acc team enemy attack-string nc time)
  (cond [(empty? attack-string) (list 0 0)]
        [(cons? attack-string) (cond [(symbol=? 'S (first attack-string)) (calc-dmg/acc (foldr cons (list (first team)) (rest team))
                                                                                        enemy
                                                                                        (rest attack-string)
                                                                                        1
                                                                                        (+ time 1))]
                                     [(n/c/e/q/nj/nd/ep/jp) (let ([res (calc-dmg/acc ...)])
                                                              (list (+ (first res) (calc-single (generate-dmg-info (first team)
                                                                                                                   enemy
                                                                                                                   (first attack-string)
                                                                                                                   nc))) ; make dmg info here
                                                                    (+ time (second res))))])]))

(define (generate-dmg-info char enemy attack nc)
  ...)

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