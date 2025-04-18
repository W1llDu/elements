#lang racket

(require "Genshin-DSL-impl.rkt")

(genshin-calc

 (define-attack-sequence attack-chain
   ([(atk% 10) 0.5 physical]
    [(atk% 25) 0.2 physical]
    [(atk% 125) 0.8 physical]
    [(atk% 250) 1.5 physical]
    #:charged [(hp% 5) 3.5 pyro]
    #:plunging [(hp% 10) 3.5 physical]))


 (define-weapon test-weapon
   450 ;; base attack stat
   (critr 24.1) ;; substat (crit rate)
   (triggered-buff
    [dmgup
     #:effect (atk% 20.0) ;; increase atk by 20%
     #:trigger normal-attack
     #:limit 1
     #:party-wide #f
     #:duration 10.0])

   (unconditional-buff
    [crit-up
     #:effect (critd 20) ;; increase crit damage by 20%
     #:party-wide #f])
   )

 ;; note : duration is optional
 (define-skill all-attack-up
   #:cooldown 25.0
   #:attr (atk% 125)
   #:duration 0.1
   #:type pyro
   (applied-buff
    [skill-atkup
     #:effect (atk 125)
     #:limit 1
     #:party-wide #t
     #:duration 10]) ;; this time applies to all members of a lineup
   )

 (define-skill basic-slash
   #:cooldown 5.0 ;; cooldown
   #:attr (atk% 25)
   #:duration 1.0 ;; duration (where character cannot do anything else)
   #:type pyro
   (applied-buff
    [skill-hpup
     #:effect (hp 20)
     #:limit 1
     #:party-wide #f
     #:duration 10.0]) ;; increase hp by 10% of atk, only applies to current character
   )

 (define-artifact test-feather
   "cool feather collection" ;; set name
   (atk 325) ;; main stat
   (atk 27) ;; substats
   (em 42)
   )

 (define-artifact test-goblet
   "cool goblet collection" ;; set name
   (critr 46.6) ;; main stat
   (critd 16.2) ;; substats
   (critr 3.0)
   (def 128)
   )

 (define-character test-char
   #:hp 12000 ;; base hp
   #:def 500   ;; base def
   #:atk 900   ;; base atk
   #:em 20    ;; base em
   #:critr 5     ;; base crit rate
   #:critd 50    ;; base crit damage
   #:attacks attack-chain
   #:weapon test-weapon ;; weapon
   #:skill basic-slash ;; skill
   #:burst all-attack-up ;; burst
   #:artifacts test-feather
   test-goblet
   )

 (define-character test-char2
   #:hp 12000 ;; base hp
   #:def 500   ;; base def
   #:atk 900   ;; base atk
   #:em 20    ;; base em
   #:critr 25     ;; base crit rate
   #:critd 50    ;; base crit damage
   #:attacks attack-chain
   #:weapon test-weapon ;; weapon
   #:skill basic-slash ;; skill
   #:burst all-attack-up ;; burst
   #:artifacts test-feather
   test-goblet
   )

 (define-enemy dummy
   #|#:type Pyro|# ; for reactions
   #:def 1000
   #:res (#:pyro 50
          #:hydro 10
          #:electro 10
          #:cryo 10
          #:geo 10
          #:anemo 10
          #:dendro 10
          #:physical -20)
   #:reduction 5
   )

 (define-team-lineup two-members (test-char test-char2))
 (define-team-lineup lone-member (test-char))
 (calculate-rotation-damage two-members dummy (E Q N N N N (Swap 1) N N N ND))
 (calculate-rotation-damage lone-member dummy (N N N N N N N N N N N N))
 )

;; (clear-data-file) ;; if the user wants to clear the data file

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(module+ test
  (require rackunit syntax/macro-testing)
  (check-exn #rx"define-attack-sequence: expected base-stat"
             (λ () (convert-compile-time-error
                    (genshin-calc

                     (define-attack-sequence attack-chain
                       ([(atk% 10) 0.5 physical]
                        [(atk% 25) 0.2 physical]
                        [(atk% 125) 0.8 physical]
                        [(atk% 250) 1.5 physical]
                        #:charged [(hp 5) 3.5 pyro]
                        #:plunging [(hp 10) 3.5 physical]))


                     (define-weapon test-weapon
                       450 ;; base attack stat
                       (critr 24.1) ;; substat (crit rate)
                       (triggered-buff
                        [dmgup
                         #:effect (atk% 20.0) ;; increase atk by 20%
                         #:trigger normal-attack
                         #:limit 1
                         #:party-wide #f
                         #:duration 10.0])

                       (unconditional-buff
                        [crit-up
                         #:effect (critd 20) ;; increase crit damage by 20%
                         #:party-wide #f])
                       )

                     ;; note : duration is optional
                     (define-skill all-attack-up
                       #:cooldown 25.0
                       #:attr (atk% 125)
                       #:duration 0.1
                       #:type pyro
                       (applied-buff
                        [skill-atkup
                         #:effect (atk 125)
                         #:limit 1
                         #:party-wide #t
                         #:duration 10]) ;; this time applies to all members of a lineup
                       )

                     (define-skill basic-slash
                       #:cooldown 5.0 ;; cooldown
                       #:attr (atk% 25)
                       #:duration 1.0 ;; duration (where character cannot do anything else)
                       #:type pyro
                       (applied-buff
                        [skill-hpup
                         #:effect (hp 20)
                         #:limit 1
                         #:party-wide #f
                         #:duration 10.0]) ;; increase hp by 10% of atk, only applies to current character
                       )

                     (define-artifact test-feather
                       "cool feather collection" ;; set name
                       (atk 325) ;; main stat
                       (atk 27) ;; substats
                       (em 42)
                       )

                     (define-artifact test-goblet
                       "cool goblet collection" ;; set name
                       (critr 46.6) ;; main stat
                       (critd 16.2) ;; substats
                       (critr 3.0)
                       (def 128)
                       )

                     (define-character test-char
                       #:hp 12000 ;; base hp
                       #:def 500   ;; base def
                       #:atk 900   ;; base atk
                       #:em 20    ;; base em
                       #:critr 5     ;; base crit rate
                       #:critd 50    ;; base crit damage
                       #:attacks attack-chain
                       #:weapon test-weapon ;; weapon
                       #:skill basic-slash ;; skill
                       #:burst all-attack-up ;; burst
                       #:artifacts test-feather
                       all-attack-up ; not an artifact
                       )
                     ))))
             
  (check-exn #rx"define-weapon: a duplicate name was found"
             (λ () (convert-compile-time-error
                    (genshin-calc

                     (define-attack-sequence attack-chain
                       ([(atk% 10) 0.5 physical]
                        [(atk% 25) 0.2 physical]
                        [(atk% 125) 0.8 physical]
                        [(atk% 250) 1.5 physical]
                        #:charged [(hp% 5) 3.5 pyro]
                        #:plunging [(hp% 10) 3.5 physical]))


                     (define-weapon attack-chain
                       450 ;; base attack stat
                       (critr 24.1) ;; substat (crit rate)
                       (triggered-buff
                        [dmgup
                         #:effect (atk% 20.0) ;; increase atk by 20%
                         #:trigger 'normal-attack
                         #:limit 1
                         #:party-wide #f
                         #:duration 10.0]))
                     ))))

  (check-equal? (genshin-calc

                               (define-attack-sequence attack-chain
                                 ([(atk% 10) 0.5 physical]
                                  [(atk% 25) 0.2 physical]
                                  [(atk% 125) 0.8 physical]
                                  [(atk% 250) 1.5 physical]
                                  #:charged [(hp% 5) 3.5 pyro]
                                  #:plunging [(hp% 10) 3.5 physical]))


                               (define-weapon test-weapon
                                 450 ;; base attack stat
                                 (critr 24.1) ;; substat (crit rate)
                                 (triggered-buff 
                                  [dmgup
                                   #:effect (atk% 20.0) ;; increase atk by 20%
                                   #:trigger normal-attack
                                   #:limit 1
                                   #:party-wide #f
                                   #:duration 10.0])

                                 (unconditional-buff
                                  [crit-up
                                   #:effect (critd 20) ;; increase crit damage by 20%
                                   #:party-wide #f])
                                 )

                               ;; note : duration is optional
                               (define-skill all-attack-up
                                 #:cooldown 25.0
                                 #:attr (atk% 125)
                                 #:duration 0.1
                                 #:type pyro
                                 (applied-buff
                                  [skill-atkup
                                   #:effect (atk 125)
                                   #:limit 1
                                   #:party-wide #t
                                   #:duration 10]) ;; this time applies to all members of a lineup
                                 )

                               (define-skill basic-slash
                                 #:cooldown 5.0 ;; cooldown
                                 #:attr (atk% 25)
                                 #:duration 1.0 ;; duration (where character cannot do anything else)
                                 #:type pyro
                                 (applied-buff
                                  [skill-hpup
                                   #:effect (hp 20)
                                   #:limit 1
                                   #:party-wide #f
                                   #:duration 10.0]) ;; increase hp by 10% of atk, only applies to current character
                                 )

                               (define-artifact test-feather
                                 "cool feather collection" ;; set name
                                 (atk 325) ;; main stat
                                 (atk 27) ;; substats
                                 (em 42)
                                 )

                               (define-artifact test-goblet
                                 "cool goblet collection" ;; set name
                                 (critr 46.6) ;; main stat
                                 (critd 16.2) ;; substats
                                 (critr 3.0)
                                 (def 128)
                                 )


                               (define-character test-char
                                 #:hp 12000 ;; base hp
                                 #:def 500   ;; base def
                                 #:atk 900   ;; base atk
                                 #:em 20    ;; base em
                                 #:critr 5     ;; base crit rate
                                 #:critd 50    ;; base crit damage
                                 #:attacks attack-chain
                                 #:weapon test-weapon ;; weapon
                                 #:skill basic-slash ;; skill
                                 #:burst all-attack-up ;; burst
                                 #:artifacts test-feather
                                 test-goblet
                                 )

                               (define-character test-char2
                                 #:hp 12000 ;; base hp
                                 #:def 500   ;; base def
                                 #:atk 900   ;; base atk
                                 #:em 20    ;; base em
                                 #:critr 25     ;; base crit rate
                                 #:critd 50    ;; base crit damage
                                 #:attacks attack-chain
                                 #:weapon test-weapon ;; weapon
                                 #:skill basic-slash ;; skill
                                 #:burst all-attack-up ;; burst
                                 #:artifacts test-feather
                                 test-goblet
                                 )

                               (define-enemy dummy
                                 #|#:type Pyro|# ; for reactions
                                 #:def 1000
                                 #:res (#:pyro 50
                                        #:hydro 10
                                        #:electro 10
                                        #:cryo 10
                                        #:geo 10
                                        #:anemo 10
                                        #:dendro 10
                                        #:physical -20)
                                 #:reduction 5
                                 )

                               (define-team-lineup two-members (test-char test-char2))
                               (define-team-lineup lone-member (test-char))
                               (calculate-raw-rotation-damage two-members dummy (E Q N N N N (Swap 1) N N N ND)))
                '(26081.90330288 5.699999999999999)))

(module+ test
  (require rackunit syntax/macro-testing) 
  (check-exn #rx"define-character: not bound as artifact"
             (λ () (convert-compile-time-error
                    (genshin-calc

                     (define-attack-sequence attack-chain
                       ([(atk% 10) 0.5 physical]
                        [(atk% 25) 0.2 physical]
                        [(atk% 125) 0.8 physical]
                        [(atk% 250) 1.5 physical]
                        #:charged [(hp% 5) 3.5 pyro]
                        #:plunging [(hp% 10) 3.5 physical]))


                     (define-weapon test-weapon 
                       450 ;; base attack stat
                       (critr 24.1) ;; substat (crit rate)
                       (triggered-buff
                        [dmgup
                         #:effect (atk% 20.0) ;; increase atk by 20%
                         #:trigger normal-attack
                         #:limit 1
                         #:party-wide #f
                         #:duration 10.0])

                       (unconditional-buff
                        [crit-up
                         #:effect (critd 20) ;; increase crit damage by 20%
                         #:party-wide #f])
                       )

                     ;; note : duration is optional
                     (define-skill all-attack-up
                       #:cooldown 25.0
                       #:attr (atk% 125)
                       #:duration 0.1
                       #:type pyro
                       (applied-buff
                        [skill-atkup
                         #:effect (atk 125)
                         #:limit 1
                         #:party-wide #t
                         #:duration 10]) ;; this time applies to all members of a lineup
                       )

                     (define-skill basic-slash
                       #:cooldown 5.0 ;; cooldown
                       #:attr (atk% 25)
                       #:duration 1.0 ;; duration (where character cannot do anything else)
                       #:type pyro
                       (applied-buff
                        [skill-hpup
                         #:effect (hp 20)
                         #:limit 1
                         #:party-wide #f
                         #:duration 10.0]) ;; increase hp by 10% of atk, only applies to current character
                       )

                     (define-artifact test-feather
                       "cool feather collection" ;; set name
                       (atk 325) ;; main stat
                       (atk 27) ;; substats
                       (em 42)
                       )

                     (define-artifact test-goblet
                       "cool goblet collection" ;; set name
                       (critr 46.6) ;; main stat
                       (critd 16.2) ;; substats
                       (critr 3.0)
                       (def 128)
                       )

                     (define-character test-char
                       #:hp 12000 ;; base hp
                       #:def 500   ;; base def
                       #:atk 900   ;; base atk
                       #:em 20    ;; base em
                       #:critr 5     ;; base crit rate
                       #:critd 50    ;; base crit damage
                       #:attacks attack-chain
                       #:weapon test-weapon ;; weapon
                       #:skill basic-slash ;; skill
                       #:burst all-attack-up ;; burst
                       #:artifacts test-feather
                       all-attack-up ; not an artifact
                       )
                     ))))
             
  (check-exn #rx"define-weapon: a duplicate name was found"
             (λ () (convert-compile-time-error
                    (genshin-calc

                     (define-attack-sequence attack-chain
                       ([(atk% 10) 0.5 physical]
                        [(atk% 25) 0.2 physical]
                        [(atk% 125) 0.8 physical]
                        [(atk% 250) 1.5 physical]
                        #:charged [(hp 5) 3.5 pyro]
                        #:plunging [(hp 10) 3.5 physical]))


                     (define-weapon attack-chain
                       450 ;; base attack stat
                       (critr 24.1) ;; substat (crit rate)
                       (triggered-buff
                        [dmgup
                         #:effect (atk% 20.0) ;; increase atk by 20%
                         #:trigger 'normal-attack
                         #:limit 1
                         #:party-wide #f
                         #:duration 10.0]))
                     ))))

  (check-equal? (begin ((λ () (genshin-calc

                               (define-attack-sequence attack-chain
                                 ([(atk% 10) 0.5 physical]
                                  [(atk% 25) 0.2 physical]
                                  [(atk% 125) 0.8 physical]
                                  [(atk% 250) 1.5 physical]
                                  #:charged [(hp% 5) 3.5 pyro]
                                  #:plunging [(hp% 10) 3.5 physical]))


                               (define-weapon test-weapon
                                 450 ;; base attack stat
                                 (critr 24.1) ;; substat (crit rate)
                                 (triggered-buff
                                  [dmgup
                                   #:effect (atk% 20.0) ;; increase atk by 20%
                                   #:trigger normal-attack
                                   #:limit 1
                                   #:party-wide #f
                                   #:duration 10.0])

                                 (unconditional-buff
                                  [crit-up
                                   #:effect (critd 20) ;; increase crit damage by 20%
                                   #:party-wide #f])
                                 )

                               ;; note : duration is optional
                               (define-skill all-attack-up
                                 #:cooldown 25.0
                                 #:attr (atk% 125)
                                 #:duration 0.1
                                 #:type pyro
                                 (applied-buff
                                  [skill-atkup
                                   #:effect (atk 125)
                                   #:limit 1
                                   #:party-wide #t
                                   #:duration 10]) ;; this time applies to all members of a lineup
                                 )

                               (define-skill basic-slash
                                 #:cooldown 5.0 ;; cooldown
                                 #:attr (atk% 25)
                                 #:duration 1.0 ;; duration (where character cannot do anything else)
                                 #:type pyro
                                 (applied-buff
                                  [skill-hpup
                                   #:effect (hp 20)
                                   #:limit 1
                                   #:party-wide #f
                                   #:duration 10.0]) ;; increase hp by 10% of atk, only applies to current character
                                 )

                               (define-artifact test-feather
                                 "cool feather collection" ;; set name
                                 (atk 325) ;; main stat
                                 (atk 27) ;; substats
                                 (em 42)
                                 )

                               (define-artifact test-goblet
                                 "cool goblet collection" ;; set name
                                 (critr 46.6) ;; main stat
                                 (critd 16.2) ;; substats
                                 (critr 3.0)
                                 (def 128)
                                 )


                               (define-character test-char
                                 #:hp 12000 ;; base hp
                                 #:def 500   ;; base def
                                 #:atk 900   ;; base atk
                                 #:em 20    ;; base em
                                 #:critr 5     ;; base crit rate
                                 #:critd 50    ;; base crit damage
                                 #:attacks attack-chain
                                 #:weapon test-weapon ;; weapon
                                 #:skill basic-slash ;; skill
                                 #:burst all-attack-up ;; burst
                                 #:artifacts test-feather
                                 test-goblet
                                 )

                               (define-character test-char2
                                 #:hp 12000 ;; base hp
                                 #:def 500   ;; base def
                                 #:atk 900   ;; base atk
                                 #:em 20    ;; base em
                                 #:critr 25     ;; base crit rate
                                 #:critd 50    ;; base crit damage
                                 #:attacks attack-chain
                                 #:weapon test-weapon ;; weapon
                                 #:skill basic-slash ;; skill
                                 #:burst all-attack-up ;; burst
                                 #:artifacts test-feather
                                 test-goblet
                                 )

                               (define-enemy dummy
                                 #|#:type Pyro|# ; for reactions
                                 #:def 1000
                                 #:res (#:pyro 50
                                        #:hydro 10
                                        #:electro 10
                                        #:cryo 10
                                        #:geo 10
                                        #:anemo 10
                                        #:dendro 10
                                        #:physical -20)
                                 #:reduction 5
                                 )

                               (define-team-lineup two-members (test-char test-char2))
                               (define-team-lineup lone-member (test-char))
                               (calculate-raw-rotation-damage two-members dummy (N))))))
                '(279.12224384 0.5)))