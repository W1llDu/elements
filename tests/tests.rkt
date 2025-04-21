#lang racket

(require "../main.rkt")

(module+ test
  (require rackunit syntax/macro-testing)
  (check-exn #rx"define-character: not bound as artifact"
             (λ () (convert-compile-time-error
                    (genshin-calc

                     (define-attack-sequence attack-chain
                       ([(base-dmg atk% 10) 0.5 physical]
                        [(base-dmg atk% 25) 0.2 physical]
                        [(base-dmg atk% 125) 0.8 physical]
                        [(base-dmg atk% 250) 1.5 physical]
                        #:charged [(base-dmg hp% 5) 3.5 pyro]
                        #:plunging [(base-dmg hp% 10) 3.5 physical]))

                     (define-weapon test-weapon
                       450 ;; base attack stat
                       (mod critr 24.1) ;; substat (crit rate)
                       (triggered-buff
                        [dmgup
                         #:effect (buff atk% 20.0) ;; increase atk by 20%
                         #:trigger normal-attack
                         #:limit 1
                         #:party-wide #f
                         #:duration 10.0])

                       (unconditional-buff
                        [crit-up
                         #:effect (buff critd 20) ;; increase crit damage by 20%
                         #:party-wide #f])
                       )

                     ;; note : duration is optional
                     (define-skill all-attack-up
                       #:cooldown 25.0
                       #:attr (base-dmg atk% 125)
                       #:duration 0.1
                       #:type pyro
                       (applied-buff
                        [skill-atkup
                         #:effect (buff atk 125)
                         #:limit 1
                         #:party-wide #t
                         #:duration 10]) ;; this time applies to all members of a lineup
                       )

                     (define-skill basic-slash
                       #:cooldown 5.0 ;; cooldown
                       #:attr (base-dmg atk% 25)
                       #:duration 1.0 ;; duration (where character cannot do anything else)
                       #:type pyro
                       (applied-buff
                        [skill-hpup
                         #:effect (buff hp 20)
                         #:limit 1
                         #:party-wide #f
                         #:duration 10.0]) ;; increase hp by 10% of atk, only applies to current character
                       )

                     (define-artifact test-feather
                       "cool feather collection" ;; set name
                       (mod atk 325) ;; main stat
                       (mod atk 27) ;; substats
                       (mod em 42)
                       )

                     (define-artifact test-goblet
                       "cool goblet collection" ;; set name
                       (mod critr 46.6) ;; main stat
                       (mod critd 16.2) ;; substats
                       (mod critr 3.0)
                       (mod def 128)
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
                       (mod critr 24.1) ;; substat (crit rate)
                       (triggered-buff
                        [base-dmgup
                         #:effect (buff atk% 20.0) ;; increase atk by 20%
                         #:trigger 'normal-attack
                         #:limit 1
                         #:party-wide #f
                         #:duration 10.0]))
                     ))))

  (check-equal? (genshin-calc

                 (define-attack-sequence attack-chain
                   ([(base-dmg atk% 10) 0.5 physical]
                    [(base-dmg atk% 25) 0.2 physical]
                    [(base-dmg atk% 125) 0.8 physical]
                    [(base-dmg atk% 250) 1.5 physical]
                    #:charged [(base-dmg hp% 5) 3.5 pyro]
                    #:plunging [(base-dmg hp% 10) 3.5 physical]))


                 (define-weapon test-weapon
                   450 ;; base attack stat
                   (mod critr 24.1) ;; substat (crit rate)
                   (triggered-buff 
                    [dmgup
                     #:effect (buff atk% 20.0) ;; increase atk by 20%
                     #:trigger normal-attack
                     #:limit 1
                     #:party-wide #f
                     #:duration 10.0])

                   (unconditional-buff
                    [crit-up
                     #:effect (buff critd 20) ;; increase crit damage by 20%
                     #:party-wide #f])
                   )

                 ;; note : duration is optional
                 (define-skill all-attack-up
                   #:cooldown 25.0
                   #:attr (base-dmg atk% 125)
                   #:duration 0.1
                   #:type pyro
                   (applied-buff
                    [skill-atkup
                     #:effect (buff atk 125)
                     #:limit 1
                     #:party-wide #t
                     #:duration 10]) ;; this time applies to all members of a lineup
                   )

                 (define-skill basic-slash
                   #:cooldown 5.0 ;; cooldown
                   #:attr (base-dmg atk% 25)
                   #:duration 1.0 ;; duration (where character cannot do anything else)
                   #:type pyro
                   (applied-buff
                    [skill-hpup
                     #:effect (buff hp 20)
                     #:limit 1
                     #:party-wide #f
                     #:duration 10.0]) ;; increase hp by 10% of atk, only applies to current character
                   )

                 (define-artifact test-feather
                   "cool feather collection" ;; set name
                   (mod atk 325) ;; main stat
                   (mod atk 27) ;; substats
                   (mod em 42)
                   )

                 (define-artifact test-goblet
                   "cool goblet collection" ;; set name
                   (mod critr 46.6) ;; main stat
                   (mod critd 16.2) ;; substats
                   (mod critr 3.0)
                   (mod def 128)
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
                '(34020.31627861 5.699999999999999)))

(module+ test
  (require rackunit syntax/macro-testing) 
  (check-exn #rx"define-character: not bound as artifact"
             (λ () (convert-compile-time-error
                    (genshin-calc

                     (define-attack-sequence attack-chain
                       ([(base-dmg atk% 10) 0.5 physical]
                        [(base-dmg atk% 25) 0.2 physical]
                        [(base-dmg atk% 125) 0.8 physical]
                        [(base-dmg atk% 250) 1.5 physical]
                        #:charged [(base-dmg hp% 5) 3.5 pyro]
                        #:plunging [(base-dmg hp% 10) 3.5 physical]))


                     (define-weapon test-weapon 
                       450 ;; base attack stat
                       (mod critr 24.1) ;; substat (crit rate)
                       (triggered-buff
                        [dmgup
                         #:effect (buff atk% 20.0) ;; increase atk by 20%
                         #:trigger normal-attack
                         #:limit 1
                         #:party-wide #f
                         #:duration 10.0])

                       (unconditional-buff
                        [crit-up
                         #:effect (buff critd 20) ;; increase crit damage by 20%
                         #:party-wide #f])
                       )

                     ;; note : duration is optional
                     (define-skill all-attack-up
                       #:cooldown 25.0
                       #:attr (base-dmg atk% 125)
                       #:duration 0.1
                       #:type pyro
                       (applied-buff
                        [skill-atkup
                         #:effect (buff atk 125)
                         #:limit 1
                         #:party-wide #t
                         #:duration 10]) ;; this time applies to all members of a lineup
                       )

                     (define-skill basic-slash
                       #:cooldown 5.0 ;; cooldown
                       #:attr (base-dmg atk% 25)
                       #:duration 1.0 ;; duration (where character cannot do anything else)
                       #:type pyro
                       (applied-buff
                        [skill-hpup
                         #:effect (buff hp 20)
                         #:limit 1
                         #:party-wide #f
                         #:duration 10.0]) ;; increase hp by 10% of atk, only applies to current character
                       )

                     (define-artifact test-feather
                       "cool feather collection" ;; set name
                       (mod atk 325) ;; main stat
                       (mod atk 27) ;; substats
                       (mod em 42)
                       )

                     (define-artifact test-goblet
                       "cool goblet collection" ;; set name
                       (mod critr 46.6) ;; main stat
                       (mod critd 16.2) ;; substats
                       (mod critr 3.0)
                       (mod def 128)
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
                         #:effect (buff atk% 20.0) ;; increase atk by 20%
                         #:trigger 'normal-attack
                         #:limit 1
                         #:party-wide #f
                         #:duration 10.0]))
                     ))))

  (check-equal? (begin ((λ () (genshin-calc

                               (define-attack-sequence attack-chain
                                 ([(base-dmg atk% 10) 0.5 physical]
                                  [(base-dmg atk% 25) 0.2 physical]
                                  [(base-dmg atk% 125) 0.8 physical]
                                  [(base-dmg atk% 250) 1.5 physical]
                                  #:charged [(base-dmg hp% 5) 3.5 pyro]
                                  #:plunging [(base-dmg hp% 10) 3.5 physical]))


                               (define-weapon test-weapon
                                 450 ;; base attack stat
                                 (mod critr 24.1) ;; substat (crit rate)
                                 (triggered-buff
                                  [dmgup
                                   #:effect (buff atk% 20.0) ;; increase atk by 20%
                                   #:trigger normal-attack
                                   #:limit 1
                                   #:party-wide #f
                                   #:duration 10.0])

                                 (unconditional-buff
                                  [crit-up
                                   #:effect (buff critd 20) ;; increase crit damage by 20%
                                   #:party-wide #f])
                                 )

                               ;; note : duration is optional
                               (define-skill all-attack-up
                                 #:cooldown 25.0
                                 #:attr (base-dmg atk% 125)
                                 #:duration 0.1
                                 #:type pyro
                                 (applied-buff
                                  [skill-atkup
                                   #:effect (buff atk 125)
                                   #:limit 1
                                   #:party-wide #t
                                   #:duration 10]) ;; this time applies to all members of a lineup
                                 )

                               (define-skill basic-slash
                                 #:cooldown 5.0 ;; cooldown
                                 #:attr (base-dmg atk% 25)
                                 #:duration 1.0 ;; duration (where character cannot do anything else)
                                 #:type pyro
                                 (applied-buff
                                  [skill-hpup
                                   #:effect (buff hp 20)
                                   #:limit 1
                                   #:party-wide #f
                                   #:duration 10.0]) ;; increase hp by 10% of atk, only applies to current character
                                 )

                               (define-artifact test-feather
                                 "cool feather collection" ;; set name
                                 (mod atk 325) ;; main stat
                                 (mod atk 27) ;; substats
                                 (mod em 42)
                                 )

                               (define-artifact test-goblet
                                 "cool goblet collection" ;; set name
                                 (mod critr 46.6) ;; main stat
                                 (mod critd 16.2) ;; substats
                                 (mod critr 3.0)
                                 (mod def 128)
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
                '(364.07722648000004 0.5)))

(module+ test
  (require rackunit syntax/macro-testing)
  ;; done without genshin calc since we do not need to worry
  ;; about compile time errors here
  (define-attack-sequence attack-chain
    ([(base-dmg atk% 10) 0.5 physical]
     [(base-dmg atk% 25) 0.2 physical]
     [(base-dmg atk% 125) 0.8 physical]
     [(base-dmg atk% 250) 1.5 physical]
     #:charged [(base-dmg hp% 5) 3.5 pyro]
     #:plunging [(base-dmg hp% 10) 3.5 physical]))

  (define-attack-sequence attack-chain2
    ([(base-dmg atk% 100) 0.5 physical]
     [(base-dmg atk% 100) 0.2 physical]
     [(base-dmg atk% 100) 0.8 physical]
     [(base-dmg atk% 100) 1.5 physical]
     #:charged [(base-dmg hp% 100) 3.5 pyro]
     #:plunging [(base-dmg hp% 100) 3.5 physical]))


   (define-weapon test-weapon
     450 ;; base attack stat
     (mod critr 24.1) ;; substat (crit rate)
     (triggered-buff
      [dmgup
       #:effect (buff atk% 20.0) ;; increase atk by 20%
       #:trigger normal-attack
       #:limit 1
       #:party-wide #f
       #:duration 10.0])

     (unconditional-buff
      [crit-up
       #:effect (buff critd 20) ;; increase crit damage by 20%
       #:party-wide #f])
     )

  (define-weapon test-weapon2
     0 ;; base attack stat
     (mod critr 0) ;; substat (crit rate)
    )

   ;; note : duration is optional
   (define-skill all-attack-up
     #:cooldown 25.0
     #:attr (base-dmg atk% 125)
     #:duration 0.1
     #:type pyro
     (applied-buff
      [skill-atkup
       #:effect (buff atk 125)
       #:limit 1
       #:party-wide #t
       #:duration 10]) ;; this time applies to all members of a lineup
     )

   (define-skill basic-slash
     #:cooldown 5.0 ;; cooldown
     #:attr (base-dmg atk% 25)
     #:duration 1.0 ;; duration (where character cannot do anything else)
     #:type pyro
     (applied-buff
      [skill-hpup
       #:effect (buff hp 20)
       #:limit 1
       #:party-wide #f
       #:duration 10.0]) ;; increase hp by 10% of atk, only applies to current character
     )

   (define-artifact test-feather
     "cool feather collection" ;; set name
     (mod atk 325) ;; main stat
     (mod atk 27) ;; substats
     (mod em 42)
     )

   (define-artifact test-goblet
     "cool goblet collection" ;; set name
     (mod critr 46.6) ;; main stat
     (mod critd 16.2) ;; substats
     (mod critr 3.0)
     (mod def 128)
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

  ;; like char1, but better
  (define-character test-char3
     #:hp 120000 ;; base hp
     #:def 5000   ;; base def
     #:atk 4500   ;; base atk
     #:em 200    ;; base em
     #:critr 50     ;; base crit rate
     #:critd 500    ;; base crit damage
     #:attacks attack-chain
     #:weapon test-weapon ;; weapon
     #:skill basic-slash ;; skill
     #:burst all-attack-up ;; burst
     #:artifacts test-feather
     test-goblet
     )

  ;; manual calculations
  (define-character test-char4
     #:hp 3000 ;; base hp
     #:def 500   ;; base def
     #:atk 1000   ;; base atk
     #:em 20    ;; base em
     #:critr 100     ;; base crit rate
     #:critd 100    ;; base crit damage
     #:attacks attack-chain2
     #:weapon test-weapon2 ;; weapon
     #:skill basic-slash ;; skill
     #:burst all-attack-up ;; burst
     #:artifacts)

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

  (define-enemy tough-dummy
    #|#:type Pyro|# ; for reactions
    #:def 5000
    #:res (#:pyro 99
           #:hydro 99
           #:electro 99
           #:cryo 99
           #:geo 99
           #:anemo 99
           #:dendro 99
           #:physical 99)
    #:reduction 99
    )

  (define-enemy default-dummy
    #|#:type Pyro|# ; for reactions
    #:def 0
    #:res (#:pyro 0
           #:hydro 0
           #:electro 0
           #:cryo 0
           #:geo 0
           #:anemo 0
           #:dendro 0
           #:physical 0)
    #:reduction 0
    )

  (define-team-lineup two-members (test-char test-char2))
  (define-team-lineup lone-member (test-char))
  (define-team-lineup lone-member-better (test-char3))
  (define-team-lineup lone-member-easy (test-char4))
  (check-equal? (calculate-raw-rotation-damage two-members dummy (C C C C)) '(2014.0728 14.0))
  ;; try with a buff
  (check-equal? (calculate-raw-rotation-damage two-members dummy (E Q C C C C)) '(4290.184863249999 15.1))
  (check-equal? (calculate-raw-rotation-damage two-members dummy (N N N N)) '(14927.16628568 3.0))
  ;; ND should lower time but decrease damage by a margin
  (check-equal? (calculate-raw-rotation-damage two-members dummy (ND ND ND ND)) '(1256.9156987200001 0.4))
  ;; swap?
  (check-equal? (calculate-raw-rotation-damage two-members dummy (N C N N N)) '(6692.831050159999 5.5))
  ;; 1 second longer since switching is 1 second
  (check-equal? (calculate-raw-rotation-damage two-members dummy (N C (Swap 2) N N N)) '(7291.18317816 6.5))
  ;; compare 2 characters side by side
  (check-equal? (calculate-raw-rotation-damage lone-member dummy (N N N N N)) '(15291.24351216 3.5))
  (check-equal? (calculate-raw-rotation-damage lone-member-better dummy (N N N N N)) '(184937.23247999998 3.5))
  ;; increase attack
  (check-equal? (calculate-raw-rotation-damage lone-member-better dummy (Q N N N N N)) '(210190.39623 3.6))
  ;; enemy resistances
  (check-equal? (calculate-raw-rotation-damage lone-member tough-dummy (N N N N N)) '(2802.6472712903224 3.5))
  (check-equal? (calculate-raw-rotation-damage lone-member-better tough-dummy (N N N N N)) '(33896.120322580646 3.5))
  ;; very simple calc
  (check-equal? (calculate-raw-rotation-damage lone-member-better default-dummy (N)) '(4002.9704 0.5))
  
  ;; manual calculation tests
  (check-equal? (calculate-raw-rotation-damage lone-member-easy default-dummy (N)) '(2000.0 0.5))
  (check-equal? (calculate-raw-rotation-damage lone-member-easy default-dummy (N N N N N)) '(10000.0 3.5))
  (check-equal? (calculate-raw-rotation-damage lone-member-easy default-dummy (C C C)) '(18000.0 10.5))
  ;; fine... off by like 1 trillionth. This is why we chose to round when displaying.
  (check-equal? (calculate-raw-rotation-damage lone-member-easy default-dummy (C C C ND ND ND)) '(24000.0 10.799999999999999))
  (check-equal? (calculate-raw-rotation-damage lone-member-easy default-dummy (Q)) '(2812.5 0.1))
  )




