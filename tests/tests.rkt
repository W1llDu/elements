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
                '(28094.508950860003 5.699999999999999)))

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
                '(300.6606644800001 0.5)))

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
  (check-equal? (calculate-raw-rotation-damage two-members dummy (C C C C)) '(1663.2528 14.0))
  ;; try with a buff
  (check-equal? (calculate-raw-rotation-damage two-members dummy (E Q C C C C)) '(3542.9017195 15.1))
  (check-equal? (calculate-raw-rotation-damage two-members dummy (N N N N)) '(12327.087243680002 3.0))
  ;; ND should lower time but decrease damage by a margin
  (check-equal? (calculate-raw-rotation-damage two-members dummy (ND ND ND ND)) '(1037.9806307200001 0.4))
  ;; swap?
  (check-equal? (calculate-raw-rotation-damage two-members dummy (N C N N N)) '(5527.044496160001 5.5))
  ;; 1 second longer since switching is 1 second
  (check-equal? (calculate-raw-rotation-damage two-members dummy (N C (Swap 2) N N N)) '(6177.45742416 6.5))
  ;; compare 2 characters side by side
  (check-equal? (calculate-raw-rotation-damage lone-member dummy (N N N N N)) '(12627.747908160001 3.5))
  (check-equal? (calculate-raw-rotation-damage lone-member-better dummy (N N N N N)) '(54126.552480000006 3.5))
  ;; increase attack
  (check-equal? (calculate-raw-rotation-damage lone-member-better dummy (Q N N N N N)) '(61517.528730000005 3.6))
  ;; enemy resistances
  (check-equal? (calculate-raw-rotation-damage lone-member tough-dummy (N N N N N)) '(2314.469924516129 3.5))
  (check-equal? (calculate-raw-rotation-damage lone-member-better tough-dummy (N N N N N)) '(9920.555806451614 3.5))
  ;; very simple calc
  (check-equal? (calculate-raw-rotation-damage lone-member-better default-dummy (N)) '(1171.5704 0.5))
  )




