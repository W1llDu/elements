#lang racket

(require "main.rkt")

(genshin-calc

 (define-attack-sequence attack-chain
   ([(dmg atk% 10) 0.5 physical]
    [(dmg atk% 25) 0.2 physical]
    [(dmg atk% 125) 0.8 physical]
    [(dmg atk% 250) 1.5 physical]
    #:charged [(dmg hp% 5) 3.5 pyro]
    #:plunging [(dmg hp% 10) 3.5 physical]))


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
   #:attr (dmg atk% 125)
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
   #:attr (dmg atk% 25)
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
 (calculate-rotation-damage two-members dummy (E Q N N N N (Swap 1) N N N ND))
 (calculate-rotation-damage lone-member dummy (N N N N N N N N N N N N))
 )

;; (clear-data-file) ;; if the user wants to clear the data file

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Example + Expansion
#| (genshin-calc

 (define-attack-sequence attack-chain
   ([(dmg atk% 10) 0.5 pyro]
    [(dmg atk% 25) 0.2 hydro]
    [(dmg atk% 125) 0.8 pyro]
    [(dmg atk% 250) 1.5 hydro]
    #:charged [(dmg hp% 5) 3.5 pyro]
    #:plunging [(dmg hp% 10) 3.5 physical]))


 (define-weapon test-weapon
   450 ;; base attack stat
   (critr 24.1) ;; substat (crit rate)
   (triggered-buff
    [dmgup
     #:effect (buff atk% 20.0) ;; increase atk by 20%
     #:trigger normal-attack
     #:limit 1
     #:party-wide #t
     #:duration 10.0])

   (unconditional-buff
    [crit-up
     #:effect (buff critd 20) ;; increase crit damage by 20%
     #:party-wide #f])
   )

 ;; note : duration is optional
 (define-skill all-attack-up
   #:cooldown 25.0
   #:attr (dmg atk% 125)
   #:duration 0.1
   #:type hydro
   (applied-buff
    [skill-atkup
     #:effect (buff atk (hp% 50))
     #:limit 1
     #:party-wide #f
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
     #:duration 10.0]) ;; increase hp by 10% of atk,
   ;; only applies to current character
   )

 (define-skill basic-slash2
   #:cooldown 5.0 ;; cooldown
   #:attr (atk% 25)
   #:duration 1.0 ;; duration (where character cannot do anything else)
   #:type pyro
   (applied-buff
    [skill-hpup
     #:effect (hp 20)
     #:limit 1
     #:party-wide #f
     #:duration 10.0]) ;; increase hp by 10% of atk,
   ;; only applies to current character
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
   #:critr 25     ;; base crit rate
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

 #;(calculate-rotation-damage two-members dummy (E Q N N N N (Swap 1) N N N ND))
 ) |# 


#|

Example output (expansion): Compiled Code
|#

#;(begin

    (define attack-chain (make-attack-sequence (list (make-attack (make-attribute 'atk% 10) 0.5 'physical)
                                                     (make-attack (make-attribute 'atk% 25) 0.2 'physical)
                                                     (make-attack (make-attribute 'atk% 125) 0.8 'physical)
                                                     (make-attack (make-attribute 'atk% 250) 1.5 'physical))
                                               (make-attack (make-attribute 'hp% 5) 3.5 'pyro)
                                               (make-attack (make-attribute 'hp% 10) 3.5 'physical)))
    
    (define test-weapon
      (make-weapon 450 (make-attribute 'critr 24.1)
                   (list (make-triggered-buff (make-attribute 'atk% 20) 'normal-attack 1 #f 10)
                         (make-unconditional-buff (make-attribute 'critd 20) #f))))

    (define all-attack-up
      (make-skill 25 (make-attribute 'atk% 125) 0.1 'pyro
                  (list (make-applied-buff (make-attribute 'atk 125) 1 #t 10))))

    (define basic-slash
      (make-skill 5 (make-attribute 'atk% 25) 1.0 'pyro
                  (list (make-applied-buff (make-attribute 'hp 20) 1 #t 10))))

    (define test-feather
      (make-artifact "cool feather collection"
                     (make-attribute 'atk 375) (list (make-attribute 'atk 27)
                                                     (make-attribute 'em 42))))
    
    (define test-goblet
      (make-artifact "cool goblet collection"
                     (make-attribute 'critr 46.6) (list (make-attribute 'critd 16.2)
                                                        (make-attribute 'critr 3.0)
                                                        (make-attribute 'def 128))))
    
    (define test-char
      (make-character 12000
                      500
                      900
                      20
                      5
                      50
                      attack-chain
                      test-weapon
                      basic-slash
                      all-attack-up
                      (list test-feather test-goblet)))

    (define dummy
      (make-enemy 1000 (make-resistances 50 10 10 10 10 10 10 -20) 5))

    (define lone-member (list test-char))
)