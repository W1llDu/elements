## elements: A DSL for optimizing and calculating Genshin Impact damage rotations

[Genshin Impact](https://genshin.hoyoverse.com/en/) is an open world RPG released in 2020. In Genshin, you control a team of up to four characters, and engage in various levels of combat.
The goal is typically to clear content as fast as possible, which requires efficient team rotations with high levels of sustained damage.
This package implements a damage calculator as a hosted DSL. With `elements`, you can easily define the basics of a team lineup, including weapons, elemental skills, and characters.
From there, you can setup damage rotations against enemies of varying toughness, and calculate potential damage output. Additionally, `elements` compares old rotations to newer ones,
allowing you to easily keep track of what combination has provided the highest sustained damage. 

Here is a simple example that defines two similar characters, two lineups, and runs different damage calculations on both. 

```
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
```
