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
   ([(base-dmg atk% 10) 0.5 physical]
    [(base-dmg atk% 25) 0.2 physical]
    [(base-dmg atk% 125) 0.8 physical]
    [(base-dmg atk% 250) 1.5 physical]
    #:charged [(base-dmg hp% 5) 3.5 pyro]
    #:plunging [(base-dmg hp% 10) 3.5 physical]))

 (define-weapon wolfs-gravestone
   450 
   (mod critr 24.1)
   (triggered-buff
    [dmgup
     #:effect (buff atk% 20.0) 
     #:trigger normal-attack
     #:limit 1
     #:party-wide #f
     #:duration 10.0])

   (unconditional-buff
    [crit-up
     #:effect (buff critd 20) 
     #:party-wide #f])
   )

 (define-weapon vitality-staff
   674 
   (mod em 1000)

   (unconditional-buff
    [crit-up
     #:effect (buff critd 20) 
     #:party-wide #f])

   (unconditional-buff
    [crit-up
     #:effect (buff hp% 50) 
     #:party-wide #f])
   )

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
     #:duration 10]) 
   )

 (define-skill super-charge
   #:cooldown 60.0
   #:attr (base-dmg hp% 500)
   #:duration 2.0
   #:type hydro
   (applied-buff
    [skill-atkup
     #:effect (buff atk% 200)
     #:limit 1
     #:party-wide #t
     #:duration 20])
   (applied-buff
    [skill-atkup
     #:effect (buff def% 300)
     #:limit 3
     #:party-wide #f
     #:duration 30])
   )

 (define-skill basic-slash
   #:cooldown 5.0 
   #:attr (base-dmg atk% 25)
   #:duration 1.0 
   #:type pyro
   (applied-buff
    [skill-hpup
     #:effect (buff hp 20)
     #:limit 1
     #:party-wide #f
     #:duration 10.0]) 
   )

 (define-skill holy-blast
   #:cooldown 90.0 
   #:attr (base-dmg atk% 500)
   #:duration 1.0 
   #:type hydro 
   )

 (define-artifact flaming-feather
   "fiery flames of a bygone era" 
   (mod atk 325) 
   (mod atk 27) 
   (mod em 42)
   )

 (define-artifact cracked-goblet
   "ruins of an the ancient city" 
   (mod critr 46.6) 
   (mod critd 16.2) 
   (mod critr 3.0)
   (mod def 128)
   )

 (define-artifact heros-cap
   "heros attire" 
   (mod atk% 20.5) 
   (mod atk 200)
   )

 (define-artifact universal-timepiece
   "relics from the edge of space" 
   (mod critd 60) 
   (mod em 300)
   )

 (define-character diluc
   #:hp 12000 
   #:def 500   
   #:atk 900   
   #:em 20    
   #:critr 5     
   #:critd 50    
   #:attacks attack-chain
   #:weapon wolfs-gravestone 
   #:skill basic-slash 
   #:burst all-attack-up 
   #:artifacts flaming-feather
   cracked-goblet
   )

 (define-character zhongli
   #:hp 15000 
   #:def 200  
   #:atk 1200   
   #:em 100    
   #:critr 75     
   #:critd 150    
   #:attacks attack-chain
   #:weapon vitality-staff
   #:skill super-charge 
   #:burst holy-blast
   #:artifacts heros-cap
   universal-timepiece
   )

 (define-enemy dummy
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

 (define-team-lineup all-alone (diluc))
 
 (define-team-lineup best-friends (diluc zhongli))
 
 (calculate-rotation-damage all-alone dummy (N N N N N N N N N N N N))

 (calculate-rotation-damage best-friends dummy (E Q N N N N (Swap 2) E Q N N N ND))
 )

Example Print Output:

Thanks for using ELEMENTS!

[]=======================================================================================[]
  Simulation run with input string: (N N N N N N N N N N N N)
  Total damage: 44781.5 Total time: 9.0 seconds DPS: 4975.72


  The best run with this layout was a sequence of: (N N N N N N N N N N N N)
  Total damage: 44781.5 Total time 9.0 seconds DPS: 4975.72
[]=======================================================================================[]

[]=======================================================================================[]
  Simulation run with input string: (E Q N N N N (Swap 2) E Q N N N ND)
  Total damage: 725403.92 Total time: 9.7 seconds DPS: 74783.91


  The best run with this layout was a sequence of: (E Q N N N N (Swap 2) E Q N N N ND)
  Total damage: 725403.92 Total time 9.7 seconds DPS: 74783.91
[]=======================================================================================[]
```
As seen above, `elements` allows for descriptive definitions of weapons, artifacts, characters, and more. This allows users to easily define new characters with varying stats, or modify current characters by applying new weapons, artifacts, or even useable skills.
Keywords also make the definitions easier to read, allowing their meanings to be reasonably readable without the need to check documentation notes. 
Additionally, `elements` ensures that users only reference the data they mean to use. This means that if a character is given an artifact or attack-string for their equipped weapon, a readable error will be displayed before being run. With this, `elements` reduces
the potential for unhelpful runtime errors by making sure the correct definitions are in the right places. This also extends to helpful compile time errors for duplicate names, giving clear errors when two definitions share the same name. 

## Installing and running

Check out this Git repository, change directory into it, and run:


```
raco pkg install
```

Then import as

```
(require elements)
```

Once installed, you can access the documentation via:

```
raco docs elements
```

Finally, you can run the tests with:

```
raco test -p elements
```
