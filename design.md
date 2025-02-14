# Purpose
The purpose of this DSL is to provide an easy way for Genshin Impact players to calculate and potentially
optimize their rotations. More concretely, this means maximizing the amount of damage their characters
do over a short, set period of time. The idea initially sprang to mind due to personal experiences we
had both faced as players of the game. The game contains many complex mechanics (which are discussed below),
and the result is a near infinite number of potential ways to set up characters deal damage. This obviously
makes it very hard to predict what actions will produce the highest and largest amounts of damage. Since
the purpose of the game essentially boils down to producing the highest and largest amounts of damage, we were
of course frustrated that there was not an easier way to figure it all out. Calculators for the game exist online,
but mostly focus on optimizing the characters themselves, not how you actually play them. There's a key distinction there,
as I could tell you have a rare lawn gnome worth $1.5 million USD, but that's not much use to you if you
have know idea where to sell it (because let's be real, that's a pretty niche market). All of that is to say 
that tools to calculate rotation damage for Genshin are severely lacking. So, the purpose of this DSL is to remedy
that. At a baseline, this DSL should be able to calculate damage values for a rotation when given the proper details
and setup, which will be described later. Additionally, this DSL will allow for the querying of buffs at specific times
within rotations, to see if damage is truly being optimized. Going even further, the DSL should be able to attempt potential
action strings to see if it can optimize damage. Finally, the DSL should be able to print some sort of visual, time permitting.
In order to further understand the purpose of this DSL, please refer to the concepts below, to gain a better understanding
of the game, and the data our DSL will need to collect.

## Concepts
1. Characters
   1. Base statistics determine damage floor
   2. Base stats are HP, Defense, Attack, Crit Rate, Crit Damage, and Elemental Mastery (em) 
   3. Normal attacks (Charged and Plunging) and Skills scale off of these stats
   4. Character have access to an elemental skill and elemental burst, both of which are activated abilities that deal damage or apply buffs.
   5. They can hold weapons and other items that provide buffs to these stats or other aspects of their damage output
   6. Buffs are divided into multiple categories. Certain buffs can have durations and require conditions for activation to gain their effects.
   For instance, a buff could be activated by doing a normal attack, and last for 10 seconds.
   7. Other buffs have a duration, but are applied after a burst or skill, meaning they have no specific trigger otherwise.
   8. Finally, certain buffs are unconditional, and are always active no matter what.
   9. In the case of all buffs, they can either apply to the character activating them, or to every member of the team at once.
   
2. Teams
   1. A team can have up to 4 characters in it.
   2. Only one character is capable of attacking at a time, known as "being on the field".
   3. Characters on a given team can be switched between on a short (basically negligible) cooldown.
   4. Characters can lose buffs when they are switched off of the field.
   5. The actions a player takes over a duration of time while using a team (for instance, attacking, switching, using skills, etc.) is called a "rotation".
   6. An optimal team is the one that deals the most amount of damage in a single rotation.

3. Enemies
   1. Enemies represent a smaller but still important aspect of the game for optimization purposes (and thus the DSL).
   2. When a character attacks with damage that has a specific element, that element is applied to the enemy they attack.
   3. If an enemy already has an element at the time this application occurs, the two elements interact, and cause a damage modifier to be applied.
   4. This action also clears all elements currently applied to the enemy.
   5. Examples of elements interacting, known as "Reactions", are defined as follows:
      1. Melt: Pyro + Cryo, deals 2x damage
      2. Vaporize: Pyro + Hydro, deals 1.5x damage
      3. Overload: Pyro + Electro, deals damage that scales off elemental mastery
      4. Electro-Charged: Hydro + Electro, deals damage that scales off elemental mastery
      5. Super-conduct: Cryo + Electro, deals damage that scales off elemental mastery
      6. Swirl: Anemo + Pyro, Cryo, Electro, or Hydro, deals damage that scales off elemental mastery


# Example programs

```
(define-weapon test-weapon
  450 ;; base attack stat
  (critr 24.1) ;; substat (crit rate)
  (triggered-buff
   [dmgup
   #:effect (atk% 20.0) ;; increase atk by 20%
   #:trigger normal-attack
   #:limit 1
   #:target self  
   #:duration 10.0])
  
  (buff-unconditional
   [crit-up
   #:effect (critd 20) ;; increase crit damage by 20%
   #:target self]) 
)

(define-skill basic-slash
  5.0 ;; cooldown
  1.0 ;; duration (where character cannot do anything else)
  (damage
   [(atk 25)
   #:duration 0.1
   #:type pyro]) ;; deal damage equal to 25% of attack, occurs after 0.1 seconds, pyro (fire) damage
  
  (applied-buff
   [skill-hpup
   #:effect (hp (atk% 10))
   #:limit 1
   #:target self
   #:duration 10.0]) ;; increase hp by 10% of atk, only applies to current character
)

;; note : duration is optional
(define-skill all-attack-up  
  25.0
  2.0
  (damage
   [(atk% 125)
   #:duration 0.1
   #:type pyro])
  (applied-buff
   [skill-atkup
   #:effect (atk (atk% 125))
   #:limit 1
   #:target all
   #:duration 10]) ;; this time applies to all members of a lineup
)

(define-attack-sequence attack-chain
  ([(atk% 10) 0.5]
   [(atk% 25) 0.2]
   [(atk% 125) 0.8]
   [(atk% 250) 1.5]
   #:charged [(hp% 5) 3.5]
   #:plunging (hp% 10)))

(define-artifact test-feather
  feather
  "cool feather collection" ;; set name
  (atk 325) ;; main stat
  (atk% 27) ;; substats
  (em 42)
)

(define-artifact test-goblet
  goblet
  "cool goblet collection" ;; set name
  (dmg-pyro 46.6) ;; main stat
  (critd 16.2) ;; substats
  (critr 3.0)
  (def 128)
)

(define-character test-char
  Hydro
  12000 ;; base hp
  500   ;; base def
  900   ;; base atk
  20    ;; base em
  5     ;; base crit rate
  50    ;; base crit damage
  attack-chain
  test-weapon ;; weapon
  basic-slash ;; skill
  all-attack-up ;; burst
  (<buff> <buff> <buff> <buff> <buff> <buff>) ;; constellation passive buffs
  <buff> ;; ascension passive buffs
  <buff>
  #:feather test-feather
  #:goblet test-goblet
  )

(define-enemy dummy
  Pyro
  1000                       ;; base def
  (50 10 10 10 10 10 10 -20) ;; resistances
  5                          ;; damage reduction
  )

(define-team-lineup lone-member `(,test-char))
(calculate-rotation-damage lone-member dummy (list N N N C E N ND N ND Q N ND E))

```


# Grammar and Signatures
```
Vocabulary:
a name is a String representing the name of some representation
a duration is a number (Decimals allowed) representing time
a Var is a sequence of characters and/or numbers, with no spaces

Grammars:

<define> := (define-weapon name Int (<stat> Int) <buff> ...)
          | (define-skill name Int Int <buff> ...)
          | (define-character name Element Int Int Int Int Int Int AttackSequence
              Weapon Skill Skill (Tuple <buff> <buff> <buff> <buff> <buff> <buff>) <buff> <buff>
              Artifact Artifact Artifact Artifact Artifact)
          | (define-attack-sequence name ([(<stat> Number) duration] ...
                                         #charged [(<stat> Number) duration]
                                         #plunging (<stat> Number)))
          | (define-artifact name ArtifactType String (<stat> Number) ...)
          | (define-team-lineup name (Var ... ))
          | (define-enemy Element Int (Tuple Int Int Int Int Int Int Int Int) Int)

;; A stat is one of of the following symbols
;; <stat> := atk ;; flat attack
           | atk% ;; attack percent
           | def ;; flat defense
           | def% ;; defense percent
           | hp ;; flat hp
           | hp% ;; hp percent
           | critr ;; crit rate percent
           | critd ;; crit damage percent
           | em ;; elemental mastery

;; <buff> := triggered-buff
;;         | unconditional-buff
;;         | applied-buff
;;         | damage

;; <target> := SELF
             | ALL

;; <artifact-type> := flower
                    | plume
                    | sands
                    | goblet
                    | circlet

;; Creates a named buff representation with an effect, trigger,
;; stack limit, target type, and a duration
<triggered-buff> := (triggered-buff [name #:effect (<stat> (<stat> Number))
                                          #:trigger <id>
                                          #:limit Int
                                          #:target <target>
                                          #:duration duration])
                      
;; Creates a named buff representation that is always active
;; with an effect and target type
<unconditional-buff> := (unconditional-buff [name #:effect e
                                                  #:target target])

;; Creates a named buff representation that is timed with no trigger
;; with an effect, stack limit, target type, and duration
<applied-buff> := (applied-buff [name #:effect (<stat> (<stat> Number))
                                      #:limit Int
                                      #:target <target>
                                      #:duration duration])

;; <element> := pyro
              | hydro
              | electro
              | cryo
              | geo
              | anemo
              | dendro

;; <damage-type> := <element>
                  | physical

;; creates a named buff representation that deals damage based on the percentage of a stat
;; after d seconds, with a specified type
<damage> := (damage [(stat Number)
                     #:duration Number
                     #:type <damage-type>])
         
;; calculates the damage of a rotation for a given team
<rotation-damage> := (calculate-rotation-damage Team Enemy (<action> ...))

;; set of possible actions
;; <action> := N ;; a normal attack
             | E ;; uses their skill
             | Q ;; uses their second skill (burst)
             | ND ;; normal attack + dash cancel
             | C ;; charged attack
             | Swap Int ;; switches to nth character in party (up to 4)

```



# Implementation milestones

1. Define data structures and macros for characters, weapons, artifacts, enemies, skills, damage, buffs, and state
2. Implement basic damage calulations (base stats only + stat increases)
3. Implement buff calculations (weapon/artifact applied/unconditional, then triggered-buff)

In any order:
- Implement elemental reaction calculations
- Implement attack string optimizer
- Implement artifact optimizer
- Implement character order optimizer
