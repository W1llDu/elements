#lang racket

(require "save-data.rkt")

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
         calc-dmg
         clear-file)

;; An AttackKey is a Symbol
;; Representing a character attack, being one of
;; 'N 'C 'E 'Q, or 'ND

;; A base stat is a Symbol
;; Representing one of a character's basic stats, being one of
;; 'hp 'atk 'def or 'em

;; An Element is a Symbol
;; Representing an elemental type, being one of
;; 'pyro 'hydro 'cryo 'electro 'geo 'anemo 'dendro or 'physical


;; A Character is a (character Number Number Number Number Number Number AttackSequence Weapon Skill Skill (ListOf Artifact))
;; Representing a character with base stats, crit stats, attacks, a weapon, a skill, a burst,
;; and a list of artifacts
(define-struct character [hp def atk critr critd em attacks weapon skill burst artifacts] #:transparent)

;; A Weapon is a (weapon Number Attribute (ListOf Buff))
;; Representing a weapon with a set damage, stat attribute, and list of buffs
(define-struct weapon [atk substat buffs] #:transparent)

;; A Skill is a (skill Number Attribute Element (ListOf TriggerBuff))
;; Representing a skill/burst with a cooldown, damage attribute, duration, element type,
;; and a list of buffs
(define-struct skill [cd attr duration type buffs] #:transparent)

;; An Attack is a (attack Attribute Number Element)
;; Representing an attack with a damage attribute, duration, and element type
(define-struct attack [attr duration type] #:transparent) ; split attr

;; An Artifact is a (artifact String Attribute (ListOf Attribute))
;; Representing an artifact belonging to a specific set, with a main attribute
;; and multiple subattributes
(define-struct artifact [set-name main-stat substats] #:transparent)

;; A Buff is a UnconditionalBuff, or a TriggerBuff

;; An UnconditionalBuff is a (unconditional-buff BuffAttribute Boolean)
;; Representing an always active buff with an attribute and an indicator to whether
;; it impacts all party members
(define-struct unconditional-buff [effect party-wide] #:transparent)

;; A TriggerBuff is a TriggeredBuff or an AppliedBuff

;; A TriggeredBuff is a (triggered-buff BuffAttribute Symbol Number Boolean Number)
;; Representing a buff with a trigger, attribute, max limit, duration, and indicator to
;; whether it impacts all party members
(define-struct triggered-buff [effect trigger limit party-wide duration] #:transparent)

;; An AppliedBuff is a (applied-buff BuffAttribute Number Boolean Duration)
;; Representing a buff with an attribute, max limit, duration, and indicator to
;; whether it impacts all party members, but without any specific trigger
(define-struct applied-buff [effect limit party-wide duration] #:transparent)

;; An AttackSequence is a (attack-sequence (ListOf Attack) Attack Attack)
;; Representing a sequence of Normal Attacks, alongside a charged attack and plunging attack
(define-struct attack-sequence [normals charged plunge] #:transparent)

;; An Attribute is an Attribute or a BuffAttribute

;; An Attribute is a (Symbol Number), where the symbol represents a certain stat

;; A BuffAttribute is a (Symbol Number) or a (Symbol Percent),
;; where the symbols represent certain stats

(define-struct attribute [attr modifier] #:transparent) ; amount is either number or percent (what is valid depends on attr)\\

;; A Percent is a (Symbol Number), where the symbol represents a scaling stat
(define-struct percent [attr p] #:transparent)

;; An Enemy is a (enemy Number ResistanceSet Number)
;; Representing an enemy with a defense stat, various resistances, and an overall damage reduction value
(define-struct enemy [def res reduction] #:transparent)

;; A ResistanceSet is a (resistances Number Number Number Number Number Number Number Number)
;; Representing a set of resistances to all 7 elements, plus non elemental physical attacks
(define-struct resistances [pyro hydro electro cryo geo anemo dendro physical] #:transparent)

; to make passing arguments much easier
;; An AccData is a ((ListOf Character) (ListOf Attack) Attack Attack (ListOf Buff) Symbol Number Number)
;; Representing data that is updated as damage calculations go on, including the current total time, damage,
;; and next attack in the normal attack sequence. It also stores buffs that are currently active, and the
;; element currently applied to the enemy (for the sake of elemental reactions)
(define-struct acc-data [team enemy attack-string cc nc active-buffs enemy-element dmg time])
 
; internal

;; A FlatCharacter is a (flat-char Number Number Number Number Number Number AttackSequence FlatSkill FlatSkill (ListOf Buff) (ListOf Buff))
;; Representing a flattened Character, with more data available near the surface
(define-struct flat-char [hp atk def critr critd em attacks skill burst unconditional-buffs trigger-buffs])

;; A FlatSkill is a (flat-skill Number BuffAttribute Number Element)
;; Representing a flattened Skill/Burst, with more data available near the surface
(define-struct flat-skill [cd attr duration type])

;; A StatInfo is a (stat-info Number Number Number Number Number Number)
;; Representing the current stat values during a calculation
(define-struct stat-info [hp atk def critr critd em])

;; A DamageInfo is a (damage-info Number Number Number Number Number Number Number Number Number)
;; Representing the information needed to calculate attack damage, including the base damage,
;; base damage multiplier, additional base damage, damage multiplier, defense multiplier,
;; resistances, amplification multiplier (for reactions), crit rate, and crit damage
(define-struct damage-info [base-dmg base-dmg-mult base-add
                                     dmg-mult
                                     def-mult
                                     res
                                     amp-mult
                                     critr
                                     critd])

; base-dmg-mult: talent/constellation
; base-add: talent/constellation/weapon/artifact (increase by scaling amount (def% 56))

; final output: dmg, time

; calculate the total damage a team does to an enemy, given an attack string
; calc-dmg : (List Character) Enemy (List AttackKey) -> (Tuple Integer Integer)
(define (calc-dmg team enemy attack-string)
  ; pull out party-wide uncond buffs into active-buffs?
  (define result (calc-dmg/acc (make-acc-data (map flatten-char team) enemy attack-string 1 1 empty 'none 0 0)))
  ; save the entry before returning it
  (save-entry (list attack-string (first result) (second result) enemy team))
  result)

; flattens character to make buffs more accessible
; flatten-char : Character -> FlatCharacter
(define (flatten-char char)
  (make-flat-char (character-hp char)
                  (+ (character-atk char) (weapon-atk (character-weapon char)))
                  (character-def char)
                  (character-critr char)
                  (character-critd char)
                  (character-em char)
                  (character-attacks char) 
                  (flatten-skill (character-skill char))
                  (flatten-skill (character-burst char))
                  (flatten-unconditional char)
                  ; triggered
                  (flatten-triggered char)))

; flattens skills like characters
; flatten-skill : Skill -> FlatSkill
(define (flatten-skill skill)
  (make-flat-skill (skill-cd skill)
                   (skill-attr skill)
                   (skill-duration skill)
                   (skill-type skill)))

; flatten unconditional buffs for easier representation
; flatten-unconditional : Character -> (List UnconditionalBuff)
(define (flatten-unconditional char)
  (append (filter unconditional-buff? (append (cons (attr->unconditional (weapon-substat (character-weapon char)))
                                                    (weapon-buffs (character-weapon char)))
                                              (skill-buffs (character-skill char))
                                              (skill-buffs (character-burst char))))
          ; attribute stats get treated as unconditional buffs
          (apply append (map (λ (artifact) (cons (attr->unconditional (artifact-main-stat artifact))
                                                 (map attr->unconditional (artifact-substats artifact))))
                             (character-artifacts char)))))

; attribute to unconditional (for artifacts)
; attr->unconditional : Attribute -> UnconditionalBuff
(define (attr->unconditional attr)
  (make-unconditional-buff attr #f))

; flatten triggered buffs for easier representation
; flatten-triggered : Character -> (List TriggeredBuff)
(define (flatten-triggered char)
  ; weapon trigger: on normal&charged
  ; skill/burst trigger: on skill/burst
  (append (filter triggered-buff? (append (weapon-buffs (character-weapon char))
                                          (skill-buffs (character-skill char))
                                          (skill-buffs (character-burst char))))
          ; convert applied to triggered at runtime, mapped to specific triggers
          (convert-to-triggered 'normal-attack (weapon-buffs (character-weapon char)))
          (convert-to-triggered 'charged-attack (weapon-buffs (character-weapon char)))
          (convert-to-triggered 'skill (skill-buffs (character-skill char)))
          (convert-to-triggered 'burst (skill-buffs (character-burst char)))))

; convert applied buffs to corresponding triggered buffs for easier calculation
; convert-to-triggered : Attack-Trigger (List Buff) -> (List TriggeredBuff)
(define (convert-to-triggered trigger buffs)
  (map (λ (buff) (applied->triggered buff trigger))
       (filter applied-buff? buffs)))

; convert all applied buffs into triggered buffs to make calculation easier
; applied->triggered : Applied-Buff Attack-Trigger -> TriggeredBuff
(define (applied->triggered applied trigger)
  (make-triggered-buff (applied-buff-effect applied)
                       trigger
                       (applied-buff-limit applied)
                       (applied-buff-party-wide applied)
                       (applied-buff-duration applied)))

; main loop calculate the total damage and time taken for a rotation
; calc-dmg/acc : AccData -> (Tuple Integer Integer)
(define (calc-dmg/acc acc-values) ; more acc args like buffs (later)
  ; define struct data
  (define attack-string (acc-data-attack-string acc-values))
  (define dmg (acc-data-dmg acc-values))
  (define time (acc-data-time acc-values))
  (cond [(empty? attack-string)
         (list dmg time)]
        [(cons? attack-string)
         (update-and-calculate acc-values)]))

; assign the necessary new values for the next attack and calculate damage
; update-and-calculate : AccData -> (Tuple Integer Integer)
(define (update-and-calculate acc-values)
  ; define struct data
  (define attack-string (acc-data-attack-string acc-values))
  (define dmg (acc-data-dmg acc-values))
  (define time (acc-data-time acc-values))
  (define team (acc-data-team acc-values))
  (define enemy (acc-data-enemy acc-values))
  (define cc (acc-data-cc acc-values))
  (define nc (acc-data-nc acc-values))
  (define active-buffs (acc-data-active-buffs acc-values))
  (define enemy-element (acc-data-enemy-element acc-values))
  ; use the old values to calculate new values
  (let* ([char (list-ref team (- cc 1))]
         [attack (first attack-string)]
         [cc* (if (list? attack) (second attack) cc)]
         [nc* (determine-next-nc acc-values char attack)]
         [duration (calc-duration acc-values char attack)]
         [active-buffs* (add-new-triggered acc-values char attack)]
         [active-buffs** (remove-expiring-buffs active-buffs* duration)]
         [action (determine-action acc-values char attack)]
         [type (determine-type attack action)]
         [enemy-element* (determine-enemy-element acc-values type attack)]
         [attr (determine-action-attribute action attack)]
         [dmg* (determine-damage acc-values char attack active-buffs* type attr)]
         [time* (+ time duration)])
    (if (list? attack)
        (if (= cc (second attack))
            ; swap to same = do nothing
            (calc-dmg/acc (make-acc-data team
                                         enemy
                                         (rest attack-string)
                                         cc
                                         nc
                                         active-buffs
                                         enemy-element
                                         dmg
                                         time))
            (calc-dmg/acc (make-acc-data team
                                         enemy
                                         (rest attack-string)
                                         cc*
                                         nc*
                                         ; remove non-teamwide buffs
                                         (filter triggered-buff-party-wide active-buffs**)
                                         enemy-element
                                         dmg
                                         time*)))
        (calc-dmg/acc (make-acc-data team
                                     enemy
                                     (rest attack-string)
                                     cc*
                                     nc*
                                     active-buffs**
                                     enemy-element*
                                     dmg*
                                     time*)))))

; determine the next nc
; determine-next-nc : AccData Character AttackKey -> Integer
(define (determine-next-nc acc-values char attack)
  (define nc (acc-data-nc acc-values))
  (if (and (symbol? attack) (symbol=? 'N attack))
      (if (= nc
             (length (attack-sequence-normals
                      (flat-char-attacks char))))
          1
          (+ 1 nc))
      1))

; calculate the duration of the current attack
; calc-duration : AccData Character AttackKey -> Number
(define (calc-duration acc-values char attack)
  (define nc (acc-data-nc acc-values))
  (cond [(list? attack) 1]
        [(symbol=? 'N attack)
         (attack-duration (list-ref (attack-sequence-normals (flat-char-attacks char))
                                    (sub1 nc)))]
        [(symbol=? 'C attack)
         (attack-duration (attack-sequence-charged (flat-char-attacks char)))]
        [(symbol=? 'E attack)
         (flat-skill-duration (flat-char-skill char))]
        [(symbol=? 'Q attack)
         (flat-skill-duration (flat-char-burst char))]
        [(symbol=? 'ND attack) 0.1]
        ))

; add newly triggered buffs
; add-new-triggered : AccData Character AttackKey -> (List Buff)
(define (add-new-triggered acc-values char attack)
  (merge-buffs (filter (λ (buff) (symbol=? (triggered-buff-trigger buff)
                                           (attack->trigger attack)))
                       (flat-char-trigger-buffs char))
               (acc-data-active-buffs acc-values)))

; turn attack-key into a trigger
; attack->trigger : AttackKey -> Attack-Trigger
(define (attack->trigger attack)
  (match attack
    ['N 'normal-attack]
    ['C 'charged-attack]
    ['E 'skill]
    ['Q 'burst]
    [_ 'none]))

; merge new buffs into existing buffs
; merge-buffs : (List Buff) (List Buff) -> (List Buff)
(define (merge-buffs new old)
  (cond [(empty? new) old]
        [(cons? new) (merge-buffs (rest new) (merge-buff* (first new) old))]))

; helper for merge-buffs
; merge-buff* : Buff (List Buff) -> (List Buff)
(define (merge-buff* new old)
  (cond [(empty? old) (list (make-triggered-buff (triggered-buff-effect new)
                                                 (triggered-buff-trigger new)
                                                 1
                                                 (triggered-buff-party-wide new)
                                                 (triggered-buff-duration new)))]
        [(cons? old) (if (triggered-buff=?* new (first old))
                         (cons (merge-buff new (first old)) (rest old))
                         (cons (first old) (merge-buff* new (rest old))))]))

; performs the merge for a single buff
; merge-buff : Buff Buff -> Buff
(define (merge-buff new old)
  (let ([limit* (min (triggered-buff-limit new)
                     (+ 1 (triggered-buff-limit old)))]
        [duration* (max (triggered-buff-duration new)
                        (triggered-buff-duration old))])
    ; limit stays the same
    (make-triggered-buff (triggered-buff-effect new)
                         (triggered-buff-trigger new)
                         limit*
                         (triggered-buff-party-wide new)
                         duration*)))

; checks if 2 triggered buffs are the same
; triggered-buff=?* : Buff Buff -> Boolean
(define (triggered-buff=?* b1 b2)
  (and (equal? (triggered-buff-effect b1)
               (triggered-buff-effect b2))
       (equal? (triggered-buff-trigger b1)
               (triggered-buff-trigger b2))
       (equal? (triggered-buff-party-wide b1)
               (triggered-buff-party-wide b2))))

; remove buffs that, after duration passes, will expire
; remove-expiring-buffs : (List Buff) Number -> (List Buff)
(define (remove-expiring-buffs active-buffs* duration)
  (filter (λ (buff) (< 0 (triggered-buff-duration buff)))
          (map (λ (buff) (make-triggered-buff (triggered-buff-effect buff)
                                              (triggered-buff-trigger buff)
                                              (triggered-buff-limit buff)
                                              (triggered-buff-party-wide buff)
                                              (- (triggered-buff-duration buff)
                                                 duration)))
               active-buffs*)))

; determine the action the character is taking
; determine-action : AccData FlatCharacter AttackKey -> (Or Empty Attack FlatSkill)
(define (determine-action acc-values char attack)
  (cond [(list? attack) '()]
        [(or (symbol=? 'N attack) (symbol=? 'ND attack))
         (list-ref (attack-sequence-normals (flat-char-attacks char))
                   (sub1 (acc-data-nc acc-values)))]
        [(symbol=? 'C attack) (attack-sequence-charged (flat-char-attacks char))]
        [(symbol=? 'E attack) (flat-char-skill char)]
        [(symbol=? 'Q attack) (flat-char-burst char)]))

; determine the type of the characters action
; deterimne-type : AttackKey (Or Empty Attack FlatSkill) -> Element
(define (determine-type attack action)
  (cond [(list? attack) '()]
        [(or (symbol=? 'N attack) (symbol=? 'ND attack) (symbol=? 'C attack))
         (attack-type action)]
        [(or (symbol=? 'E attack) (symbol=? 'Q attack))
         (flat-skill-type action)]))

; determine the next element of the enemy
; determine-enemy-element : AccData Element Element (Or Empty Attack FlatSkill) -> Element
(define (determine-enemy-element acc-values type attack)
  (define enemy-element (acc-data-enemy-element acc-values))
  (cond [(list? attack) enemy-element]
        [(symbol=? type 'physical) enemy-element]
        ; type is not 'physical anymore
        [(symbol=? enemy-element 'none) type]
        [(symbol=? type enemy-element) enemy-element]
        [else 'none]))

; determine the attribute of the character's action
; determine-action-attribute : (Or Empty Attack FlatSkill) AttackKey -> Attribute
(define (determine-action-attribute action attack)
  (cond [(list? attack) '()]
        [(or (symbol=? 'N attack) (symbol=? 'ND attack) (symbol=? 'C attack))
         (attack-attr action)]
        [(or (symbol=? 'E attack) (symbol=? 'Q attack))
         (flat-skill-attr action)]))

; stage 2
; determine the total damage following the action
; determine-damage : AccData FlatCharacter AttackKey (List Buff) Element Attribute -> Number
(define (determine-damage acc-values char attack active-buffs* type attr)
  (define dmg (acc-data-dmg acc-values))
  (define enemy (acc-data-enemy acc-values))
  (define nc (acc-data-nc acc-values))
  (define enemy-element (acc-data-enemy-element acc-values))
  (if (list? attack)
      dmg
      (let ([stats (calc-total-stats char active-buffs*)])
        (+ dmg
           ; stage 2, stage 3
           ; generate-dmg-info is a complex way
           ; to get all the modified stats + values needed to
           ; calculate damage
           (* (+ (* (calc-base-dmg attr stats)
                    (+ 1 0))  ; base-dmg-mult ; look for dmg% in buffs
                 0) ; base-add ; look for dmg in buffs
              1 ; dmg-mult ; elemental dmg bonus, which we dont have syntax for
              1 ; def-mult ; relies on levels, which we dont have syntax for
              (calc-res (enemy-res enemy) type) ; actual enemy res
              (calc-amp-mult type enemy-element) ; amp-mult (reactions)
              (calc-crit (min (stat-info-critr stats) 100) (stat-info-critd stats)))))))

; do a tree traversal (abstract out, use symbols?) flat incr vs incr by other amt (atk (def% 50)) increases atk by 50% of base def
; calc-total-stats : FlatCharacter (List Buff) -> Stat-Info
(define (calc-total-stats char active-buffs)
  ; remember to scale % to decimal
  ; % is already desugared
  (let ([buffs (append (flat-char-unconditional-buffs char) active-buffs)]
        [base-stat-info (make-stat-info (flat-char-hp char)
                                        (flat-char-atk char)
                                        (flat-char-def char)
                                        (flat-char-critr char)
                                        (flat-char-critd char)
                                        (flat-char-em char))])
    ; (stat flat)
    ; (stat (stat% percent))
    ; base stats + percent modifiers calculated (and flat mods too)
    (make-stat-info (apply-all-modifiers buffs base-stat-info (flat-char-hp char) 'hp) 
                    (apply-all-modifiers buffs base-stat-info (flat-char-atk char) 'atk)
                    (apply-all-modifiers buffs base-stat-info (flat-char-def char) 'def)
                    (apply-all-modifiers buffs base-stat-info (flat-char-critr char) 'critr)
                    (apply-all-modifiers buffs base-stat-info (flat-char-critd char) 'critd)
                    (apply-all-modifiers buffs base-stat-info (flat-char-em char) 'em))))

; applies all buffs of a specified category
; apply-all-modifiers : (List Buff) StatInfo Number BaseStat -> Number
(define (apply-all-modifiers buffs base-stat-info base-attr symbol)
  (+ base-attr
     (apply + (map (λ (modifier) (calc-modifier modifier base-stat-info))
                   (get-mods-with-attr buffs symbol)))))

; gets modifiers of buffs that increase the given scaling-stat
; get-mods-with-attr : (List Buff) ScalingStat -> (List Modifier)
(define (get-mods-with-attr buffs attr)
  ; convert to modifier
  (map (λ (buff) (cond [(unconditional-buff? buff)
                        (attribute-modifier (unconditional-buff-effect buff))]
                       [(triggered-buff? buff)
                        (attribute-modifier (triggered-buff-effect buff))]))
       (filter (λ (buff) (cond [(unconditional-buff? buff)
                                (symbol=? (attribute-attr
                                           (unconditional-buff-effect buff))
                                          attr)]
                               [(triggered-buff? buff)
                                (symbol=? (attribute-attr
                                           (triggered-buff-effect buff))
                                          attr)]))
               buffs)))

; calculate how much a modifer augments the given base stats
; calc-modifer : (Or Number Percent) StatInfo -> Number
(define (calc-modifier modifier base-stats)
  (if (number? modifier)
      modifier
      (* (percent-p modifier)
         0.01
         (lookup-stat base-stats (percent-attr modifier)))))

; calculate base damage
; calc-base-attr : Attribute StatInfo -> Number
(define (calc-base-dmg attr stats)
  ; ignore attribute-attr
  ; must be percent
  (* (percent-p (attribute-modifier attr))
     0.01
     (lookup-stat stats (percent-attr (attribute-modifier attr)))))

; match a stat symbol to its proper stat info
; lookup-stat : Stat-Info PercentStat -> Number
(define (lookup-stat stats attr)
  (case attr
    ['atk% (stat-info-atk stats)]
    ['def% (stat-info-def stats)]
    ['hp% (stat-info-hp stats)]
    ['em% (stat-info-em stats)]
    ))

; calculate the resistance to a given element
; calc-res : ResistanceSet Element -> Number
(define (calc-res ress type)
  (let ([res (/ (lookup-res ress type) 100)])
    (cond [(< res 0) (- 1 (/ res 2))]
          [(>= res 0.75) (/ 1 (+ (* 4 res) 1))]
          [else (- 1 res)])))

; match an element symbol to its proper resistance
; lookup-res : ResistanceSet Element -> Number
(define (lookup-res ress type)
  (case type
    ['pyro (resistances-pyro ress)]
    ['hydro (resistances-hydro ress)]
    ['electro (resistances-electro ress)]
    ['cryo (resistances-cryo ress)]
    ['geo (resistances-geo ress)]
    ['anemo (resistances-anemo ress)]
    ['dendro (resistances-dendro ress)]
    ['physical (resistances-physical ress)]
    ))

; amplifying reactions (transformatives DNE)
; calc-amp-mult : Element Element -> Element
(define (calc-amp-mult attack-type enemy-element)
  (match `(,attack-type ,enemy-element)
    ['(pyro cryo) 2]
    ['(cryo pyro) 1.5]
    ['(hydro pyro) 2]
    ['(pyro hydro) 1.5]
    [_ 1]))

; determine the average bonus crit damage
; calc-crit : Number Number -> Number
(define (calc-crit crit-rate crit-dmg)
  (+ (- 1 (/ crit-rate 100))
     (* (/ crit-rate 100)
        (+ 1 (/ crit-dmg 100))))) 

;; some quick runtime tests
(module+ test
  (require rackunit)

  ;; define runtime versions of macros
  (define attack-chain (make-attack-sequence
                        (list (make-attack (make-attribute 'atk (make-percent 'atk% 10)) 0.5 'physical)
                              (make-attack (make-attribute 'atk (make-percent 'atk% 25)) 0.2 'physical)
                              (make-attack (make-attribute 'atk (make-percent 'atk% 125)) 0.8 'physical)
                              (make-attack (make-attribute 'atk (make-percent 'atk% 250)) 1.5 'physical))
                        (make-attack (make-attribute 'hp (make-percent 'hp% 5)) 3.5 'pyro)
                        (make-attack (make-attribute 'hp (make-percent 'hp% 10)) 3.5 'physical)))
    
  (define test-weapon
    (make-weapon 450 (make-attribute 'critr 24.1)
                 (list (make-triggered-buff (make-attribute 'atk (make-percent 'atk% 20)) 'normal-attack 1 #f 10)
                       (make-unconditional-buff (make-attribute 'critd 20) #f))))

  (define all-attack-up
    (make-skill 25 (make-attribute 'atk (make-percent 'atk% 125)) 0.1 'pyro
                (list (make-applied-buff (make-attribute 'atk 125) 1 #t 10))))

  (define basic-slash
    (make-skill 5 (make-attribute 'atk (make-percent 'atk% 25)) 1.0 'pyro
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

  ;;test runtime
  (check-equal? (calc-dmg lone-member dummy '(N N N N)) '(12639.64016568 3.0))
  ;; different string
  (check-equal? (calc-dmg lone-member dummy '(N N N N N C)) '(13363.737272160002 7.0))
  ;; simple
  (check-equal? (calc-dmg lone-member dummy '(N)) '(308.2839064800001 0.5))
  (check-equal? (calc-dmg lone-member dummy '(C)) '(415.8132 3.5))
    )

(module+ test
  (require rackunit)
  ;; ensure calc-crit works
  (check-equal? (calc-crit 100 100) 2)
  (check-equal? (calc-crit 0 0) 1)
  (check-equal? (calc-crit 50 50) (/ 5 4))
  ;; overcap technically possible
  (check-equal? (calc-crit 200 100) 3)
  )