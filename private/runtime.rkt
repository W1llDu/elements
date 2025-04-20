#lang racket

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
         display-data
         calc-dmg
         clear-file)

(define-struct character [hp def atk critr critd em attacks weapon skill burst artifacts] #:transparent)
(define-struct weapon [atk substat buffs] #:transparent)
(define-struct skill [cd attr duration type buffs] #:transparent)
(define-struct attack [attr duration type] #:transparent) ; split attr
(define-struct artifact [set-name main-stat substats] #:transparent)
(define-struct triggered-buff [effect trigger limit party-wide duration] #:transparent)
(define-struct unconditional-buff [effect party-wide] #:transparent)
(define-struct applied-buff [effect limit party-wide duration] #:transparent)
(define-struct attack-sequence [normals charged plunge] #:transparent)
(define-struct attribute [attr modifier] #:transparent) ; amount is either number or percent (what is valid depends on attr)
(define-struct percent [attr p] #:transparent)
(define-struct enemy [def res reduction] #:transparent)
(define-struct resistances [pyro hydro electro cryo geo anemo dendro physical] #:transparent)

;; to make passing arguments much easier
(define-struct acc-data [team enemy attack-string cc nc active-buffs enemy-element dmg time] #:transparent)
 
; internal
(define-struct flat-char [hp atk def critr critd em attacks skill burst unconditional-buffs trigger-buffs] #:transparent)
(define-struct flat-skill [cd attr duration type] #:transparent)
(define-struct stat-info [hp atk def critr critd em] #:transparent)
(define-struct damage-info [base-dmg base-dmg-mult base-add
                                     dmg-mult
                                     def-mult
                                     res
                                     amp-mult
                                     critr
                                     critd] #:transparent)
; base-dmg-mult: talent/constellation
; base-add: talent/constellation/weapon/artifact (increase by scaling amount (def% 56))

; final output: dmg, time, dmg/s

; saves an entry to the save data file
(define (save-entry entry)
  (call-with-output-file "data.txt"
    (lambda (out)
      (write entry out) 
      (newline out))   
    #:exists 'append))

; loads all information in the save data file
(define (load-entries)
  (with-handlers ([exn:fail? (lambda (_) '())])
    (call-with-input-file "data.txt"
      (lambda (in)
        (read-lines '() in)))))

; reads the lines of the save data file
(define (read-lines entries in)
  (let ([line (read in)])
    (if (eof-object? line)
        (reverse entries)
        (read-lines (cons line entries) in))))

; clears the save data file
(define (clear-file)
  (call-with-output-file "data.txt"
    (lambda (out) (void))
    #:exists 'truncate))


; used to remember the attack string for a calculation
(define current-atk-string '())

; calculate the total damage a team does to an enemy, given an attack string
(define (calc-dmg team enemy attack-string)
  ; pull out party-wide uncond buffs into active-buffs
  (set! current-atk-string attack-string)
  (calc-dmg/acc (make-acc-data (map flatten-char team) enemy attack-string 1 1 empty 'none 0 0)))

;; flatten unconditional buffs for easier representation
(define (flatten-unconditional char)
  (append (filter unconditional-buff? (append (weapon-buffs (character-weapon char))
                                              (skill-buffs (character-skill char))
                                              (skill-buffs (character-burst char))))
          ; attribute stats get treated as unconditional buffs
          (apply append (map (λ (artifact) (cons (attr->unconditional (artifact-main-stat artifact))
                                                 (map attr->unconditional (artifact-substats artifact))))
                             (character-artifacts char)))))

;; convert applied buffs to corresponding triggered buffs for easier calculation
(define (convert-to-triggered symbol value)
  (map (λ (buff) (applied->triggered buff symbol))
       (filter applied-buff? value)))

;; flatten triggered buffs for easier representation
(define (flatten-triggered char)
  (append (filter triggered-buff? (append (weapon-buffs (character-weapon char))
                                          (skill-buffs (character-skill char))
                                          (skill-buffs (character-burst char))))
          ; convert applied to triggered at runtime, mapped to specific triggers
          (convert-to-triggered 'normal-attack (weapon-buffs (character-weapon char)))
          (convert-to-triggered 'charged-attack (weapon-buffs (character-weapon char)))
          (convert-to-triggered 'skill (skill-buffs (character-skill char)))
          (convert-to-triggered 'burst (skill-buffs (character-burst char)))))

;; flattens character to make buffs more accessible
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
                  (flatten-triggered char)
                  ; weapon trigger: on normal&charged
                  ; skill/burst trigger: on skill/burst
                  ))

;; flattens skills like characters
(define (flatten-skill skill)
  (make-flat-skill (skill-cd skill)
                   (skill-attr skill)
                   (skill-duration skill)
                   (skill-type skill)))

; attribute to unconditional (for artifacts)
(define (attr->unconditional attr)
  (make-unconditional-buff attr #f))

; convert all applied buffs into triggered buffs to make calculation easier
(define (applied->triggered applied trigger)
  (make-triggered-buff (applied-buff-effect applied)
                       trigger
                       (applied-buff-limit applied)
                       (applied-buff-party-wide applied)
                       (applied-buff-duration applied)))

;; checks for a more optimal damage rotation based on saved results
(define (determine-current-optimal data-list curr-max team enemy)
  (cond [(empty? data-list) curr-max]
        [else (define entry (first data-list))
              (define dps (/ (second entry)
                             (third entry)))
              (if (and (or (empty? curr-max) (> dps (/ (second curr-max)
                                                       (third curr-max))))
                       (equal? (fourth entry) team)
                       (equal? (fifth entry) enemy))
                  (determine-current-optimal (rest data-list) entry team enemy)
                  (determine-current-optimal (rest data-list) curr-max team enemy))]))

;; simple rounding function for displaying data
(define (decimal-round num)
  (/ (round (* 100 num)) 100))

;; calculate and display the results of a damage calculation
(define (display-data team enemy attack-string)
  (define result (calc-dmg team enemy attack-string))
  (define final-dmg (first result))
  (define final-time (second result))
  (define string-data (last (load-entries)))
  (display "[]=======================================================================================[]\n")
  (display "  Simulation run with input string: ")
  (display current-atk-string)
  (display "\n")
  (display (format "  Total damage: ~a Total time: ~a seconds DPS: ~a\n"
                   (decimal-round final-dmg)
                   (decimal-round final-time)
                   (decimal-round (/ final-dmg final-time)))) 
  (display "\n\n")
  (define best (determine-current-optimal (load-entries)
                                          (list)
                                          (fourth string-data)
                                          (fifth string-data)))
  (display (format "  The best run with this layout was a sequence of: ~a\n"
                   (first best)))
  (display (format "  Total damage: ~a Total time ~a seconds DPS: ~a\n"
                   (decimal-round (second best))
                   (decimal-round (third best))
                   (decimal-round (/ (second best) (third best)))))
  (display "[]=======================================================================================[]\n\n"))

;; calculate the duration of the current attack
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

;; add newly triggered buffs
(define (add-new-triggered acc-values char attack)
  (merge-buffs (filter (λ (buff) (symbol=? (triggered-buff-trigger buff)
                                           (attack->trigger attack)))
                       (flat-char-trigger-buffs char))
               (acc-data-active-buffs acc-values)))

;; remove buffs that, after duration passes, will expire
(define (remove-expiring-buffs active-buffs* duration)
  (filter (λ (buff) (< 0 (triggered-buff-duration buff)))
          (map (λ (buff) (make-triggered-buff (triggered-buff-effect buff)
                                              (triggered-buff-trigger buff)
                                              (triggered-buff-limit buff)
                                              (triggered-buff-party-wide buff)
                                              (- (triggered-buff-duration buff)
                                                 duration)))
               active-buffs*)))

;; determine the action the character is taking
(define (determine-action acc-values char attack)
  (cond [(list? attack) '()]
        [(or (symbol=? 'N attack) (symbol=? 'ND attack))
         (list-ref (attack-sequence-normals (flat-char-attacks char))
                   (sub1 (acc-data-nc acc-values)))]
        [(symbol=? 'C attack) (attack-sequence-charged (flat-char-attacks char))]
        [(symbol=? 'E attack) (flat-char-skill char)]
        [(symbol=? 'Q attack) (flat-char-burst char)]))

;; determine the type of the characters action
(define (determine-type attack action)
  (cond [(list? attack) '()]
        [(or (symbol=? 'N attack) (symbol=? 'ND attack) (symbol=? 'C attack))
         (attack-type action)]
        [(or (symbol=? 'E attack) (symbol=? 'Q attack))
         (flat-skill-type action)]))

(define (determine-enemy-element acc-values type attack)
  (define enemy-element (acc-data-enemy-element acc-values))
  (cond [(list? attack) '()]
        [(symbol=? type 'physical) enemy-element]
        [(symbol=? enemy-element 'none) type]
        [(symbol=? type enemy-element) enemy-element]
        [else 'none]))

;; determine the attribute of the character's action
(define (determine-action-attribute action attack)
  (cond [(list? attack) '()]
        [(or (symbol=? 'N attack) (symbol=? 'ND attack) (symbol=? 'C attack))
         (attack-attr action)]
        [(or (symbol=? 'E attack) (symbol=? 'Q attack))
         (flat-skill-attr action)]))

;; determine the damage of the action
(define (determine-damage acc-values char attack active-buffs* type attr)
  (define dmg (acc-data-dmg acc-values))
  (define enemy (acc-data-enemy acc-values))
  (define nc (acc-data-nc acc-values))
  (define enemy-element (acc-data-enemy-element acc-values))
  (if (list? attack)
      dmg
      (+ dmg
         ; stage 2, stage 3
         ;; generate-dmg-info is a complex way
         ;; to get all the modified stats + values needed to
         ;; calculate damage
         (calc-single-dmg (generate-dmg-info char
                                             enemy
                                             attack
                                             nc
                                             enemy-element
                                             active-buffs*
                                             type
                                             attr)))))
;; determine the next nc
(define (determine-next-nc acc-values char attack)
  (define nc (acc-data-nc acc-values))
  (if (and (symbol? attack) (symbol=? 'N attack))
      (if (= nc
             (length (attack-sequence-normals
                      (flat-char-attacks char))))
          1
          (+ 1 nc))
      1))

;; assign the necessary new values for the next attack and calculate damage
(define (update-and-calculate acc-values)
  ;; define struct data
  (define attack-string (acc-data-attack-string acc-values))
  (define dmg (acc-data-dmg acc-values))
  (define time (acc-data-time acc-values))
  (define team (acc-data-team acc-values))
  (define enemy (acc-data-enemy acc-values))
  (define cc (acc-data-cc acc-values))
  (define nc (acc-data-nc acc-values))
  (define active-buffs (acc-data-active-buffs acc-values))
  (define enemy-element (acc-data-enemy-element acc-values))
  ;; use the old values to calculate new values
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

;; main loop calculate the total damage and time taken for a rotation
(define (calc-dmg/acc acc-values) ; more acc args like buffs (later)
  ;; define struct data
  (define attack-string (acc-data-attack-string acc-values))
  (define dmg (acc-data-dmg acc-values))
  (define time (acc-data-time acc-values))
  (define team (acc-data-team acc-values))
  (define enemy (acc-data-enemy acc-values))
  (cond [(empty? attack-string)
         (save-entry (list current-atk-string dmg time enemy team))
         (list dmg time)]
        [(cons? attack-string)
         (update-and-calculate acc-values)]))

(define (attack->trigger attack)
  (match attack
    ['N 'normal-attack]
    ['C 'charged-attack]
    ['E 'skill]
    ['Q 'burst]
    [_ 'none]))

#|
(define (triggered->applied triggered)
  (make-applied-buff (triggered-buff-effect triggered)
                     (triggered-buff-limit triggered)
                     (triggered-buff-party-wide triggered)
                     (triggered-buff-duration triggered)))
|#

;; merge new buffs into existing buffs
(define (merge-buffs new old)
  (cond [(empty? new) old]
        [(cons? new) (merge-buffs (rest new) (merge-buff* (first new) old))]))

;; helper for merge-buffs
(define (merge-buff* new old)
  (cond [(empty? old) (list (make-triggered-buff (triggered-buff-effect new)
                                                 (triggered-buff-trigger new)
                                                 1
                                                 (triggered-buff-party-wide new)
                                                 (triggered-buff-duration new)))]
        [(cons? old) (if (triggered-buff=?* new (first old))
                         (cons (merge-buff new (first old)) (rest old))
                         (cons (first old) (merge-buff* new (rest old))))]))

;; performs the merge for a single buff
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

;; checks if 2 triggered buffs are the same
(define (triggered-buff=?* b1 b2)
  (and (equal? (triggered-buff-effect b1)
               (triggered-buff-effect b2))
       (equal? (triggered-buff-trigger b1)
               (triggered-buff-trigger b2))
       (equal? (triggered-buff-party-wide b1)
               (triggered-buff-party-wide b2))))

; stage 2
(define (generate-dmg-info char enemy attack nc enemy-element active-buffs type attr)
  (let ([stats (calc-total-stats char active-buffs)])
    ; c/e/q/nd
    ; calculate base dmg here + extra stats
    (make-damage-info (calc-base-attr attr stats)
                      1 ; base-dmg-mult ; look for dmg dmg% in buffs, which we dont have syntax for
                      0 ; base-add ; look for rest of dmg, which we dont have syntax for
                      1 ; dmg-mult ; elemental dmg bonus, which we dont have syntax for
                      1 ; def-mult ; relies on levels, which we dont have syntax for
                      (calc-res (enemy-res enemy) type) ; actual enemy res, figure out attack attr
                      (calc-amp-mult type enemy-element) ; amp-mult (reactions) (optional?)
                      (stat-info-critr stats)
                      (stat-info-critd stats))))

; calculate the stat change 
(define (calc-base-attr attr stats)
  ; ignore attribute-attr
  ; must be %?
  (* (percent-p (attribute-modifier attr))
     0.01
     (lookup-stat stats (percent-attr (attribute-modifier attr)))))

;; applies all buffs of a specified category
(define (apply-all-modifiers buffs base-stat-info attr-char symbol)
  (+ attr-char
     (apply + (map (λ (modifier) (calc-modifier modifier base-stat-info))
                   (get-mods-with-attr buffs symbol)))))

; do a tree traversal (abstract out, use symbols?) flat incr vs incr by other amt (atk (def% 50)) increases atk by 50% of base def
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
(define (calc-modifier modifier base-stats)
  (if (number? modifier)
      modifier
      (* (percent-p modifier)
         0.01
         (lookup-stat base-stats (percent-attr modifier)))))

; match a stat symbol to its proper stat info
(define (lookup-stat stats attr)
  (case attr
    ['atk% (stat-info-atk stats)]
    ['def% (stat-info-def stats)]
    ['hp% (stat-info-hp stats)]
    ['critr% (stat-info-critr stats)]
    ['critd% (stat-info-critd stats)]
    ['em (stat-info-em stats)]
    ))

; calculate the resistance to a given element
(define (calc-res ress type)
  (let ([res (/ (lookup-res ress type) 100)])
    (cond [(< res 0) (- 1 (/ res 2))]
          [(>= res 0.75) (/ 1 (+ (* 4 res) 1))]
          [else (- 1 res)])))

; match an element symbol to its proper resistance
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
(define (calc-amp-mult attack-type enemy-element)
  (match `(,attack-type ,enemy-element)
    ['(pyro cryo) 2]
    ['(cryo pyro) 1.5]
    ['(hydro pyro) 2]
    ['(pyro hydro) 1.5]
    [_ 1]))

; final calculation for a single attack
(define (calc-single-dmg dmg)
  (* (+ (* (damage-info-base-dmg dmg)
           (damage-info-base-dmg-mult dmg))
        (damage-info-base-add dmg))
     (damage-info-dmg-mult dmg)
     (damage-info-def-mult dmg)
     (damage-info-res dmg)
     (damage-info-amp-mult dmg)
     (calc-crit (min (damage-info-critr dmg) 100) (damage-info-critd dmg))))

; determine the average bonus crit damage
(define (calc-crit crit-rate crit-dmg)
  (+ (- 1 (/ crit-rate 100))
     (* (/ crit-rate 100)
        (+ 1 (/ crit-dmg 100)))))