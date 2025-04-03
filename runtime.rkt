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

(define (save-entry entry)
  (call-with-output-file "data.txt"
    (lambda (out)
      (write entry out) 
      (newline out))   
    #:exists 'append))

(define (load-entries)
  (with-handlers ([exn:fail? (lambda (_) '())])
    (call-with-input-file "data.txt"
      (lambda (in)
        (let loop ([entries '()])
          (let ([line (read in)])
            (if (eof-object? line)
                (reverse entries)
                (loop (cons line entries)))))))))

(define (clear-file)
  (call-with-output-file "data.txt"
    (lambda (out) (void))
    #:exists 'truncate))

(define current-atk-string '())

(define (calc-dmg team enemy attack-string)
  ; pull out party-wide uncond buffs into active-buffs
  (set! current-atk-string attack-string)
  (calc-dmg/acc (map flatten-char team) enemy 1 attack-string 1 empty 'none 0 0))

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
                  ; TODO
                  (append (filter unconditional-buff? (append (weapon-buffs (character-weapon char))
                                                              (skill-buffs (character-skill char))
                                                              (skill-buffs (character-burst char))))
                          (apply append (map (λ (artifact) (cons (attr->unconditional (artifact-main-stat artifact))
                                                                 (map attr->unconditional (artifact-substats artifact))))
                                             (character-artifacts char))))
                  ; TODO
                  (append (filter triggered-buff? (append (weapon-buffs (character-weapon char))
                                                          (skill-buffs (character-skill char))
                                                          (skill-buffs (character-burst char))))
                          (map (λ (buff) (applied->triggered buff 'normal-attack)) (filter applied-buff? (weapon-buffs (character-weapon char))))
                          (map (λ (buff) (applied->triggered buff 'charged-attack)) (filter applied-buff? (weapon-buffs (character-weapon char))))
                          (map (λ (buff) (applied->triggered buff 'skill)) (filter applied-buff? (skill-buffs (character-skill char))))
                          (map (λ (buff) (applied->triggered buff 'burst)) (filter applied-buff? (skill-buffs (character-burst char)))))
                  ; weapon trigger: on normal&charged
                  ; skill/burst trigger: on skill/burst
                  ))

(define (flatten-skill skill)
  (make-flat-skill (skill-cd skill) (skill-attr skill) (skill-duration skill) (skill-type skill)))


(define (determine-current-optimal data-list curr-min team enemy)
  (cond [(empty? data-list) curr-min]
        [else (define entry (first data-list))
              (define dps (/ (second entry)
                             (third entry)))
              (if (and (or (empty? curr-min) (> dps (/ (second curr-min)
                                                       (third curr-min))))
                       (equal? (fourth entry) team)
                       (equal? (fifth entry) enemy))
                  (determine-current-optimal (rest data-list) entry team enemy)
                  (determine-current-optimal (rest data-list) curr-min team enemy))]))
(define (attr->unconditional attr)
  (make-unconditional-buff attr #f))


(define (applied->triggered applied trigger)
  (make-triggered-buff (applied-buff-effect applied)
                       trigger
                       (applied-buff-limit applied)
                       (applied-buff-party-wide applied)
                       (applied-buff-duration applied)))


(define (calc-dmg/acc team enemy cc attack-string nc active-buffs enemy-element dmg time) ; more acc args like buffs (later)
  (cond [(empty? attack-string)
         (save-entry (list current-atk-string dmg time enemy team))
         (define string-data (last (load-entries)))
         (display (list dmg time))
         (display "\n")
         (define best (determine-current-optimal (load-entries)
                                                 (list)
                                                 (fourth string-data)
                                                 (fifth string-data)))
         (display (format "best run with this team was a sequence of: ~a with a dps of: ~a\n"
                          (first best)
                          (/ (second best) (third best))))]
        [(cons? attack-string) (let* ([attack (first attack-string)]
                                      [char (list-ref team (- cc 1))]
                                      [nc* (if (and (symbol? attack) (symbol=? 'N attack))
                                               (if (= nc (length (attack-sequence-normals (flat-char-attacks char))))
                                                   1
                                                   (+ 1 nc))
                                               1)]
                                      [duration (calc-duration attack nc char)]
                                      ; add newly triggered buffs
                                      [active-buffs* (merge-buffs (filter (λ (buff) (symbol=? (triggered-buff-trigger buff)
                                                                                              (attack->trigger attack)))
                                                                          (flat-char-trigger-buffs char))
                                                                  active-buffs)]
                                      ; remove buffs that, after duration passes, will expire
                                      [active-buffs** (filter (λ (buff) (< 0 (triggered-buff-duration buff)))
                                                              (map (λ (buff) (make-triggered-buff (triggered-buff-effect buff)
                                                                                                  (triggered-buff-trigger buff)
                                                                                                  (triggered-buff-limit buff)
                                                                                                  (triggered-buff-party-wide buff)
                                                                                                  (- (triggered-buff-duration buff) duration)))
                                                                   active-buffs*))])
                                 (cond
                                   ; swap
                                   [(list? attack)
                                    (if (= cc (second attack)) ; swapping to same char (do nothing)
                                        (calc-dmg/acc team
                                                      enemy
                                                      cc
                                                      (rest attack-string)
                                                      nc
                                                      active-buffs
                                                      enemy-element
                                                      dmg
                                                      time)
                                        (calc-dmg/acc team
                                                      enemy
                                                      (second attack)
                                                      (rest attack-string)
                                                      nc*
                                                      active-buffs** ; TODO: remove non-teamwide buffs
                                                      enemy-element
                                                      dmg
                                                      (+ time duration)))]
                                   ; apply buffs
                                   ; attack is done, then buffs
                                   [(symbol=? 'N attack)
                                    (let ([na (list-ref (attack-sequence-normals (flat-char-attacks char)) (sub1 nc))])
                                      (calc-dmg/acc team
                                                    enemy
                                                    cc
                                                    (rest attack-string)
                                                    nc*
                                                    active-buffs** ; add applied buffs TODO
                                                    (cond [(symbol=? (attack-type na) 'none) enemy-element]
                                                          [(symbol=? enemy-element 'none) (attack-type na)]
                                                          [(symbol=? (attack-type na) enemy-element) enemy-element]
                                                          [else 'none]) ; assume reaction
                                                    (+ dmg
                                                       (calc-single-dmg (generate-dmg-info char
                                                                                           enemy
                                                                                           attack
                                                                                           nc
                                                                                           enemy-element
                                                                                           active-buffs*))) ; middle buffs
                                                    (+ time duration)))]
                                   [(symbol=? 'C attack)
                                    (let ([na (attack-sequence-charged (flat-char-attacks char))])
                                      (calc-dmg/acc team
                                                    enemy
                                                    cc
                                                    (rest attack-string)
                                                    nc*
                                                    active-buffs** ; add applied buffs TODO
                                                    (cond [(symbol=? (attack-type na) 'none) enemy-element]
                                                          [(symbol=? enemy-element 'none) (attack-type na)]
                                                          [(symbol=? (attack-type na) enemy-element) enemy-element]
                                                          [else 'none]) ; assume reaction
                                                    (+ dmg
                                                       (calc-single-dmg (generate-dmg-info char
                                                                                           enemy
                                                                                           attack
                                                                                           nc
                                                                                           enemy-element
                                                                                           active-buffs*))) ; middle buffs
                                                    (+ time duration)))]
                                   [(symbol=? 'E attack)
                                    (let ([skill (flat-char-skill char)])
                                      (calc-dmg/acc team
                                                    enemy
                                                    cc
                                                    (rest attack-string)
                                                    nc*
                                                    active-buffs** ; add applied buffs TODO
                                                    (cond [(symbol=? (flat-skill-type skill) 'none) enemy-element]
                                                          [(symbol=? enemy-element 'none) (flat-skill-type skill)]
                                                          [(symbol=? (flat-skill-type skill) enemy-element) enemy-element]
                                                          [else 'none]) ; assume reaction
                                                    (+ dmg
                                                       (calc-single-dmg (generate-dmg-info char
                                                                                           enemy
                                                                                           attack
                                                                                           nc
                                                                                           enemy-element
                                                                                           active-buffs*))) ; middle buffs
                                                    (+ time duration)))]
                                   [(symbol=? 'Q attack)
                                    (let ([burst (flat-char-burst char)])
                                      (calc-dmg/acc team
                                                    enemy
                                                    cc
                                                    (rest attack-string)
                                                    nc*
                                                    active-buffs** ; add applied buffs TODO
                                                    (cond [(symbol=? (flat-skill-type burst) 'none) enemy-element]
                                                          [(symbol=? enemy-element 'none) (flat-skill-type burst)]
                                                          [(symbol=? (flat-skill-type burst) enemy-element) enemy-element]
                                                          [else 'none]) ; assume reaction
                                                    (+ dmg
                                                       (calc-single-dmg (generate-dmg-info char
                                                                                           enemy
                                                                                           attack
                                                                                           nc
                                                                                           enemy-element
                                                                                           active-buffs*))) ; middle buffs
                                                    (+ time duration)))]
                                   ; TODO: nd
                                   ))]))

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

(define (merge-buffs new old)
  (cond [(empty? new) old]
        [(cons? new) (merge-buffs (rest new) (merge-buff* (first new) old))]))

(define (merge-buff* new old)
  (cond [(empty? old) (cons new old)]
        [(cons? old) (if (triggered-buff=?* new (first old))
                         (cons (merge-buff new (first old)) (rest old))
                         (cons (first old) (merge-buff* new (rest old))))]))

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

(define (triggered-buff=?* b1 b2)
  (and (equal? (triggered-buff-effect b1) (triggered-buff-effect b2))
       (equal? (triggered-buff-trigger b1) (triggered-buff-trigger b2))
       (equal? (triggered-buff-party-wide b1) (triggered-buff-party-wide b2))))

; TODO: INLINE THIS
(define (calc-duration attack nc char)
  ; c/e/q/nd
  (cond [(list? attack) 1]
        [(symbol=? 'N attack) (attack-duration (list-ref (attack-sequence-normals (flat-char-attacks char)) (sub1 nc)))]
        [(symbol=? 'C attack) (attack-duration (attack-sequence-charged (flat-char-attacks char)))]
        [(symbol=? 'E attack) (flat-skill-duration (flat-char-skill char))]
        [(symbol=? 'Q attack) (flat-skill-duration (flat-char-burst char))]
        [(symbol=? 'ND attack) 0.1]; TODO
        ))

(define (generate-dmg-info char enemy attack nc enemy-element buffs)
  (let ([stats (calc-total-stats char buffs)])
    ; c/e/q/nd
    ; calculate base dmg here
    (cond [(symbol=? 'N attack) (let ([attack (list-ref (attack-sequence-normals (flat-char-attacks char)) (sub1 nc))])
                                  (make-damage-info (calc-base-attr (attack-attr attack) stats)
                                                    ; should be in terms of total atk, not base
                                                    1 ; base-dmg-mult ; look for dmg dmg% in buffs TODO
                                                    0 ; base-add ; look for rest of dmg TODO
                                                    1 ; dmg-mult ; elemental dmg bonus, which we dont have syntax for yet TODO
                                                    1 ; def-mult ; relies on levels, which we dont have syntax for yet TODO
                                                    (calc-res (enemy-res enemy) (attack-type attack)) ; actual enemy res, figure out attack attr
                                                    (calc-amp-mult (attack-type attack) enemy-element) ; amp-mult (reactions) (optional?)
                                                    (stat-info-critr stats)
                                                    (stat-info-critd stats)))]
          [(symbol=? 'C attack) (let ([attack (attack-sequence-charged (flat-char-attacks char))])
                                  (make-damage-info (calc-base-attr (attack-attr attack) stats)
                                                    ; should be in terms of total atk, not base
                                                    1 ; base-dmg-mult ; look for dmg dmg% in buffs TODO
                                                    0 ; base-add ; look for rest of dmg TODO
                                                    1 ; dmg-mult ; elemental dmg bonus, which we dont have syntax for yet TODO
                                                    1 ; def-mult ; relies on levels, which we dont have syntax for yet TODO
                                                    (calc-res (enemy-res enemy) (attack-type attack)) ; actual enemy res, figure out attack attr
                                                    (calc-amp-mult (attack-type attack) enemy-element) ; amp-mult (reactions) (optional?)
                                                    (stat-info-critr stats)
                                                    (stat-info-critd stats)))]
          [(symbol=? 'E attack) (make-damage-info (calc-base-attr (flat-skill-attr (flat-char-skill char)) stats)
                                                  ; should be in terms of total atk, not base
                                                  1 ; base-dmg-mult ; look for dmg dmg% in buffs TODO
                                                  0 ; base-add ; look for rest of dmg TODO
                                                  1 ; dmg-mult ; elemental dmg bonus, which we dont have syntax for yet TODO
                                                  1 ; def-mult ; relies on levels, which we dont have syntax for yet TODO
                                                  (calc-res (enemy-res enemy) (flat-skill-type (flat-char-skill char))) ; actual enemy res, figure out attack attr
                                                  (calc-amp-mult (flat-skill-type (flat-char-skill char)) enemy-element) ; amp-mult (reactions) (optional?)
                                                  (stat-info-critr stats)
                                                  (stat-info-critd stats))]
          [(symbol=? 'Q attack) (make-damage-info (calc-base-attr (flat-skill-attr (flat-char-burst char)) stats)
                                                  ; should be in terms of total atk, not base
                                                  1 ; base-dmg-mult ; look for dmg dmg% in buffs TODO
                                                  0 ; base-add ; look for rest of dmg TODO
                                                  1 ; dmg-mult ; elemental dmg bonus, which we dont have syntax for yet TODO
                                                  1 ; def-mult ; relies on levels, which we dont have syntax for yet TODO
                                                  (calc-res (enemy-res enemy) (flat-skill-type (flat-char-burst char))) ; actual enemy res, figure out attack attr
                                                  (calc-amp-mult (flat-skill-type (flat-char-burst char)) enemy-element) ; amp-mult (reactions) (optional?)
                                                  (stat-info-critr stats)
                                                  (stat-info-critd stats))]
          )))

; do a tree traversal (abstract out, use symbols?) flat incr vs incr by other amt (atk (def% 50)) increases atk by 50% of base def
(define (calc-total-stats char buffs)
  ; remember to scale % to decimal
  ; % is already desugared
  (let ([buffs (append (flat-char-unconditional-buffs char) buffs)]
        [base-stat-info (make-stat-info (flat-char-hp char)
                                        (flat-char-atk char)
                                        (flat-char-def char)
                                        (flat-char-critr char)
                                        (flat-char-critd char)
                                        (flat-char-em char))])
    (make-stat-info (+ (flat-char-hp char) ; base hp
                       (apply + (map (λ (modifier) (calc-modifier modifier base-stat-info)) (get-buffs-with-attr buffs 'hp))))
                    (+ (flat-char-atk char) ; base atk
                       (apply + (map (λ (modifier) (calc-modifier modifier base-stat-info)) (get-buffs-with-attr buffs 'atk))))
                    (+ (flat-char-def char) ; base def
                       (apply + (map (λ (modifier) (calc-modifier modifier base-stat-info)) (get-buffs-with-attr buffs 'def))))
                    (+ (flat-char-critr char)
                       (apply + (map (λ (modifier) (calc-modifier modifier base-stat-info)) (get-buffs-with-attr buffs 'critr))))
                    (+ (flat-char-critd char)
                       (apply + (map (λ (modifier) (calc-modifier modifier base-stat-info)) (get-buffs-with-attr buffs 'critd))))
                    (+ (flat-char-em char)
                       (apply + (map (λ (modifier) (calc-modifier modifier base-stat-info)) (get-buffs-with-attr buffs 'em)))))))

(define (get-buffs-with-attr buffs attr)
  ; convert to modifier
  (map (λ (buff) (cond [(unconditional-buff? buff) (attribute-modifier (unconditional-buff-effect buff))]
                       [(triggered-buff? buff) (attribute-modifier (triggered-buff-effect buff))]))
       (filter (λ (buff) (cond [(unconditional-buff? buff) (symbol=? (attribute-attr (unconditional-buff-effect buff)) attr)]
                               [(triggered-buff? buff) (symbol=? (attribute-attr (triggered-buff-effect buff)) attr)]))
               buffs)))
  

(define (calc-base-attr attr stats)
  ; ignore attribute-attr
  ; must be %?
  (* (percent-p (attribute-modifier attr)) 0.01 (lookup-stat stats (percent-attr (attribute-modifier attr)))))
  
; TODO (unused?)
(define (calc-attr attr base-stats)
  (cond
    ; must be %?
    [(symbol=? (attribute-attr attr) 'atk) (calc-modifier (attribute-modifier attr) base-stats)]
    ))

(define (calc-modifier modifier base-stats)
  (if (number? modifier)
      modifier
      (* (percent-p modifier) 0.01 (lookup-stat base-stats (percent-attr modifier)))))

(define (lookup-stat stats attr)
  (cond
    [(symbol=? attr 'atk%) (stat-info-atk stats)]
    [(symbol=? attr 'def%) (stat-info-def stats)]
    [(symbol=? attr 'hp%) (stat-info-hp stats)]
    [(symbol=? attr 'critr%) (stat-info-critr stats)]
    [(symbol=? attr 'critd%) (stat-info-critd stats)]
    [(symbol=? attr 'em) (stat-info-em stats)]
    ))

(define (calc-res ress type)
  (let ([res (/ (lookup-res ress type) 100)])
    (cond [(< res 0) (- 1 (/ res 2))]
          [(>= res 0.75) (/ 1 (+ (* 4 res) 1))]
          [else (- 1 res)])))

(define (lookup-res ress type)
  (cond
    [(symbol=? type 'pyro) (resistances-pyro ress)]
    [(symbol=? type 'hydro) (resistances-hydro ress)]
    [(symbol=? type 'electro) (resistances-electro ress)]
    [(symbol=? type 'cryo) (resistances-cryo ress)]
    [(symbol=? type 'geo) (resistances-geo ress)]
    [(symbol=? type 'anemo) (resistances-anemo ress)]
    [(symbol=? type 'dendro) (resistances-dendro ress)]
    [(symbol=? type 'physical) (resistances-physical ress)]
    ))

; amplifying reactions (transformatives DNE)
(define (calc-amp-mult attack-type enemy-element)
  (match `(,attack-type ,enemy-element)
    ['(pyro cryo) 2]
    ['(cryo pyro) 1.5]
    ['(hydro pyro) 2]
    ['(pyro hydro) 1.5]
    [_ 1]))

; final calculation
(define (calc-single-dmg dmg)
  (* (+ (* (damage-info-base-dmg dmg) (damage-info-base-dmg-mult dmg)) (damage-info-base-add dmg))
     (damage-info-dmg-mult dmg)
     (damage-info-def-mult dmg)
     (damage-info-res dmg)
     (damage-info-amp-mult dmg)
     (calc-crit (min (damage-info-critr dmg) 100) (damage-info-critd dmg))))

(define (calc-crit crit-rate crit-dmg)
  (+ (- 1 (/ crit-rate 100))
     (* (/ crit-rate 100)
        (+ 1 (/ crit-dmg 100)))))