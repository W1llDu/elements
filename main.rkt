#lang racket

;; ELEMENTS
;; "Extensive
;;  Language for the
;;  Efficient
;;  Monitoring of
;;  Effective
;;  Numerical
;;  Team-wide
;;  Statistics”
;;  ...
;; (which includes damage)
;; (Note: Not everything needs to be an acronym)

#| READ ME
  This file defines the syntax for the DSL,
  which includes a syntax-spec implementation +
  macros for defining various objects, like weapons
  or skills. Additionally, we use this file to implement
  compile time error checks, to make sure the proper
  items are added to the proper places, and to prevent
  duplicate definitions from showing up at compile time.
|#

(require (for-syntax syntax/parse))
(require syntax-spec-v3 (for-syntax syntax/parse syntax/to-string))
(require "private/runtime.rkt")
(require racket/hash)

(provide (all-defined-out))

(begin-for-syntax

  (define-syntax-class stat
    (pattern flat:flat-stat)
    (pattern base:base-stat)
    (pattern percent:percent-stat))
  
  (define-syntax-class flat-stat
    (pattern (~datum critr))
    (pattern (~datum critd))
    (pattern (~datum dmg%)))

  (define-syntax-class base-stat
    (pattern (~datum hp))
    (pattern (~datum atk))
    (pattern (~datum def))
    (pattern (~datum em)))

  (define-syntax-class percent-stat
    (pattern (~datum hp%))
    (pattern (~datum atk%))
    (pattern (~datum def%))
    (pattern (~datum em%))))

(syntax-spec

 (extension-class genshin-macro #:binding-space genshin)
 (binding-class weapon-bind #:description "weapon" #:binding-space genshin)
 (binding-class skill-bind #:description "skill" #:binding-space genshin)
 (binding-class artifact-bind #:description "artifact" #:binding-space genshin)
 (binding-class attack-sequence-bind #:description "attack sequence" #:binding-space genshin)
 (binding-class character-bind #:description "character" #:binding-space genshin)
 (binding-class enemy-bind #:description "enemy" #:binding-space genshin)
 (binding-class team-lineup-bind #:description "team lineup" #:binding-space genshin)

 (host-interface/definitions
  (define-weapon name:weapon-bind
    damage:number
    attr:modifier-attribute
    buffs:buff ...)
  #:binding (export name)
  #'(compile-define-weapon
     name
     damage
     attr
     buffs ...))

 (host-interface/definitions
  (define-skill name:skill-bind
    #:cooldown cd:number
    #:attr attr:damage-attribute
    #:duration duration:number
    #:type type:element
    buffs:trigger-buff ...)
  #:binding (export name)
  #'(compile-define-skill
     name
     #:cooldown cd
     #:attr attr
     #:duration duration
     #:type type
     buffs ...))

 (host-interface/definitions
  (define-attack-sequence
    name:attack-sequence-bind
    ([attr:damage-attribute duration:number type:element] ...
     #:charged  [attr2:damage-attribute duration2:number type2:element]
     #:plunging [attr3:damage-attribute duration3:number type3:element]))
  #:binding (export name)
  #'(compile-define-attack-sequence
     name ([attr duration type] ...
           #:charged  [attr2 duration2 type2]
           #:plunging [attr3 duration3 type3])))
 
 (host-interface/definitions
  (define-artifact
    name:artifact-bind
    set-name:string
    mattr:modifier-attribute
    sattr:modifier-attribute ...)
  #:binding (export name)
  #'(compile-define-artifact
     name
     set-name
     mattr
     sattr ...))

 (host-interface/definitions
  (define-character
    name:character-bind
    #:hp hp:number #:def def:number #:atk atk:number
    #:em em:number #:critr critr:number #:critd critd:number
    #:attacks attacks:attack-sequence-bind #:weapon weapon:weapon-bind #:skill skill:skill-bind
    #:burst burst:skill-bind #:artifacts artifacts:artifact-bind ...)
  #:binding (export name)
  #'(compile-define-character
     name
     #:hp hp #:def def #:atk atk
     #:em em #:critr critr #:critd critd
     #:attacks attacks #:weapon weapon #:skill skill
     #:burst burst #:artifacts artifacts ...))

 (host-interface/definitions
  (define-enemy
    name:enemy-bind #:def def:number
    #:res (#:pyro pyro:number
           #:hydro hydro:number
           #:electro electro:number
           #:cryo cryo:number
           #:geo geo:number
           #:anemo anemo:number
           #:dendro dendro:number
           #:physical physical:number)
    #:reduction reduction:number)
  #:binding (export name)
  #'(compile-define-enemy
     name #:def def
     #:res (#:pyro pyro
            #:hydro hydro
            #:electro electro
            #:cryo cryo
            #:geo geo
            #:anemo anemo
            #:dendro dendro
            #:physical physical)
     #:reduction reduction))
 
 (host-interface/definitions
  (define-team-lineup name:team-lineup-bind (chars:character-bind ...))
  #:binding (export name)
  #'(compile-define-team-lineup name (chars ...)))
 
 (nonterminal attack-key
              N
              C
              E
              Q
              ND
              (Swap Int:integer))

 (nonterminal element
              pyro
              hydro
              cryo
              electro
              geo
              anemo
              dendro
              physical)

 (nonterminal buff-attribute
    #:description "buff attribute"
    (buff attr:stat value:number)
    (buff attr:base-stat (sattr:percent-stat percent:number))
    (buff attr:flat-stat (sattr:percent-stat percent:number)))

 (nonterminal modifier-attribute
   #:description "modifier attribute"
   (mod attr:stat value:number))

 (nonterminal damage-attribute
   #:description "damage attribute"
   (dmg attr:percent-stat percent:number))

 (nonterminal trigger
              normal-attack
              charged-attack
              skill
              burst)

 (nonterminal buff
              (unconditional-buff [name:id #:effect attr:buff-attribute
                                           #:party-wide party-wide:boolean])
              trigger:trigger-buff)
              

 (nonterminal trigger-buff
              (triggered-buff [name:id #:effect attr:buff-attribute
                                       #:trigger t:trigger
                                       #:limit limit:number
                                       #:party-wide party-wide:boolean
                                       #:duration duration:number])
              (applied-buff [name:id #:effect attr:buff-attribute
                                     #:limit limit:number
                                     #:party-wide party-wide:boolean
                                     #:duration duration:number]))

 (host-interface/expression
  (calculate-rotation-damage lineup:team-lineup-bind enemy:enemy-bind (attk:attack-key ...))
  #'(compile-calculate-rotation-damage
     lineup
     enemy
     (attk ...)))
 (host-interface/expression
  (calculate-raw-rotation-damage lineup:team-lineup-bind enemy:enemy-bind (attk:attack-key ...))
  #'(compile-calculate-raw-rotation-damage
     lineup
     enemy
     (attk ...)))

 )

;; allows a proper sequencing of DSL code
(define-syntax genshin-calc
  (lambda (stx)
    (syntax-parse stx
      [(_ exprs ...)
       (check-duplicate-names (attribute exprs))
       #'(let () (display "Thanks for using ELEMENTS!\n\n") exprs ...)])))
#|
   #:attacks
   #:weapon
   #:skill/#:burst
   #:artifacts
char
enemy
|#
(begin-for-syntax
  (require racket/list)

  ;; helper to check for duplicate names in the hash table
  (define (check-duplicates name result expr symbol)
    (when (member (syntax->datum name) (apply append (hash-values result)))
      (raise-syntax-error symbol "a duplicate name was found" expr name)))

  ;; helper to  update the hash table storing names
  (define (update-hash symbol name result)
    (hash-set result
              symbol
              (cons (syntax->datum name)
                    (hash-ref result symbol))))

  (define (check-duplicate-names exprs)
    (foldl (λ (expr result)
             (if (hash? result)
                 (syntax-parse expr
                   [((~datum define-attack-sequence) name rest ...)
                    (check-duplicates #'name result expr 'define-attack-sequence)
                    (update-hash 'attacks #'name result)]
                   [((~datum define-weapon) name rest ...)
                    (check-duplicates #'name result expr 'define-weapon)
                    (update-hash 'weapons #'name result)]
                   [((~datum define-skill) name rest ...)
                    (check-duplicates #'name result expr 'define-skill)
                    (update-hash 'skills #'name result)]
                   [((~datum define-artifact) name rest ...)
                    (check-duplicates #'name result expr 'define-artifact)
                    (update-hash 'artifacts #'name result)]
                   [((~datum define-character)
                     name _ _ _ _ _ _ _ _ _ _ _ _ _
                     attacks*:id _
                     weapon*:id _
                     skill*:id _
                     burst*:id _
                     artifacts* ...)
                    (check-duplicates #'name result expr 'define-character)
                    (update-hash 'characters #'name result)]
                   [((~datum define-enemy) name rest ...)
                    (check-duplicates #'enemy result expr 'define-enemy)
                    (update-hash 'enemies #'name result)]
                   [((~datum define-team-lineup) name (chars ...))
                    (check-duplicates #'team result expr 'define-team-lineup)
                    (update-hash 'teams #'name result)]
                   [((~datum calculate-rotation-damage) team* enemy* _) #t]
                   [((~datum calculate-raw-rotation-damage) team* enemy* _) #t]) ;; nothing to check
                 result))
           (hash 'attacks (list)
                 'weapons (list)
                 'skills (list)
                 'artifacts (list)
                 'characters (list)
                 'enemies (list)
                 'teams (list))
           exprs))
  )

(define-syntax parse-attribute
  (lambda (stx)
    (syntax-parse stx
      ; flat stat
      #;[(_ stat amount:number)
         #'(make-attribute 'stat amount)]
      [(_ (~datum hp) amount:number)
       #'(make-attribute 'hp amount)]
      [(_ (~datum atk) amount:number)
       #'(make-attribute 'atk amount)]
      [(_ (~datum def) amount:number)
       #'(make-attribute 'def amount)]
      [(_ (~datum critr) amount:number)
       #'(make-attribute 'critr amount)]
      [(_ (~datum critd) amount:number)
       #'(make-attribute 'critd amount)]
      [(_ (~datum em) amount:number)
       #'(make-attribute 'em amount)]
      ; flat stat by another stat percent
      ; how to enforce that stat is hp%/atk%/def%/em?
      #; (hp (atk% 20))
      [(_ (~datum hp) (stat percent:number))
       #'(make-attribute 'hp (make-percent 'stat percent))]
      [(_ (~datum atk) (stat percent:number))
       #'(make-attribute 'atk (make-percent 'stat percent))]
      [(_ (~datum def) (stat percent:number))
       #'(make-attribute 'def (make-percent 'stat percent))]
      [(_ (~datum em) (stat percent:number))
       #'(make-attribute 'em (make-percent 'stat percent))]
      ; stat by stat percent (sugar)  (hp% 20) = (hp (hp% 20))
      [(_ (~datum hp%) percent:number)
       #'(make-attribute 'hp (make-percent 'hp% percent))]
      [(_ (~datum atk%) percent:number)
       #'(make-attribute 'atk (make-percent 'atk% percent))]
      [(_ (~datum def%) percent:number)
       #'(make-attribute 'def (make-percent 'def% percent))]
      )))

(define-syntax compile-define-weapon
  (lambda (stx)
    (syntax-parse stx
      [(_ name:id atk:number ((~datum mod) attr modifier) buffs ...)
       #'(define name (make-weapon atk
                                   (parse-attribute attr modifier)
                                   (list (parse-buff buffs) ...)))])))

(define-syntax compile-define-skill
  (lambda (stx)
    (syntax-parse stx
      [(_ name:id
          #:cooldown cd:number
          #:attr ((~datum dmg) attr modifier)
          #:duration duration:number
          #:type type buffs ...)
       #'(define name (make-skill cd
                                  (parse-attribute attr modifier)
                                  duration 'type
                                  (list (parse-buff buffs) ...)))])))

(define-syntax parse-buff
  (lambda (stx)
    (syntax-parse stx
      [(_ ((~datum triggered-buff) [name:id
                                    #:effect ((~datum buff) attr percent)
                                    #:trigger trigger
                                    #:limit limit:number
                                    #:party-wide party-wide:boolean
                                    #:duration duration]))
       #'(make-triggered-buff (parse-attribute attr percent)
                              'trigger
                              limit
                              party-wide
                              duration)]
      [(_ ((~datum unconditional-buff) [name:id
                                        #:effect ((~datum buff) attr percent)
                                        #:party-wide party-wide:boolean]))
       #'(make-unconditional-buff (parse-attribute attr percent) party-wide)]
      [(_ ((~datum applied-buff) [name:id
                                  #:effect ((~datum buff) attr percent)
                                  #:limit limit:number
                                  #:party-wide party-wide:boolean
                                  #:duration duration]))
       #'(make-applied-buff (parse-attribute attr percent)
                            limit
                            party-wide
                            duration)])))

(define-syntax compile-define-attack-sequence
  (lambda (stx)
    (syntax-parse stx
      [(_ name:id ([((~datum dmg) attr percent:number) duration:number type] ...
                   #:charged  [((~datum dmg) attr2 percent2:number)
                               duration2:number
                               type2]
                   #:plunging [((~datum dmg) attr3 percent3:number)
                               duration3:number
                               type3]))
       #'(define name (make-attack-sequence
                       (list (make-attack (parse-attribute attr percent)
                                          duration
                                          'type) ...)
                       (make-attack (parse-attribute attr2 percent2)
                                    duration2
                                    'type2)
                       (make-attack (parse-attribute attr3 percent3)
                                    duration3
                                    'type3)))])))

(define-syntax compile-define-artifact
  (lambda (stx)
    (syntax-parse stx
      [(_ name:id set-name:string ((~datum mod) mattr mstat) ((~datum mod) sattr sstat) ...)
       #'(define name
           (make-artifact set-name (parse-attribute mattr mstat)
                          (list (parse-attribute sattr sstat) ...)))])))

(define-syntax compile-define-character
  (lambda (stx)
    (syntax-parse stx
      [(_ name:id
          #:hp hp:number #:def def:number #:atk atk:number
          #:em em:number #:critr critr:number #:critd critd:number
          #:attacks attacks:id #:weapon weapon:id #:skill skill:id
          #:burst burst:id #:artifacts artifacts:id ...)
       #'(define name (make-character hp def atk em critr critd
                                      attacks weapon skill burst
                                      (list artifacts ...)))])))

(define-syntax compile-define-enemy
  (lambda (stx)
    (syntax-parse stx
      [(_ name:id #:def def:number
          #:res (#:pyro pyro:number
                 #:hydro hydro:number
                 #:electro electro:number
                 #:cryo cryo:number
                 #:geo geo:number
                 #:anemo anemo:number
                 #:dendro dendro:number
                 #:physical physical:number)
          #:reduction reduction:number)
       #'(define name
           (make-enemy def
                       (make-resistances pyro hydro electro cryo
                                         geo anemo dendro physical)
                       reduction))])))

(define-syntax compile-define-team-lineup
  (lambda (stx)
    (syntax-parse stx
      [(_ name:id (chars:id ...))
       #'(define name (list chars ...))])))

(define-syntax compile-calculate-rotation-damage
  (lambda (stx)
    (syntax-parse stx
      [(_ lineup:id enemy:id (attack-string ...))
       #'(display-data lineup enemy (list 'attack-string ...))])))

(define-syntax compile-calculate-raw-rotation-damage
  (lambda (stx)
    (syntax-parse stx
      [(_ lineup:id enemy:id (attack-string ...))
       #'(calc-dmg lineup enemy (list 'attack-string ...))])))

(define-syntax clear-data-file
  (lambda (stx)
    (syntax-parse stx
      [(_) #'(clear-file)])))






