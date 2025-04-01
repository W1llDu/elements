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

(require (for-syntax syntax/parse))
(require syntax-spec-v3 (for-syntax syntax/parse syntax/to-string))
(require "runtime.rkt")
(require racket/hash)

(provide (all-defined-out))


(syntax-spec

 (extension-class genshin-macro #:binding-space genshin)
 
 (host-interface/definitions
  (genshin-calc exprs:expr ...)
  #'(compile-genshin-calc exprs ...))
 
 (host-interface/definitions
  (define-weapon name:id
    damage:number
    attr:genshin-attribute
    buffs:buff ...)
  #'(compile-define-weapon
     name
     damage
     attr
     buffs ...))

 (host-interface/definitions
  (define-skill name:id
    cd:number
    #:attr attr:genshin-attribute
    #:duration duration:number
    #:type type:element
    buffs:buff ...)
  #'(compile-define-skill
     name
     cd
     #:attr attr
     #:duration duration
     #:type type
     buffs ...))

 (host-interface/definitions
  (define-attack-sequence
    name:id
    ([attr:genshin-attribute duration:number type:element] ...
     #:charged  [attr2:genshin-attribute duration2:number type2:element]
     #:plunging [attr3:genshin-attribute duration3:number type3:element]))
  #'(compile-define-attack-sequence
     name ([attr duration type] ...
           #:charged  [attr2 duration2 type2]
           #:plunging [attr3 duration3 type3])))
 
 (host-interface/definitions
  (define-artifact
    name:id
    set-name:string
    mattr:genshin-attribute
    sattr:genshin-attribute ...)
  #'(compile-define-artifact
     name
     set-name
     mattr
     sattr ...))

 (nonterminal stat
              hp
              atk
              def
              em
              critr
              critd
              hp%
              atk%
              def%)

 (nonterminal element
              pyro
              hydro
              cryo
              electro
              gro
              anemo
              dendro
              physical)

 (nonterminal genshin-attribute
              (attr:stat mod:modifier))

 (nonterminal modifier
              flat:number
              (attr:stat percent:number))

 (nonterminal buff
              #:binding-space genshin
              (triggered-buff [name:id #:effect attr:genshin-attribute
                                       #:trigger trigger:racket-expr
                                       #:limit limit:number
                                       #:party-wide party-wide:boolean
                                       #:duration duration:number])
              (unconditional-buff [name:id #:effect attr:genshin-attribute
                                           #:party-wide party-wide:boolean])
              (applied-buff [name:id #:effect attr:genshin-attribute
                                     #:limit limit:number
                                     #:party-wide party-wide:boolean
                                     #:duration duration:number]))
 
 
 )

(define-syntax compile-genshin-calc
  (lambda (stx)
    (syntax-parse stx
      [(_ exprs ...)
       (check-types (attribute exprs))
       (check-unique-names (attribute exprs))
       #'(begin exprs ...)])))
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

  (define (check-character-syntax
           result
           name
           attacks
           weapon
           skill
           burst
           artifacts
           expr)
    (define id 0)
    (if (and (member (syntax->datum attacks) (hash-ref result 'attacks))
             (set! id 1)
             (member (syntax->datum weapon) (hash-ref result 'weapons))
             (set! id 2)
             (member (syntax->datum skill) (hash-ref result 'skills))
             (set! id 3)
             (member (syntax->datum burst) (hash-ref result 'skills))
             (set! id 4)
             (andmap (λ (art)
                       (and (member (syntax->datum art)
                                    (hash-ref result 'artifacts))
                            (set! id (add1 id))))
                     artifacts)
             (set! id 5))
        (hash-set result 'characters
                  (cons (syntax->datum name)
                        (hash-ref result 'characters)))
        (raise-syntax-error 'character
                            "Incorrect data input for character"
                            expr
                            (list-ref (append (list attacks weapon skill burst)
                                              artifacts) id))))

  (define (check-team-syntax result chars name expr)
    (define id 0)
    (if (andmap (λ (char) (and (member (syntax->datum char)
                                       (hash-ref result 'characters))
                               (set! id (add1 id))))
                chars)
        (hash-set result 'teams (cons (syntax->datum name) (hash-ref result 'teams)))
        (raise-syntax-error 'teams
                            "Incorrect data input for team"
                            expr
                            (list-ref chars id))))

  (define (check-rotation-syntax result team enemy expr)
    (define id 0)
    (if (and (member (syntax->datum team) (hash-ref result 'teams)) (set! id 1)
             (member (syntax->datum enemy) (hash-ref result 'enemies)))
        result
        (raise-syntax-error 'calculate-rotation-damage
                            "Incorrect data input for damage rotation"
                            expr
                            (list-ref (list team enemy) id))))
  
  (define (check-types exprs)
    ((λ (result) (if (equal? (length (apply append (hash-values result)))
                             (length (remove-duplicates
                                      (apply append
                                             (hash-values result)))))
                     (void)
                     (raise-syntax-error 'calc "a duplicate name was found")))
     (foldl (λ (expr result) (if (hash? result)
                                 (syntax-parse expr
                                   [((~datum define-attack-sequence) name rest ...)
                                    (hash-set result
                                              'attacks
                                              (cons (syntax->datum #'name)
                                                    (hash-ref result 'attacks)))]
                                   [((~datum define-weapon) name rest ...)
                                    (hash-set result
                                              'weapons
                                              (cons (syntax->datum #'name)
                                                    (hash-ref result 'weapons)))]
                                   [((~datum define-skill) name rest ...)
                                    (hash-set result
                                              'skills
                                              (cons (syntax->datum #'name)
                                                    (hash-ref result 'skills)))]
                                   [((~datum define-artifact) name rest ...)
                                    (hash-set result
                                              'artifacts
                                              (cons (syntax->datum #'name)
                                                    (hash-ref result 'artifacts)))]
                                   [((~datum define-character)
                                     name _ _ _ _ _ _ _ _ _ _ _ _ _
                                     attacks*:id _
                                     weapon*:id _
                                     skill*:id _
                                     burst*:id _
                                     artifacts* ...)
                                    (check-character-syntax
                                     result
                                     #'name
                                     #'attacks*
                                     #'weapon*
                                     #'skill*
                                     #'burst*
                                     (attribute artifacts*)
                                     expr)]
                                   [((~datum define-enemy) name rest ...)
                                    (hash-set result
                                              'enemies
                                              (cons (syntax->datum #'name)
                                                    (hash-ref result 'enemies)))]
                                   [((~datum define-team-lineup) name (chars ...))
                                    (check-team-syntax result (attribute chars) #'name expr)]
                                   [((~datum calculate-rotation-damage) team* enemy* _)
                                    (check-rotation-syntax result #'team* #'enemy* expr)])
                                 result))
            (hash 'attacks (list)
                  'weapons (list)
                  'skills (list)
                  'artifacts (list)
                  'characters (list)
                  'enemies (list)
                  'teams (list))
            exprs)))
  (define (check-unique-names exprs)
    ; TODO
    (when #f
      (raise-syntax-error 'calc "incorrect type")))
  (define (check-artifact-overlap exprs)
    ; TODO (optional)
    (when #f
      (raise-syntax-error 'calc "incorrect type")))
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
      ; how to enforce that stat is hp/atk/def/em%?
      #; (hp (atk% 20))
      [(_ (~datum hp) (stat percent:number))
       #'(make-attribute 'hp (make-percent 'stat percent))]
      [(_ (~datum atk) (stat percent:number))
       #'(make-attribute 'atk (make-percent 'stat percent))]
      [(_ (~datum def) (stat percent:number))
       #'(make-attribute 'def (make-percent 'stat percent))]
      [(_ (~datum em) (stat percent:number))
       #'(make-attribute 'em (make-percent 'stat percent))]
      ; stat by stat percent (shortcut)  (hp% 20) = (hp (hp% 20))
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
      [(_ name:id atk:number (attr modifier) buffs ...)
       #'(define name (make-weapon atk (parse-attribute attr modifier) (list (parse-buff buffs) ...)))])))

(define-syntax compile-define-skill
  (lambda (stx)
    (syntax-parse stx
      [(_ name:id cd:number #:attr (attr modifier) #:duration duration:number #:type type buffs ...)
       #'(define name (make-skill cd (parse-attribute attr modifier) duration 'type (list (parse-buff buffs) ...)))])))

(define-syntax parse-buff
  (lambda (stx)
    (syntax-parse stx
      [(_ ((~datum triggered-buff) [name:id
                                    #:effect (attr percent)
                                    #:trigger trigger
                                    #:limit limit:number
                                    #:party-wide party-wide:boolean
                                    #:duration duration]))
       #'(make-triggered-buff (parse-attribute attr percent) trigger limit party-wide duration)]
      [(_ ((~datum unconditional-buff) [name:id
                                        #:effect (attr percent)
                                        #:party-wide party-wide:boolean]))
       #'(make-unconditional-buff (parse-attribute attr percent) party-wide)]
      [(_ ((~datum applied-buff) [name:id
                                  #:effect (attr percent)
                                  #:limit limit:number
                                  #:party-wide party-wide:boolean
                                  #:duration duration]))
       #'(make-applied-buff (parse-attribute attr percent) limit party-wide duration)])))

(define-syntax compile-define-attack-sequence
  (lambda (stx)
    (syntax-parse stx
      [(_ name:id ([(attr percent:number) duration:number type] ...
                   #:charged  [(attr2 percent2:number) duration2:number type2]
                   #:plunging [(attr3 percent3:number) duration3:number type3]))
       #'(define name (make-attack-sequence (list (make-attack (parse-attribute attr percent) duration 'type) ...)
                                            (make-attack (parse-attribute attr2 percent2) duration2 'type2)
                                            (make-attack (parse-attribute attr3 percent3) duration3 'type3)))])))

(define-syntax compile-define-artifact
  (lambda (stx)
    (syntax-parse stx
      [(_ name:id set-name:string (mattr mstat) (sattr sstat) ...)
       #'(define name (make-artifact set-name (parse-attribute mattr mstat) (list (parse-attribute sattr sstat) ...)))])))

(define-syntax define-character
  (lambda (stx)
    (syntax-parse stx
      [(_ name:id #:hp hp:number #:def def:number #:atk atk:number #:em em:number #:critr critr:number #:critd critd:number
          #:attacks attacks:id #:weapon weapon:id #:skill skill:id #:burst burst:id #:artifacts artifacts:id ...)
       #'(define name (make-character hp def atk em critr critd attacks weapon skill burst (list artifacts ...)))])))

(define-syntax define-enemy
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
       #'(define name (make-enemy def (make-resistances pyro hydro electro cryo geo anemo dendro physical) reduction))])))

(define-syntax define-team-lineup
  (lambda (stx)
    (syntax-parse stx
      [(_ name:id (chars:id ...))
       #'(define name (list chars ...))])))

(define-syntax calculate-rotation-damage
  (lambda (stx)
    (syntax-parse stx
      [(_ lineup:id enemy:id (attack-string ...))
       #'(calc-dmg lineup enemy (list 'attack-string ...))])))


;;;;;;;;;;;;;;;;;;;;;;
(genshin-calc

 (define-attack-sequence attack-chain
   ([(atk% 10) 0.5 physical]
    [(atk% 25) 0.2 physical]
    [(atk% 125) 0.8 physical]
    [(atk% 250) 1.5 physical]
    #:charged [(hp 5) 3.5 pyro]
    #:plunging [(hp 10) 3.5 physical]))


 (define-weapon test-weapon
   450 ;; base attack stat
   (critr 24.1) ;; substat (crit rate)
   (triggered-buff
    [dmgup
     #:effect (atk% 20.0) ;; increase atk by 20%
     #:trigger 'normal-attack
     #:limit 1
     #:party-wide #f
     #:duration 10.0])

   (unconditional-buff
    [crit-up
     #:effect (critd 20) ;; increase crit damage by 20%
     #:party-wide #f])
   )

 ;; note : duration is optional
 (define-skill all-attack-up
   25.0
   #:attr (atk% 125)
   #:duration 0.1
   #:type pyro
   (applied-buff
    [skill-atkup
     #:effect (atk 125)
     #:limit 1
     #:party-wide #t
     #:duration 10]) ;; this time applies to all members of a lineup
   )

 (define-skill basic-slash
   5.0 ;; cooldown
   #:attr (atk% 25)
   #:duration 1.0 ;; duration (where character cannot do anything else)
   #:type pyro
   (applied-buff
    [skill-hpup
     #:effect (hp 20)
     #:limit 1
     #:party-wide #f
     #:duration 10.0]) ;; increase hp by 10% of atk, only applies to current character
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
   #:critr 5     ;; base crit rate
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



 (define-team-lineup lone-member (test-char))

 (calculate-rotation-damage lone-member dummy (N N N N N N N N N N N N))
 )

#|

Example output: Compiled Code
|#

#;(begin

    (define attack-chain (make-attack-sequence (list (make-attack (make-attribute 'atk% 10) 0.5 'physical)
                                                     (make-attack (make-attribute 'atk% 25) 0.2 'physical)
                                                     (make-attack (make-attribute 'atk% 125) 0.8 'physical)
                                                     (make-attack (make-attribute 'atk% 250) 1.5 'physical))
                                               (make-attack (make-attribute 'hp 5) 3.5 'pyro)
                                               (make-attack (make-attribute 'hp 10) 3.5 'physical)))


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

    (calc-dmg lone-member
              dummy
              (list 'N 'N 'N 'N 'N 'N 'N 'N 'N 'N 'N 'N))

    )






