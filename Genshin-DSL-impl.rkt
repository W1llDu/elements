#lang racket
(require (for-syntax syntax/parse))
(require syntax-spec-v3 (for-syntax syntax/parse syntax/to-string))
(require "runtime.rkt")

(syntax-spec

 (extension-class genshin-macro #:binding-space genshin)

 (host-interface/definitions
  (define-weapon name:id damage:number attr:attribute buffs:buff ...)
  #'(compile-define-weapon name damage attr buffs ...))

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
 
 #;(nonterminal stat%
                hp%
                atk%
                def%)

 (nonterminal attribute
              (attr:stat mod:modifier)
              #;(attr:stat% percent:number))

 (nonterminal modifier
              flat:number
              (attr:stat percent:number)
              #;(attr:stat% percent:number))

 (nonterminal buff
              #:binding-space genshin
              (triggered-buff [name:id #:effect attr:attribute
                                       #:trigger trigger:racket-expr
                                       #:limit limit:number
                                       #:party-wide party-wide:boolean
                                       #:duration duration:number])
              (unconditional-buff [name:id #:effect attr:attribute
                                           #:party-wide party-wide:boolean]))

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
      [(_ (~datum hp) (stat percent:number)) #; (hp (atk% 20))
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

(define-syntax define-skill
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

#;(define-syntax parse-element
    (lambda (stx)
      (syntax-parse stx
        [(_ (~datum pyro))
         #''pyro]
        [(_ (~datum cryo))
         #''cryo]
        [(_ (~datum electro))
         #''electro]
        [(_ (~datum hydro))
         #''hydro]
        [(_ (~datum geo))
         #''geo]
        [(_ (~datum anemo))
         #''anemo]
        [(_ (~datum dendro))
         #''dendro]
        [(_ (~datum physical))
         #''physical])))

(define-syntax define-attack-sequence
  (lambda (stx)
    (syntax-parse stx
      [(_ name:id ([(attr percent:number) duration:number type] ...
                   #:charged  [(attr2 percent2:number) duration2:number type2]
                   #:plunging [(attr3 percent3:number) duration3:number type3]))
       #'(define name (make-attack-sequence (list (make-attack (parse-attribute attr percent) duration 'type) ...)
                                            (make-attack (parse-attribute attr2 percent2) duration2 'type2)
                                            (make-attack (parse-attribute attr3 percent3) duration3 'type3)))])))

(define-syntax define-artifact
  (lambda (stx)
    (syntax-parse stx
      [(_ name:id set-name:string (mattr mstat) (sattr sstat) ...)
       #'(define name (make-artifact set-name (parse-attribute mattr mstat) (list (parse-attribute sattr sstat) ...)))])))

(define-syntax define-character
  (lambda (stx)
    (syntax-parse stx
      [(_ name #:hp hp:number #:def def:number #:atk atk:number #:em em:number #:critr critr:number #:critd critd:number
          #:normal-attacks na:id #:weapon weapon:id #:skill skill:id #:burst burst:id #:artifacts artifact ...)
       #'(define name (make-character hp def atk em critr critd na weapon skill burst (list artifact ...)))])))

(define-syntax define-enemy
  (lambda (stx)
    (syntax-parse stx
      [(_ name #:def def:number
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
      [(_ name (chars ...))
       #'(define name (list chars ...))])))

(define-syntax calculate-rotation-damage
  (lambda (stx)
    (syntax-parse stx
      [(_ lineup enemy (attack-string ...))
       #'(calc-dmg lineup enemy (list 'attack-string ...))])))



;; EXAMPLE

;; wrap this

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
  #:normal-attacks attack-chain
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
#;(calc-dmg lone-member dummy '(N N N N N N N N N N N N))

