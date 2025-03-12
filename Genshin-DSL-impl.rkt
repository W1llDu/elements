#lang racket
(require (for-syntax syntax/parse))
(require syntax-spec-v3 (for-syntax syntax/parse syntax/to-string))

;; define basic stats + calculations
(define-struct character [hp atk def critr critd em attacks weapon skill burst artifacts])
(define-struct state-handler [character active-buffs time])
(define-struct weapon [damage substat buffs])
(define-struct skill [cd duration buffs])
(define-struct attack [attr duration])
(define-struct artifact [set-name main-stat substats])
(define-struct triggered-buff [effect trigger limit party-wide duration])
(define-struct unconditional-buff [effect party-wide])
(define-struct applied-buff [effect limit party-wide duration])
(define-struct attack-sequence [normals charged plunge])
(define-struct attribute [attr-func value])
(define-struct damage [attr duration elem-type])

(define-syntax parse-attribute
  (lambda (stx)
    (syntax-parse stx
      [(_ (~datum hp) percent:number)
       #'(make-attribute (lambda (x) (character-hp x)) percent)]
      [(_ (~datum atk) percent:number)
       #'(make-attribute (lambda (x) (character-atk x)) percent)]
      [(_ (~datum def) percent:number)
       #'(make-attribute (lambda (x) (character-def x)) percent)]
      [(_ (~datum critr) percent:number)
       #'(make-attribute (lambda (x) (character-critr x)) percent)]
      [(_ (~datum critd) percent:number)
       #'(make-attribute (lambda (x) (character-critd x)) percent)]
      [(_ (~datum em) percent:number)
       #'(make-attribute (lambda (x) (character-em x)) percent)])))

(define-syntax define-weapon
  (lambda (stx)
    (syntax-parse stx
      [(_ name:id damage:number (attr percent) buffs ...)
       #'(define name (make-weapon damage (parse-attribute attr percent) (list (parse-buff buffs) ...)))])))

(define-syntax define-skill
  (lambda (stx)
    (syntax-parse stx
      [(_ name:id cd:number duration:number buffs ...)
       #'(define name (make-skill cd duration (list (parse-buff buffs) ...)))])))

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
       #'(make-applied-buff (parse-attribute attr percent) limit party-wide duration)]
      [(_ ((~datum damage) [(attr percent)
                            #:duration duration:number
                            #:type elem-type]))
       #'(make-damage (parse-attribute attr percent) duration (parse-element elem-type))])))

(define-syntax parse-element
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
      [(_ name:id ([(attr percent:number) duration:number] ...
                   #:charged  [(attr2 percent2:number) duration2:number]
                   #:plunging [(attr3 percent3:number) duration3:number]))
       #'(define name (make-attack-sequence (list (make-attack (parse-attribute attr percent) duration) ...)
                                            (make-attack (parse-attribute attr2 percent2) duration2)
                                            (make-attack (parse-attribute attr3 percent3) duration3)))])))

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




;; EXAMPLE

(define-attack-sequence attack-chain
  ([(atk 10) 0.5]
   [(atk 25) 0.2]
   [(atk 125) 0.8]
   [(atk 250) 1.5]
   #:charged [(hp 5) 3.5]
   #:plunging [(hp 10) 3.5]))


(define-weapon test-weapon
  450 ;; base attack stat
  (critr 24.1) ;; substat (crit rate)
  (triggered-buff
   [dmgup
    #:effect (atk 20.0) ;; increase atk by 20%
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
  2.0
  (damage
   [(atk 125)
   #:duration 0.1
   #:type pyro])
  (applied-buff
   [skill-atkup
   #:effect (atk 125)
   #:limit 1
   #:party-wide #t
   #:duration 10]) ;; this time applies to all members of a lineup
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
