#lang racket
(require (for-syntax syntax/parse))
(require syntax-spec-v3 (for-syntax syntax/parse syntax/to-string))


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
              critd)

 (nonterminal attribute
              (attr:stat percent:number))

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

;; define basic stats + calculations
(define-struct character [hp atk def critr critd em attacks weapon artifacts])
(define-struct state-handler [character active-buffs time])
(define-struct weapon [damage substat buffs])
(define-struct skill [cd duration buffs])
(define-struct attack [attr duration])
(define-struct artifact [set-name main-stat substats])
(define-struct triggered-buff [effect trigger limit party-wide duration])
(define-struct unconditional-buff [effect party-wide])
(define-struct attack-sequence [normals charged plunge])
(define-struct attribute [attr-func value])

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

(define-syntax compile-define-weapon
  (lambda (stx)
    (syntax-parse stx
      [(_ name:id damage:number (attr percent) buffs ...)
       #'(define name (make-weapon damage (parse-attribute attr percent) (list (parse-buff buffs) ...)))])))

(define-syntax define-skill
  (lambda (stx)
    (syntax-parse stx
      [(_ name:id cd:number duration:number buffs ...)
       #'(define name (make-skill cd duration (buffs ...)))])))

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
       #'(make-unconditional-buff (parse-attribute attr percent) party-wide)])))

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
       #'(define name (make-artifact set-name (make-stat mattr mstat) (make-stat sattr sstat) ...))])))

(define-syntax define-character
  (lambda (stx)
    (syntax-parse stx
      [(_ name #:hp hp:number #:def def:number #:atk atk:number #:em em:number #:critr critr:number #:critd critd:number
          #:normal-attacks na:id #:skill skill:id #:burst burst:id #:artifacts artifact ...)
       #'(define name (make-character hp def atk em critr critd na skill burst (list artifact ...)))])))

(define-syntax define-team-lineup
  (lambda (stx)
    (syntax-parse stx
      [(_ name (character ...))
       #'(define name (list character ...))])))

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



(define (test-func a)
  (begin
    (define test-weapon "wow! you did it!")
    2))