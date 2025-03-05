#lang racket
(require (for-syntax syntax/parse))

;; define basic stats + calculations
(define-struct character [hp atk def critr critd em attacks weapon artifacts])
(define-struct state-handler [character active-buffs time])
(define-struct weapon [damage substat buffs])
(define-struct skill [cd duration buffs])
(define-struct attack [attr percent duration])
(define-struct artifact [set-name main-stat substats])
(define-struct stat [attr percent])
(define-struct triggered-buff [effect trigger limit target duration])
(define-struct unconditional-buff [effect target])

(define-syntax define-weapon
  (lambda (stx)
    (syntax-parse stx
      [(_ name:id damage:number (attr percent) buffs ...)
       #'(define name (make-weapon damage (make-stat attr percent) (list (parse-buff buffs) ...)))])))

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
                                   #:target target
                                   #:duration duration]))
       #'(make-triggered-buff (make-stat attr percent) trigger limit target duration)]
      [(_ ((~datum unconditional-buff) [name:id
                                   #:effect (attr percent)
                                   #:target target]))
       #'(make-unconditional-buff (make-stat attr percent) target)])))

(define-syntax define-attack-sequence
  (lambda (stx)
    (syntax-parse stx
      [(_ name:id ([(attr:id percent:number) duration:number]) ...)
       #'(define name (list (make-attack attr percent duration) ...))])))

(define-syntax define-artifact
  (lambda (stx)
    (syntax-parse stx
      [(_ name:id set-name:string (mattr mstat) (sattr sstat) ...)
       #'(define name (make-artifact set-name (make-stat mattr mstat) (make-stat sattr sstat) ...))])))


(define-weapon test-weapon
  450 ;; base attack stat
  ('critr 24.1) ;; substat (crit rate)
  (triggered-buff
   [dmgup
   #:effect ('atk% 20.0) ;; increase atk by 20%
   #:trigger 'normal-attack
   #:limit 1
   #:target 'self  
   #:duration 10.0])
  
  (unconditional-buff
   [crit-up
   #:effect ('critd 20) ;; increase crit damage by 20%
   #:target 'self]) 
)



(define (test-func a)
  (begin
    (define test-weapon "wow! you did it!")
    2))