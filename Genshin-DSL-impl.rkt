#lang racket

(require (for-syntax syntax/parse))
(require syntax-spec-v3 (for-syntax syntax/parse syntax/to-string))
(require "runtime.rkt")
(require racket/hash)

(provide (all-defined-out))

#|
(syntax-spec

 (extension-class genshin-macro #:binding-space genshin)

 (host-interface/definitions
  (genshin-calc exprs:expr ...)
  #'(compile-calc exprs ...))
 #|
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
 |#
 )
|#

(define-syntax genshin-calc
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
  (define (check-types exprs)
    ((λ (result) (if (boolean? result)
                     (raise-syntax-error 'calc "improper type given somewhere")
                     (if (equal? (length (apply append (hash-values result)))
                                      (length (remove-duplicates (apply append (hash-values result)))))
                         (void)
                         (raise-syntax-error 'calc "a duplicate name was found"))))
       (foldl (λ (expr result) (if (hash? result)
                                   (syntax-parse expr
                                     [((~datum define-attack-sequence) name rest ...)
                                      (hash-set result 'attacks (cons (syntax->datum #'name) (hash-ref result 'attacks)))]
                                     [((~datum define-weapon) name rest ...)
                                      (hash-set result 'weapons (cons (syntax->datum #'name) (hash-ref result 'weapons)))]
                                     [((~datum define-skill) name rest ...)
                                      (hash-set result 'skills (cons (syntax->datum #'name) (hash-ref result 'skills)))]
                                     [((~datum define-artifact) name rest ...)
                                      (hash-set result 'artifacts (cons (syntax->datum #'name) (hash-ref result 'artifacts)))]
                                     [((~datum define-character) name _ _ _ _ _ _ _ _ _ _ _ _ _ attacks*:id _ weapon*:id _ skill*:id _ burst*:id _ artifacts* ...)
                                      (if (and (member (syntax->datum #'attacks*) (hash-ref result 'attacks))
                                               (member (syntax->datum #'weapon*) (hash-ref result 'weapons))
                                               (member (syntax->datum #'skill*) (hash-ref result 'skills))
                                               (member (syntax->datum #'burst*) (hash-ref result 'skills))
                                               (andmap (λ (art) (member (syntax->datum art) (hash-ref result 'artifacts)))
                                                       (attribute artifacts*)))
                                          (hash-set result 'characters (cons (syntax->datum #'name) (hash-ref result 'characters)))
                                          #f)]
                                     [((~datum define-enemy) name rest ...)
                                      (hash-set result 'enemies (cons (syntax->datum #'name) (hash-ref result 'enemies)))]
                                     [((~datum define-team-lineup) name (chars ...))
                                      (if (andmap (λ (char) (member (syntax->datum char) (hash-ref result 'characters)))
                                                  (attribute chars))
                                          (hash-set result 'teams (cons (syntax->datum #'name) (hash-ref result 'teams)))
                                          #f)]
                                     [((~datum calculate-rotation-damage) team* enemy* _)
                                      (if (and (member (syntax->datum #'team*) (hash-ref result 'teams))
                                               (member (syntax->datum #'enemy*) (hash-ref result 'enemies)))
                                          result
                                          #f)])
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

(define-syntax define-weapon
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
          #:attacks attacks:id #:weapon weapon:id #:skill skill:id #:burst burst:id #:artifacts artifacts ...)
       #'(define name (make-character hp def atk em critr critd attacks weapon skill burst (list artifacts ...)))])))

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