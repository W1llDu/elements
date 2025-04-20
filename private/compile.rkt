#lang racket

(require "runtime.rkt"
         racket/hash
         (for-syntax syntax/parse))

#|
  Compile time checks for the elements dsl.
  Main goal is to prevent duplicate name definitions,
  and provide better errors.
|#

(begin-for-syntax
  (provide compile-genshin)
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

  (define (compile-genshin exprs)
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