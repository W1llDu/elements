#lang racket

(require "save-data.rkt")

(provide display-data)

#|
  Deals with how data is displayed to the user when the user wants more than raw data.
  Formats the results properly, and compares it to previous attempts.
|#

;; checks for a more optimal damage rotation based on saved results
;; (List Any) (List Any) Team Enemy -> (List Any)
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
;; Number -> Number
(define (decimal-round num)
  (/ (round (* 100 num)) 100))

;; calculate and display the results of a damage calculation
;; (List Number) (List Symbol) -> Void
(define (display-data result attack-string)
  (define final-dmg (first result))
  (define final-time (second result))
  (define string-data (last (load-entries)))
  (display "[]=======================================================================================[]\n")
  (display "  Simulation run with input string: ")
  (display attack-string)
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