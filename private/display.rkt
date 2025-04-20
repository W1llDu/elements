#lang racket

(require "save-data.rkt")

(provide display-data)

;; simple rounding function for displaying data
(define (decimal-round num)
  (/ (round (* 100 num)) 100))

;; calculate and display the results of a damage calculation
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