#lang racket

(provide (all-defined-out))

#|
  Handles saving and loading to the save data file.
|#

; saves an entry to the save data file
;; (List Any) -> Void
(define (save-entry entry)
  (call-with-output-file "data.txt"
    (lambda (out)
      (write entry out) 
      (newline out))   
    #:exists 'append)) 

; loads all information in the save data file
;; -> (List Any)
(define (load-entries)
  (with-handlers ([exn:fail? (lambda (_) '())])
    (call-with-input-file "data.txt"
      (lambda (in)
        (read-lines '() in)))))

; reads the lines of the save data file
;; (List Any) InputPort -> (List Any)
(define (read-lines entries in)
  (let ([line (read in)])
    (if (eof-object? line)
        (reverse entries)
        (read-lines (cons line entries) in)))) 

; clears the save data file
;; -> Void
(define (clear-file)
  (call-with-output-file "data.txt"
    (lambda (out) (void))
    #:exists 'truncate))  