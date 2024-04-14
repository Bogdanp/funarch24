#lang racket/base

(require racket/match
         (except-in web-server/formlets button)
         "study.rkt")

(provide
 example-study)

(define (hello)
  `(div
    (h1 "Hello!")
    (p "Welcome to the study.")
    ,(button "Continue")))

(define (get-details)
  (define details-formlet
    (formlet
     (div
      (label "Name:" ,{input-string . => . name})
      (label "Age:" ,{input-string . => . age})
      (button ([type "submit"]) "Continue"))
     (list name age)))
  `(div
    (h1 "Tell us about yourself")
    ,(form
      `(div ,@(formlet-display details-formlet))
      (lambda (req)
        (match-define (list name (app string->number age))
          (formlet-process details-formlet req))
        (and name age (>= age 0) (< age 100))))))

(define (done)
  `(div
    (h1 "You're done!")
    ,(button "Continue")))

(define example-study
  (study
   (list
    (step hello)
    (step get-details)
    (step done))))
