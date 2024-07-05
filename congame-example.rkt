#lang conscript/local

(provide
 funarch24)

(defvar* score the-score)

(defstep (get-score)
  (define (update-score #:score s)
    (set! score s))
  (html
   (h1 "Score")
   (form
    #:action update-score
    (input-number #:score #:min 0 #:max 10 "Your score (from 0 to 10)")
    submit-button)))

(defstudy score-study
  [get-score --> ,(lambda () done)])

(defstep (high-scorers)
  (html
   (h1 "You are a high scorer!")
   (p "Your score was " (number->string score))
   (button "Next")))

(defstudy high-scorers-study
  [high-scorers --> ,(lambda () done)])

(defstep (final)
  (html
   (h1 "Thank you for participating")))

(defstudy final-study
  [final --> final])

(defstudy funarch24
  [score-study --> ,(lambda ()
                      (if (> score 4)
                          'high-scorers-study
                          'final-study))]
  [high-scorers-study --> final-study]
  [final-study --> final-study])
