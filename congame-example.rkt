#lang racket/base

(require congame/components/study
         congame/components/formular
         congame/components/transition-graph
         koyo/haml)

(provide
 funarch24)

(define submit-button
  (haml
   (:button.button.next-button ([:type "submit"]) "Submit")))

(define (score)
  (page
   (haml
    (.container
     (:h1 "Score")

     (formular
      (haml
       (:div
        (:div
         (#:score
          (input-number #:min 0 #:max 10 "Your score (from 0 to 10)")))
        submit-button)))))))

(define score-study
  (make-study
   "score"
   #:provides '(score)
   (list
    (make-step 'score score))))

(define (high-scorers)
  (page
   (haml
    (.container
     (:h1 "You are a high scorer!")

     (button void "Next")))))

(define high-scorers-study
  (make-study
   "high-scorers-only"
   (list (make-step 'high-scorers-only high-scorers))))

(define (final)
  (page
   (haml
    (.container
     (:h1 "Thank you for participating")))))

(define final-study
  (make-study
   "final"
   (list (make-step 'final final))))

(define funarch24
  (make-study
   "funarch24-study"
   #:transitions
   (transition-graph
    [score --> ,(lambda ()
                        (if (> (get 'score) 4)
                            'high-scorers-only
                            'final))]
    [high-scorers-only --> final]
    [final --> final])

   (list
    (make-step/study 'score score-study #:provide-bindings `([score score]))
    (make-step/study 'high-scorers-only high-scorers-study #:require-bindings `((score score)))
    (make-step/study 'final final-study))))
