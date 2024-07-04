;; Really short version

; Define studies
(define score-study
  (make-study
   "score"
   #:provides '(score)
   (list
    (make-step 'score score))))

; ...

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

;; Longer version

; Define form
     (formular
      (haml
       (:div
        (:div
         (#:score
          (input-number #:min 0 #:max 10 "Your score (from 0 to 10)")))
        submit-button)))

; Define studies
(define score-study
  (make-study
   "score"
   #:provides '(score)
   (list
    (make-step 'score score))))

(define high-scorers-study
  (make-study
   "high-scorers-only"
   (list (make-step 'high-scorers-only high-scorers))))

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
