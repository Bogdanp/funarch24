#lang conscript/local

(require racket/random)

(defvar* ok? guessed-correctly-1)
(defstep (intro)
  (html
   (h1 "Welcome to the study!")
   (button "Start")))
(defstep (heads-or-tails)
  (define toss (random-ref '(h t)))
  (html
   (button (λ () (set! ok? (eq? toss 'h))) "Heads")
   " or "
   (button (λ () (set! ok? (eq? toss 't))) "Tails")))
(defstep (result)
  (html
   (if ok?
       (p "You guessed right.")
       (p "You guessed wrong."))))
(defstudy choices
  [heads-or-tails --> ,(λ () done)])
(defstudy example
  [intro --> choices --> result --> ,(λ () done)])
