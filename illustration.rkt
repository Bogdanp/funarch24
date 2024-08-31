#lang conscript/local

(require racket/random)
(provide illustration)

(defvar name)
(defvar fav-pl)

(defstep (survey)
  @md{# Survey

      @form{
        @label{Name: @set![name (input-text)]}
        @label{Favorite programming language: @set![fav-pl (input-text)]}
        @submit-button}})

(defstep (evaluate-survey)
  (define pass-attention-check?
    (string=? (string-downcase fav-pl) "racket"))

  @md{# Thank you @|name|

      @(if pass-attention-check?
           "My favorite programming language is Racket too!"
           (format "Your favorite programming language is ~a" @fav-pl))

      @button{Next}})

(defvar guess)
(defvar toss)

(defstep (heads-or-tails)
  (set! toss (random-ref '(h t)))

  @md{# Coin Toss

      @form{
        @label{Guess heads or tails:}

        @set![guess
              (radios '((h . "Heads")
                        (t . "Tails")))]
        @submit-button}})

(defstep (result)
  (define correct-guess?
    (equal? toss guess))
  (define (ht ct)
    (case ct
      [(h) "Heads"]
      [(t) "Tails"]))

  @md{# Result

      The toss came up with @(ht toss) and you guessed @(ht guess).

      So you guessed @(if correct-guess? "right" "wrong").

      @button{Next}})

(defstudy coin-toss
  [heads-or-tails --> result
                  --> ,(lambda () done)])

(defstep (the-end)
  @md{# The End

      Thank you for participating.})

(defstudy illustration
  [survey --> evaluate-survey
          --> coin-toss
          --> the-end])
