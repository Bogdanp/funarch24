#lang racket/base

(require "example.rkt"
         "study.rkt")

(provide
 nested-study)

(define (intro)
  `(div
    (h1 "Welcome to the Nested Study")
    ,(button "Continue")))

(define (intermission)
  `(div
    (h1 "Intermission")
    ,(button "Continue")))

(define (done)
  `(div
    (h1 "Done")
    (p "You are done.")
    ,(button "Continue")))

(define nested-study
  (study
   'nested
   (list
    (step 'intro intro)
    (step 'example-1 example-study)
    (step 'intermission intermission)
    (step 'example-2 example-study)
    (step 'done done))))
