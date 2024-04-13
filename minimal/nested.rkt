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
   (list
    (step intro)
    (step example-study)
    (step intermission)
    (step example-study)
    (step done))))
