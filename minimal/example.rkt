#lang racket/base

(require threading
         web-server/http
         "study.rkt")

(provide
 example-study)

(define (hello)
  `(div
    (h1 "Hello!")
    (p "Welcome to the study.")
    ,(button "Continue")))

(define (get-details)
  `(div
    (h1 "Tell us about yourself")
    ,(form
      `(div
        (label "Name:" (input ([name "name"] [type "text"])))
        (label "Age:" (input ([name "age"] [type "number"])))
        (button ([type "submit"]) "Continue"))
      (lambda (req)
        (define bindings (request-bindings/raw req))
        (define name
          (and~>
           (bindings-assq #"name" bindings)
           (binding:form-value)
           (bytes->string/utf-8)))
        (define age
          (and~>
           (bindings-assq #"age" bindings)
           (binding:form-value)
           (bytes->string/utf-8)
           (string->number)))
        (and name age (> age 0) (< age 100))))))

(define (done)
  `(div
    (h1 "You're done!")
    ,(button "Continue")))

(define example-study
  (study
   'example
   (list
    (step 'hello hello)
    (step 'details get-details)
    (step 'done done))))
