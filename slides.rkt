#lang at-exp slideshow

(require pict
         threading)

(define (author+email name email)
  (vc-append
   (t name)
   (colorize (t email) "gray")))

(slide
 @titlet{Continuations: What Have They Ever Done for Us?}
 @author+email["Marc Kaufmann"]{kaufmannm@"@"ceu.edu}
 @author+email["Bogdan Popa"]{bogdan@"@"defn.io})

(slide
 #:title @~a{Intro}) ;; Marc

(slide
 #:title @~a{Congame}) ;; Marc

(slide
 #:title @~a{Congame vs oTree}) ;; Marc

(slide
 #:title @~a{Continuations on the Web}) ;; Bogdan

(slide
 #:title @~a{Mini Congame}) ;; Bogdan

(slide
 #:title @~a{Challenge: Combining Dynamic Variables and Continuations}) ;; Bogdan

(slide
 #:title @~a{Challenge: Debugging}) ;; Bogdan

(slide
 #:title @~a{Reflections}) ;; Marc & Bogdan
