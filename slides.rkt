#lang at-exp slideshow

(require pict
         slideshow/code
         slideshow/text)

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

(slide ;; Bogdan
 #:title @~a{Continuations on the Web}
 @t{Web programming with continuations simplifies control flow.}
 'next
 'alts
 (parameterize ([get-current-code-font-size (λ () 16)])
   (let ([example-without-continuations
          (code
           (define (guess-page req)
             (define n (random 1 100))
             (add-cookie
              "n" (number->string n)
              (response/xexpr
               `(form
                 ([action "/answer"])
                 (input ([name "guess"]))
                 (button "Submit")))))
           code:blank
           (define (answer-page req)
             (define n
               (let ([s (cookies-ref req "n")])
                 (and s (string->number s))))
             (unless n
               (error "how'd you get here?"))
             (define guess
               (string->number (form-ref req "guess")))
             (response/xexpr
              (cond
                [(= guess n) "You guessed right."]
                [(< guess n) "Your guess was too low."]
                [(> guess n) "Your guess was too high."])))
           code:blank
           (register-route! "/answer" answer-page))]
         [example-with-continuations
          (code
           (define (guess-the-number req)
             (define n (random 1 100))
             (define next-req
               (send/suspend
                (lambda (k-url)
                  (response/xexpr
                   `(form
                     ([action ,k-url])
                     (input ([name "guess"]))
                     (button "Submit"))))))
             (define guess
               (string->number (form-ref next-req "guess")))
             (response/xexpr
              (cond
                [(= guess n) "You guessed right."]
                [(< guess n) "Your guess was too low."]
                [(> guess n) "Your guess was too high."]))))])
     (list
      (list
       @t{Instead of:}
       example-without-continuations)
      (list
       (ht-append
        (vc-append
         10
         @t{Instead of:}
         example-without-continuations)
        (vc-append
         10
         @t{We can write:}
         example-with-continuations)))))))

(slide ;; Bogdan
 #:title @~a{Mini Congame}
 'alts
 (let ([mini-congame
        (lambda ()
          (code
           (define current-embed (make-parameter #f))
           (struct step (handler))
           (struct study (steps))
           (define (run-study the-study)
             (let loop ([steps (study-steps the-study)])
               (if (null? steps)
                   '(continue)
                   (match (begin0 (run-step (car steps))
                            (redirect/get/forget))
                     ['(retry) (loop steps)]
                     ['(continue) (loop (cdr steps))]))))
           (define (run-step the-step)
             (match the-step
               [(step (? study? substudy))
                (run-study substudy)]
               [(step handler)
                (send/suspend/dispatch
                 (lambda (embed)
                   (parameterize ([current-embed embed])
                     (response/xexpr (handler)))))]))))]
       [widgets
        (lambda ()
          (code
           (define (button label [action void])
             `(a
               ([href ,((current-embed)
                        (lambda (_req)
                          (action)
                          '(continue)))])
               ,label))
           code:blank
           (define (form e [action (λ (_req) #t)])
             `(form
               ([action ,((current-embed/url)
                          (lambda (req)
                            (if (action req)
                                '(continue)
                                '(retry))))]
                [method "POST"])
               ,e))))])
   (list
    (list
     @t{The core of Congame is:}
     'next
     @item{Studies, represented as trees of @italic{steps} and sub-studies.}
     @item{A servlet that traverses a given study, implemented using continuations.}
     @item{Widgets that let a participant interact with the study.})
    (list
     (parameterize ([get-current-code-font-size (λ () 24)])
       (mini-congame)))
    (list
     (parameterize ([get-current-code-font-size (λ () 18)])
       (ht-append
        (mini-congame)
        (widgets)))))))

(slide
 #:title @~a{Challenge: Combining Dynamic Variables and Continuations}) ;; Bogdan

(slide
 #:title @~a{Challenge: Debugging}) ;; Bogdan

(slide
 #:title @~a{Reflections}) ;; Marc & Bogdan
