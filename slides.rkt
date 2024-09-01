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
 #:title @~a{Intro: Illustration of Congame}

 @para{Congame is a platform for economics experiments, which are stateful applications:}
 'next

 @t{Example study: #<INCLUDE LINK AND QR CODE>})

(slide
 #:title @~a{Aspirations of Congame}

 @bit{Expand the Range of the Possible}

 'next
 @para{Concretely, scale controlled experiments to:}
 'next
 @item{long-term experiments,}
 'next
 @item{with many, interacting treatments,}
 'next
 @item{personalized to the participants,}
 'next
 @item{while tracking multi-dimensional outcomes.}
 'next

 @para{Above all, this requires @it{composable} studies.})

(slide
 #:title @~a{Why a Custom Platform?}

 @para{Why not use web frameworks (e.g., PHP, Django)?}
 'next
 @item{Do not address universal tasks:}
 @subitem{Track participants, store and extract data.}
 'next
 @item{Experimentalists don't care about web programming:}
 @subitem{How to handle requests, create DB schemas.}
 'next
 @item{No notion of @it{study},}
 @subitem{thus poor composability of studies.})

(slide
 #:title @~a{Why a Custom Platform?}
 @t{Why not use specialized software (e.g., oTree, Qualtrics)?}
 'next
 @item{Qualtrics: clicky-clicky gooey interface}
 'next
 @item{oTree: strong limitations on composing studies.}
 'next
 @subitem{@bt{But} oTree has many nice features,}
 'next
 @subitem{and is the primary inspiration of Congame.})


(slide
 #:title @~a{Why Congame?}
 @item{Automatically tracks participants}
 'next
 @item{Notion of @it{study} and @it{step}}
 @subitem{Recursive and composable}
 @subitem{Can be generated dynamically}
 'next
 @item{Full power of Racket available (almost) anywhere}
 'next
 @item{Notion of @it{study} scope for variables}
 @subitem{Imitates @it{lexical} scope}
 @subitem{=> study composition without name collisions}
 'next
 @item{Predictable sharing of data and variables across study})

(slide
 #:title @~a{Why Continuations?}

 @para{Continuations, by closing over arbitrary code, naturally enabled several of these benefits:}
 'next
 @item{Naturally captures state and progress}
 @subitem{Traverse study naturally}
 'next
 @item{Pick most appropriate data structure without worrying about serialization.}
 'next
 @item{Use full power of Racket}
 'next
 @para{These in turn facilitated the other advantages, while staying flexible.})

(slide
 #:title @~a{Why Congame?}
 )

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
 #:title @~a{Reflections}) ;; Marc & Bogdan: highlight costs of working in it, not just "is it possible"
