#lang at-exp slideshow

(require pict
         slideshow/code
         slideshow/text)

(define (author+email name email)
  (vc-append
   (t name)
   (colorize (t email) "gray")))

(define-syntax-rule (with-code-size size body0 body ...)
  (parameterize ([get-current-code-font-size (lambda () size)])
    body0 body ...))

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
 (with-code-size 15
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
           (register-route! "/" guess-page)
           (register-route! "/answer" answer-page))]
         [example-with-continuations
          (code
           (define (guess-page req)
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
                [(> guess n) "Your guess was too high."])))
           code:blank
           (register-route! "/" guess-page))])
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
       [button-widget
        (lambda ()
          (code
           (define (button label [action void])
             `(a
               ([href ,((current-embed)
                        (lambda (req)
                          (action)
                          '(continue)))])
               ,label))))]
       [form-widget
        (lambda ()
          (code
           (define (form e [action (λ (req) #t)])
             `(form
               ([action ,((current-embed)
                          (lambda (req)
                            (if (action req)
                                '(continue)
                                '(retry))))]
                [method "POST"])
               ,e))))]
       [example-study
        (lambda ()
          (code
           (define (hello)
             `(div
               (p "Welcome to the study.")
               ,(button "Continue")))
           code:blank
           (define (done)
             `(p "Thank you for participating."))
           code:blank
           (define example-study
             (study
              (list
               (step hello)
               (step done))))))])
   (list
    (list
     @t{The core of Congame is:}
     'next
     @item{Studies, represented as trees of @italic{steps} and sub-studies.}
     'next
     @item{A servlet that traverses a given study, implemented using continuations.}
     'next
     @item{Widgets that let a participant interact with the study.})
    (list
     (with-code-size 24
       (mini-congame)))
    (list
     (with-code-size 24
       (vc-append
        20
        (button-widget)
        (form-widget))))
    (list
     (with-code-size 24
       (example-study))))))

(slide ;; Bogdan
 #:title @~a{Challenge: Dynamic Variables}
 'alts
 (let ([thread-example
        (with-code-size 20
          (code
           (define p (make-parameter #f))
           (define tag (make-continuation-prompt-tag))
           (define k-ch (make-channel))
           (void
            (thread
             (lambda ()
               (call-with-continuation-prompt
                (lambda ()
                  (parameterize ([p 'p1])
                    (p 'p2)
                    ((call-with-current-continuation
                      (lambda (k)
                        (thunk (channel-put k-ch k)))
                      tag))))
                tag))))
           (thread-wait
            (thread
             (lambda ()
               (define k (channel-get k-ch))
               (call-with-continuation-prompt
                (lambda ()
                  (k (λ () (printf "~s~n" (p)))))
                tag))))))]
       [cont-example-1
        (with-code-size 20
          (code
           (define a (make-parameter #f))
           (define b (make-parameter #f))
           (define tag (make-continuation-prompt-tag))
           (define k
             (parameterize ([a 'a])
               (call-with-continuation-prompt
                (lambda ()
                  ((call-with-current-continuation
                    (λ (k) (thunk k))
                    tag)))
                tag)))
           (call-with-continuation-prompt
            (lambda ()
              (k (lambda ()
                   (printf "~s ~s~n" (a) (b)))))
            tag)))]
       [cont-example-2
        (with-code-size 20
          (code
           (define a (make-parameter #f))
           (define b (make-parameter #f))
           (define tag (make-continuation-prompt-tag))
           (define k
             (parameterize ([a 'a])
               (call-with-continuation-prompt
                (lambda ()
                  (parameterize ([b 'b])
                    ((call-with-current-continuation
                      (λ (k) (thunk k))
                      tag))))
                tag)))
           (call-with-continuation-prompt
            (lambda ()
              (k (lambda ()
                   (printf "~s ~s~n" (a) (b)))))
            tag)))])
   (list
    (list
     @para{Racket has dynamic variable support via ``parameters''.}
     'next
     @item{Parameters are thread-specific and continuation-specific.}
     'next
     @item{In Congame, we use parameters to track the user's current position in a study.})
    (list
     @item{Parameters can be changed using the @code[parameterize] form.}
     'next
     @item{Or by using direct assignment: @code[(current-directory "/home")]}
     'next
     @item{Direct assignment mutates the thread-local state directly
                  without affecting any surrounding parameterizations.})
    (list
     @para{Thread specific:}
     (with-code-size 24
       (code
        (define current-position
          (make-parameter null))
        code:blank
        (parameterize ([current-position '(root study-1)])
          (thread
           (lambda ()
             (displayln (current-position))
             (current-position '(root study-1 study-2))
             (displayln (current-position)))))
        code:blank
        (thread
         (lambda ()
           (displayln (current-position))))
        code:blank
        (code:comment "Output:")
        (code:comment "t1: (root study-1)")
        (code:comment "t1: (root study-1 study-2)")
        (code:comment "t2: ()"))))
    (list
     @para{Parameter loss:}
     thread-example)
    (list
     @para{Parameter loss:}
     (ht-append
      thread-example
      (with-code-size 20
        (code
         (code:comment "Output:")
         (code:comment "p1")))))
    (list
     @para{Continuations + parameters:}
     cont-example-1)
    (list
     @para{Continuations + parameters:}
     (vl-append
      cont-example-1
      (with-code-size 20
        (code
         (code:comment "Output:")
         (code:comment "#f #f")))))
    (list
     @para{A surprising interaction:}
     cont-example-2)
    (list
     @para{A surprising interaction:}
     (vl-append
      cont-example-2
      (with-code-size 20
        (code
         (code:comment "Output:")
         (code:comment "a b"))))))))

(slide ;; Bogdan
 #:title @~a{Challenge: Debugging}
 'alts
 (list
  (list
   @item{There is mental overhead related to programming with continuations.}
   'next
   @item{Using continuations can amplify bugs.}
   @(hc-append
     (scale (bitmap "debugging-1.png") 0.2)
     (scale (bitmap "debugging-2.png") 0.2)))))

(slide ;; Marc & Bogdan: highlight costs of working in it, not just "is it possible"
 #:title @~a{Reflections & Recommendations}
 'alts
 (list
  (list
   'next
   @item{Continuations let us implement our study harness as a straightforward depth-first traversal of a tree.}
   'next
   @item{Less powerful language features, such as coroutines might offer similar benefits.}
   'next
   @item{The issues we encountered were mostly due to some surprising interactions between dynamic variables and delimited continuations.}
   'next
   @item{It would be nice if continuations were inspectable at runtime to aid with debugging.})))

(slide
 @titlet{Thanks!})
