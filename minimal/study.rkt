#lang racket/base

(require racket/match
         web-server/servlet)

(provide (all-defined-out))

(define current-embed/url
  (make-parameter (λ (_) (error "embed/url not installed"))))

(struct step (handler))
(struct study (steps))

(define (run-study the-study)
  (let loop ([steps (study-steps the-study)])
    (if (null? steps)
        '(continue)
        (match (begin0 (run-step (car steps))
                 (redirect/get/forget))
          ['(continue)
           (loop (cdr steps))]
          ['(retry)
           (loop steps)]
          [(? response? r)
           (send/back r)]))))

(define (run-step the-step)
  (match the-step
    [(step (? study? substudy))
     (run-study substudy)]
    [(step handler)
     (send/suspend/dispatch
      (lambda (embed/url)
        (parameterize ([current-embed/url embed/url])
          (response/xexpr (handler)))))]))

(define (button label [action void])
  `(a
    ([href ,((current-embed/url)
             (lambda (_req)
               (action)
               '(continue)))])
    ,label))

(define (form e [action (λ (_req) #t)])
  `(form
    ([action ,((current-embed/url)
               (lambda (req)
                 (if (action req)
                     '(continue)
                     '(retry))))]
     [method "POST"])
    ,e))
