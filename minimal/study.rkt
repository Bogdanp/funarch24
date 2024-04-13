#lang racket/base

(require racket/match
         web-server/servlet)

(provide (all-defined-out))

(define current-embed/url
  (make-parameter #f))

(struct step (id handler))
(struct study (id steps))

(define (run-study the-study)
  (let loop ([steps (study-steps the-study)])
    (if (null? steps)
        `(continue ,(current-parameterization))
        (match (begin0 (run-step (car steps))
                 (redirect/get/forget))
          [`(continue ,paramz)
           (call-with-parameterization paramz (λ () (loop (cdr steps))))]
          [`(retry)
           (loop steps)]
          [(? response? r)
           (send/back r)]))))

(define (run-step the-step)
  (match the-step
    [(step _ (? study? substudy))
     (run-study substudy)]
    [(step _ handler)
     (send/suspend/dispatch
      (lambda (embed/url)
        (parameterize ([current-embed/url embed/url])
          (response/xexpr (handler)))))]))

(define (embed k)
  (define paramz
    (current-parameterization))
  ((current-embed/url)
   (lambda (req)
     (k paramz req))))

(define (button label [action void])
  `(a
    ([href ,(embed
             (lambda (paramz _req)
               (action)
               `(continue ,paramz)))])
    ,label))

(define (form e [action (λ (_req) #t)])
  `(form
    ([action ,(embed
               (lambda (paramz req)
                 (if (action req)
                     `(continue ,paramz)
                     `(retry))))]
     [method "POST"])
    ,e))
