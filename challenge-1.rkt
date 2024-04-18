#lang racket

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
 tag)
