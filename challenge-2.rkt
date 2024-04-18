#lang racket

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
     tag))))
