#lang racket/base

(require (prefix-in dbg: debugging/server))
(define dbg:stop (dbg:serve))
(define prompt (make-continuation-prompt-tag))

(define (make-collector)
  (thread
   (lambda ()
     (let loop ()
       (with-handlers ([exn:fail? void])
         (sleep 0.05)
         (loop))))))

(random-seed 1337)
(with-handlers ([exn:break? void])
  (call-with-continuation-prompt
   (lambda ()
     (let loop ()
       (define k
         (call-with-composable-continuation
          (lambda (k)
            (begin0 k
              (make-collector)))
          prompt))
       (sleep 0.1)
       (when (< (random) 0.05)
         (k void))
       (loop)))
   prompt))
(dbg:stop)
