#lang racket/base

(require (prefix-in dbg: debugging/server))
(define dbg:stop (dbg:serve))

(define (make-collector)
  (thread
   (lambda ()
     (let loop ()
       (with-handlers ([exn:fail? void])
         (sleep 0.05)
         (loop))))))

(with-handlers ([exn:break? void])
  (let loop ()
    (make-collector)
    (sleep 0.1)
    (loop)))
(dbg:stop)
