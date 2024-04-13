#lang racket/base

(require web-server/http
         web-server/servlet-dispatch
         web-server/web-server
         "nested.rkt"
         "study.rkt")

(define (app _req)
  (run-study nested-study)
  (response/xexpr "Done."))

(module+ main
  (require racket/async-channel
           racket/cmdline)
  (define-values (host port)
    (let ([HOST "127.0.0.1"]
          [PORT 8000])
      (command-line
       #:once-each
       [("--host" "-H")
        HOST-STR "the host to bind to"
        (set! HOST HOST-STR)]
       [("--port" "-P")
        PORT-STR "the port to listen on"
        (let ([port (string->number PORT-STR)])
          (unless (and port (>= port 0) (<= port 65535))
            (error "invalid --port value"))
          (set! PORT port))]
       #:args []
       (values HOST PORT))))
  (define ch (make-async-channel))
  (define stop
    (serve
     #:dispatch (dispatch/servlet app)
     #:confirmation-channel ch
     #:listen-ip host
     #:port port))
  (define maybe-exn
    (sync ch))
  (when (exn:fail? maybe-exn)
    (raise maybe-exn))
  (with-handlers ([exn:break? void])
    (sync/enable-break never-evt))
  (stop))
