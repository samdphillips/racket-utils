#lang racket/base

(require racket/function)

(module+ test
  (require rackunit)

  (define (make-tracer)
    (define trace-ch (make-channel))
    (define get-ch   (make-channel))

    (define (serve t trace)
      (sync (handle-evt trace-ch
                        (lambda (x)
                          (serve (cons (list (- (current-milliseconds) t) x)
                                       trace))))
            (handle-evt get-ch
                        (lambda (rpy-ch)
                          (channel-put rpy-ch (reverse trace))
                          (serve trace)))))

    (define (trace v)
      (channel-put trace-ch v))

    (define (get)
      (define rpy-ch (make-channel))
      (channel-put get-ch rpy-ch)
      (channel-get rpy-ch))

    (thread (thunk (serve null)))
    (values trace get)))

(define (timeout-evt msecs)
  (define evt
    (wrap-evt
      (alarm-evt (+ (current-inexact-milliseconds)
                    msecs))
      (const evt)))
  evt)

(module+ test
  (test-case "check timeout-evt"
    (define a (timeout-evt 2000))
    (check-false (sync/timeout 0 (wrap-evt a (const 'oops)))
                 "timeout-evt is ready after timer expires")
    (check-eq? (sync/timeout 4 (wrap-evt a (const 'ok))) 'ok
               "timeout-evt is ready after timer expires")
    (check-eq? (sync/timeout 0 (wrap-evt a (const 'ok))) 'ok
               "timeout-evt doesn't reset on sync")))

(define (timer-evt msecs)
  (define evt
    (guard-evt
      (lambda ()
        (wrap-evt
          (alarm-evt (+ (current-inexact-milliseconds)
                        msecs))
          (const evt)))))
  evt)

(module+ test
  (test-case "check timer-evt"
    (define a (timer-evt 2000))
    (check-false (sync/timeout 0 (wrap-evt a (const #t)))
                 "timer-evt is ready after timer expires")
    (check-eq? (sync/timeout 4 (wrap-evt a (const 'ok))) 'ok)
    (check-false (sync/timeout 0 (wrap-evt a (const #t)))
                 "timer-evt resets on each sync")
    (check-eq? (sync/timeout 4 (wrap-evt a (const 'ok))) 'ok)))

(define (poll-evt poll-thunk
                  #:interval [interval-thunk (const 1000)]
                  #:cleanup  [cleanup-thunk  void])
  (poll-guard-evt
    (lambda (polling?)
      (if polling?
          (cond [(poll-thunk) => 
                 (lambda (v)
                   (wrap-evt always-evt (const v)))]
                [else never-evt])
      (nack-guard-evt
        (lambda (cancel-evt)
          (define ch (make-channel))
          (define (poll)
            (cond [(poll-thunk) => (curry channel-put ch)]
                  [else #f]))
          (define (poller)
            (sync (handle-evt cancel-evt
                              (thunk*
                                (cleanup-thunk)))
                  (handle-evt (timer-evt (interval-thunk))
                              (thunk* (or (poll) (poller))))))
          (thread poller)
          ch))))))

(module+ test
  (sync (wrap-evt (timer-evt 4000)
                  (thunk*
                    (printf "[~a] finished~%"
                            (current-inexact-milliseconds))))
        (poll-evt (thunk
                    (printf "[~a] polling~%"
                            (current-inexact-milliseconds))
                    #f)
                  #:cleanup
                  (thunk
                    (printf "[~a] cancelling~%"
                            (current-inexact-milliseconds)))))
  
  (sleep 0)
  'ok)

(module+ test
  (let ([count 2])
    (sync (poll-evt (thunk
                      (printf "[~a] polling~%"
                              (current-inexact-milliseconds))
                      (cond [(zero? count) 'ok]
                            [else
                              (set! count (sub1 count))
                              #f]))))))
                          

(module+ test
  (sync/timeout 0 (poll-evt (thunk
                              (printf "[~a] polling~%"
                                      (current-inexact-milliseconds))
                              #f))))

(module+ test
  (sync/timeout 0 (poll-evt (thunk
                              (printf "[~a] polling~%"
                                      (current-inexact-milliseconds))
                              'ok))))

