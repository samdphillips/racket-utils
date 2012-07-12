#lang racket/base

(require racket/function)

(module+ test
  (require rackunit)

  (define (make-tracer)
    (define trace-ch (make-channel))
    (define get-ch   (make-channel))

    (define t (current-milliseconds))
    (define (serve trace)
      (sync (handle-evt trace-ch
                        (lambda (x)
                          (serve (cons (list (quotient 
                                               (- (current-milliseconds) t) 1000)
                                             x)
                                       trace))))
            (handle-evt get-ch
                        (lambda (rpy-ch)
                          (channel-put rpy-ch (reverse trace))
                          (serve trace)))))

    (define (trace v)
      (channel-put trace-ch v)
      (sleep 0))

    (define (get)
      (define rpy-ch (make-channel))
      (channel-put get-ch rpy-ch)
      (sleep 0)
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
        (lambda (nack-evt)
          (define ch (make-channel))

          (define cancel-evt
            (handle-evt nack-evt
                        (thunk* (cleanup-thunk))))

          (define (poll)
            (cond [(poll-thunk) => (lambda (v)
                                     (sync cancel-evt
                                           (channel-put-evt ch v)))]
                  [else #f]))

          (define (poller)
            (sync cancel-evt
                  (handle-evt (timer-evt (interval-thunk))
                              (thunk* (or (poll) (poller))))))
          (thread poller)
          ch))))))

(module+ test
  (test-case "poll-evt calls cleanup thunk if not chosen"
    (define-values (trace get) (make-tracer))

    (sync (wrap-evt (timer-evt 7000)
                    (thunk* (trace 'finished)))
          (poll-evt (thunk (trace 'polling) #f)
                    #:interval
                    (const 2000)
                    #:cleanup
                    (thunk (trace 'cancelling))))
    
    (sleep 0)
    (check-equal? (get)
                  '((2 polling) (4 polling) (6 polling)
                    (7 finished) (7 cancelling)))))

(module+ test
  (test-case "poll-evt is ready with the value of  poll when it returns non-false"
    (define-values (trace get) (make-tracer))
    (define count 2)

    (trace
      (sync (poll-evt (thunk
                        (trace 'polling)
                        (cond [(zero? count) 'ok]
                              [else
                                (set! count (sub1 count))
                                #f])))))
    (check-equal? (get)
                  '((1 polling) (2 polling) (3 polling) (3 ok)))))


(module+ test
  (test-case "poll-evt with sync/timeout doesn't run polling thread"
    (define-values (trace get) (make-tracer))

    (check-false (sync/timeout 0 (poll-evt (thunk
                                             (trace 'one)
                                             #f))))
    (check-equal? (get) '((0 one)))

    (check-eq? (sync/timeout 0 (poll-evt (thunk
                                           (trace 'two) 
                                           'ok)))
               'ok)
    (check-equal? (get) '((0 one) (0 two)))))

