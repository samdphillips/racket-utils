#lang racket/base

(require (for-syntax racket/base
                     syntax/parse
                     (only-in unstable/sequence
                              in-syntax)
                     (only-in racket/syntax
                              format-id
                              generate-temporary))

         (only-in racket/function
                  thunk)
         (only-in racket/match
                  match-define))

(module+ test
  (require rackunit))

(define root-logger (current-logger))
(define loggers (make-hasheq))

(define (reset-loggers!)
  (set! loggers (make-hasheq)))

(define (parent-name name)
  (define str  (symbol->string name))
  (define last (sub1 (string-length str)))
  (define end
    (for/or ([i (in-range last 0 -1)]
             [c (in-string str last 0 -1)])
      (and (char=? c #\.) i)))
  
  (and end (string->symbol (substring str 0 end))))

(module+ test
  (check-eq?   (parent-name 'a.b.c) 'a.b)
  (check-eq?   (parent-name 'a.b)   'a)
  (check-false (parent-name 'a)))

(define (find-or-create-logger! name)
  (hash-ref! loggers name (thunk (create-logger name))))

(define (create-logger name)
  (define parent
    (cond [(parent-name name) => find-or-create-logger!]
          [else root-logger]))
  (make-logger name parent)) 

(module+ test
  (check-eq? (find-or-create-logger! 'a)
             (find-or-create-logger! 'a))
  
  (check-eq? (find-or-create-logger! 'a.b)
             (find-or-create-logger! 'a.b))
  
  #;
  (define (t)
    (define a.b
      (find-or-create-logger! 'a.b))
    (define a
      (find-or-create-logger! 'a))
    (define receiver
      (make-log-receiver a 'info))
    
    (define (serve)
      (sync (handle-evt receiver
                        (lambda (v)
                          (printf "logging: ~a~%" v)
                          (serve)))))
    (thread serve)
    (values a a.b)))

(define-syntax (rawlog stx)
  (syntax-parse stx
    [(_ logger:id level:id fmt-str fmt-v:expr ... (~seq kw:keyword v) ...)
     #:with
     (kvp ...)
     (for/list ([k (in-syntax #'(kw ...))]
                [v (in-syntax #'(v  ...))])
       #`(cons '#,(datum->syntax k (string->symbol (keyword->string (syntax->datum k))))
               #,v))
     #`(when (log-level? logger 'level)
         (let ([message (format fmt-str fmt-v ...)])
           (log-message logger
                        'level
                        message
                        (log-entry (current-milliseconds)
                                   (logger-name logger)
                                   message
                                   (current-continuation-marks)
                                   (make-immutable-hash (list kvp ...))))))]))

(define-syntax (define-logging stx)
  (syntax-parse stx
    [(_ log:id level:id logger:id) 
     (syntax/loc stx
       (define-syntax-rule (log fmt-str v (... ...))
         (rawlog logger level fmt-str v (... ...))))]))

(define-syntax (declare-logger stx)
  (syntax-parse stx
    [(_ logger-name:id)
     #:with 
     logger
     (generate-temporary #'logger-name)
     
     #:with 
     (log-stx ...)
     (for/list ([x '(fatal error warning info debug)])
       (define log (format-id #'logger-name "log-~a" x))
       (define lvl (datum->syntax stx x))
       #`(define-logging #,log #,lvl logger))
     
     (quasisyntax/loc stx
       (begin
        (define logger (find-or-create-logger! 'logger-name))
        log-stx ...))]))

(struct log-entry (timestamp category level message marks extra))

; which logger and level?
; what state to haul around?
;  
; (make-log-appender logger? log-level? any?) -> log-appender?
; (close-log-appender log-appender)


(provide find-or-create-logger!
         declare-logger)

