#lang racket/base

(require (for-syntax racket/base
                     syntax/parse
                     (only-in racket/syntax
                              format-id
                              generate-temporary))
         (only-in racket/function
                  thunk))

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

(define-syntax (define-log-syntax stx)
  (syntax-parse stx
    [(_ log:id level:id logger:id) 
     (syntax/loc stx
       (define-syntax-rule (log fmt-str v (... ...))
         (when (log-level? logger 'level)
           (log-message logger 
                        'level 
                        (format fmt-str v (... ...))
                        (current-continuation-marks)))))]))

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
       #`(define-log-syntax #,log #,lvl logger))
     
     (quasisyntax/loc stx
       (begin
        (define logger (find-or-create-logger! 'logger-name))
        log-stx ...))]))

(provide find-or-create-logger!
         define-log-syntax
         declare-logger)
