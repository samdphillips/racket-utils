#lang racket/base

(require ffi/unsafe)

(define-cpointer-type _tm)

(define gmtime
  (get-ffi-obj 'gmtime #f
               (_fun (_ptr i _long) -> _tm)))

(define localtime
  (get-ffi-obj 'localtime #f
               (_fun (_ptr i _long) -> _tm)))

(define c:strftime
  (get-ffi-obj 'strftime #f
               (_fun (out : (_bytes o len))
                     (len : _uint = 512)
                     _string
                     _tm
                     -> (outlen : _uint)
                     -> (bytes->string/latin-1
                         (subbytes out 0 outlen)))))

(define (strftime fmt [time (quotient (current-milliseconds) 1000)])
  (c:strftime fmt (gmtime time)))

(provide strftime)

