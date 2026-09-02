(define-library (srfi NNN unicode)
  (import (scheme base) (scheme case-lambda) (srfi 143)
          (only (srfi NNN base) endianness?)
          (srfi NNN u16))
  (export error-handling-mode?
          i/o-decoding-error?
          string->utf8 string->utf16 string->utf32
          utf8->string utf16->string utf32->string)
  (cond-expand
    ((library (srfi NNN u32))
     (import (srfi NNN u32)))
    (else))
  (cond-expand
    ((library (rnrs io ports))
     (import (only (rnrs io ports)
                   i/o-decoding-error?
                   make-i/o-decoding-error)
             (only (rnrs conditions)
                   condition
                   make-message-condition
                   make-who-condition
                   make-irritants-condition))
     (begin
       (define (raise-i/o-decoding-error who obj message . irritants)
         (raise (condition (make-who-condition who)
                           (make-message-condition message)
                           (make-irritants-condition irritants)
                           (make-i/o-decoding-error obj))))))
    (else
     (begin
       (define (i/o-decoding-error? obj)
         (and (error-object? obj)
              (let ((irritants (error-object-irritants obj)))
                (and (pair? irritants)
                     (eq? (car irritants) 'i/o-decoding-error)))))
       (define (raise-i/o-decoding-error who obj message . irritants)
         (apply error
                message
                'i/o-decoding-error
                who
                obj
                irritants)))))
  (include "unicode.scm"))