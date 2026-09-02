(define-library (srfi 281 f64)
  (import (scheme base)
          (scheme inexact)
          (only (srfi 281 base)
                endianness?
                native-endianness
                bytevector-fill!)
          (srfi 143)
          (srfi 281 internal-flonum)
          (srfi 281 u64))
  (export bytevector-binary64-set!
          (rename bytevector-binary64-set!
                  bytevector-ieee-single-set!)
          bytevector-binary64-ref
          (rename bytevector-binary64-ref
                  bytevector-ieee-single-ref)
          bytevector-binary64-native-set!
          (rename bytevector-binary64-native-set!
                  bytevector-ieee-single-native-set!)
          bytevector-binary64-native-ref
          (rename bytevector-binary64-native-ref
                  bytevector-ieee-single-native-ref))
  (cond-expand
    ((library (srfi 208))
     (import (srfi 208)))
    (else))
  (cond-expand
    ((library (srfi 160))
     (import (only (srfi 160) u8vector-reverse-copy!)))
    (else
     (begin
       (define (u8vector-reverse-copy! to at from)
         (do ((i (- (bytevector-length from) 1)
                 (- i 1))
              (at at (+ at 1)))
             ((negative? i))
           (bytevector-u8-set! to at (bytevector-u8-ref from i)))))))
  (include "f64.scm"))