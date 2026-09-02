(define-library (srfi 281 s64)
  (import (scheme base) (only (srfi 281 base) endianness?))
  (export bytevector-s64-ref
          bytevector-s64-set!
          bytevector-s64-native-ref
          bytevector-s64-native-set!)
  (include-library-declarations "internal.scm")
  (include-library-declarations "internal-fixed.scm")
  (begin
    (define-signed-for-fixed-width 8
      bytevector-s64-ref
      bytevector-s64-set!
      bytevector-s64-native-ref
      bytevector-s64-native-set!)))