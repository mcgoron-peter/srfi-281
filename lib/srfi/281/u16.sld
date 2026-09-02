(define-library (srfi 281 u16)
  (import (scheme base) (only (srfi 281 base) endianness?))
  (export bytevector-u16-ref
          bytevector-u16-set!
          bytevector-u16-native-ref
          bytevector-u16-native-set!)
  (include-library-declarations "internal.scm")
  (include-library-declarations "internal-fixed.scm")
  (begin
    (define-unsigned-for-fixed-width 2
      bytevector-u16-ref
      bytevector-u16-set!
      bytevector-u16-native-ref
      bytevector-u16-native-set!)))