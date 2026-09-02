(define-library (srfi NNN u32)
  (import (scheme base) (only (srfi NNN base) endianness?))
  (export bytevector-u32-ref
          bytevector-u32-set!
          bytevector-u32-native-ref
          bytevector-u32-native-set!)
  (include-library-declarations "internal.scm")
  (include-library-declarations "internal-fixed.scm")
  (begin
    (define-unsigned-for-fixed-width 4
      bytevector-u32-ref
      bytevector-u32-set!
      bytevector-u32-native-ref
      bytevector-u32-native-set!)))