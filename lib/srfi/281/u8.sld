(define-library (srfi 281 u8)
  (import (scheme base) (scheme case-lambda))
  (export bytevector-u8-ref
          bytevector-u8-set!
          u8-list->bytevector
          bytevector->u8-list)
  (include "u8.scm"))