(define-library (srfi NNN serialization)
  (import (scheme base) (scheme case-lambda))
  (export bytevector->hex-string
          hex-string->bytevector
          bytevector->base64
          base64->bytevector)
  (cond-expand
    (chicken
     (import (rename (chicken fixnum)
                     (fxshl fxarithmetic-shift-left)
                     (fxshr fxarithmetic-shift-right))))
    ((library (srfi 143))
     (import (srfi 143))))
  (include "serialization.scm"))