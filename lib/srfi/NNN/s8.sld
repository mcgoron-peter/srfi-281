(define-library (srfi NNN s8)
  (import (scheme base))
  (export bytevector-s8-ref bytevector-s8-set!)
  (cond-expand
    ((library (rnrs bytevectors))
     (import (only (rnrs bytevectors)
                   bytevector-s8-ref bytevector-s8-set!)))
    (else (include-library-declarations "internal.scm")
          (include "s8.scm"))))