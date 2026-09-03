; SPDX-FileCopyrightText: 2026 Peter McGoron
;
; SPDX-License-Identifier: MIT

(define-library (srfi 281 int)
  (export bytevector-uint-ref bytevector-uint-set!
          bytevector-sint-ref bytevector-sint-set!
          uint-list->bytevector bytevector->uint-list
          sint-list->bytevector bytevector->sint-list)
  (cond-expand
    ((library (rnrs bytevectors))
     (import (only (rnrs bytevectors)
                   bytevector-uint-ref
                   bytevector-uint-set!
                   bytevector-sint-ref
                   bytevector-sint-set!)))
    (else (import (except (scheme base) make-bytevector)
                  (scheme case-lambda)
                  (srfi 281 base))
          (include-library-declarations "internal.scm")
          (include-library-declarations "internal-fixed.scm")
          (include "int.scm"))))
