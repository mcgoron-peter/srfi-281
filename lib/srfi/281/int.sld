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
     (import (only (rename (rnrs bytevectors)
                           (uint-list->bytevector %uint-list->bytevector)
                           (sint-list->bytevector %sint-list->bytevector)
                           (bytevector->uint-list %bytevector->uint-list)
                           (bytevector->sint-list %bytevector->sint-list))
                   bytevector-uint-ref
                   bytevector-uint-set!
                   bytevector-sint-ref
                   bytevector-sint-set!
                   %uint-list->bytevector
                   %bytevector->uint-list
                   %sint-list->bytevector
                   %bytevector->sint-list)
             (scheme base) (srfi 1) (scheme case-lambda))
     (begin
       (define (list-converter proc)
         (case-lambda
           ((list endianness size)
            (proc list endianness size))
           ((list endianness size start)
            (proc (drop list start) endianness size))
           ((list endianness size start end)
            (proc (take (drop list start) (- end start))
                  endianness size))))
       (define uint-list->bytevector
         (list-converter %uint-list->bytevector))
       (define sint-list->bytevector
         (list-converter %sint-list->bytevector))
       (define (bytevector-converter proc)
         (case-lambda
           ((bv endianness size)
            (proc bv endianness size))
           ((bv endianness size start)
            (proc (bytevector-copy bv start) endianness size))
           ((bv endianness size start end)
            (proc (bytevector-copy bv start end) endianness size))))
       (define bytevector->uint-list
         (bytevector-converter %bytevector->uint-list))
       (define bytevector->sint-list
         (bytevector-converter %bytevector->sint-list))))
    (else (import (except (scheme base) make-bytevector)
                  (scheme case-lambda)
                  (srfi 281 base))
          (include-library-declarations "internal.scm")
          (include-library-declarations "internal-fixed.scm")
          (include "int.scm"))))
