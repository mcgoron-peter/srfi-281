; SPDX-FileCopyrightText: 2026 Peter McGoron
;
; SPDX-License-Identifier: MIT

(define-library (srfi 281 u16)
  (export bytevector-u16-ref
          bytevector-u16-set!
          bytevector-u16-native-ref
          bytevector-u16-native-set!)
  (cond-expand
    ((library (rnrs bytevectors))
     (import (rnrs bytevectors)))
    (else
     (import (scheme base) (only (srfi 281 base) endianness? native-endianness))
     (include-library-declarations "internal.scm")
     (include-library-declarations "internal-fixed.scm")
     (begin
       (define-unsigned-for-fixed-width 2
         bytevector-u16-ref
         bytevector-u16-set!
         bytevector-u16-native-ref
         bytevector-u16-native-set!)))))