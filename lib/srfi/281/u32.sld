; SPDX-FileCopyrightText: 2026 Peter McGoron
;
; SPDX-License-Identifier: MIT

(define-library (srfi 281 u32)
  (export bytevector-u32-ref
          bytevector-u32-set!
          bytevector-u32-native-ref
          bytevector-u32-native-set!)
  (cond-expand
    ((library (rnrs bytevectors))
     (import (rnrs bytevectors)))
    (else
     (import (scheme base) (only (srfi 281 base) endianness? native-endianness))
     (include-library-declarations "internal.scm")
     (include-library-declarations "internal-fixed.scm")
     (begin
       (define-unsigned-for-fixed-width 4
         bytevector-u32-ref
         bytevector-u32-set!
         bytevector-u32-native-ref
         bytevector-u32-native-set!)))))