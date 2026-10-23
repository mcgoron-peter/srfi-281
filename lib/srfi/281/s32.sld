; SPDX-FileCopyrightText: 2026 Peter McGoron
;
; SPDX-License-Identifier: MIT

(define-library (srfi 281 s32)
  (export bytevector-s32-ref
          bytevector-s32-set!
          bytevector-s32-native-ref
          bytevector-s32-native-set!)
  (cond-expand
    ((library (rnrs bytevectors))
     (import (rnrs bytevectors)))
    (else
     (import (scheme base) (only (srfi 281 base) endianness? native-endianness))
     (include-library-declarations "internal.scm")
     (include-library-declarations "internal-fixed.scm")
     (begin
       (define-signed-for-fixed-width 4
         bytevector-s32-ref
         bytevector-s32-set!
         bytevector-s32-native-ref
         bytevector-s32-native-set!)))))