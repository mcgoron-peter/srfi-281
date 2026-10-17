; SPDX-FileCopyrightText: 2026 Peter McGoron
;
; SPDX-License-Identifier: MIT

(define-library (srfi 281 u64)
  (export bytevector-u64-ref
          bytevector-u64-set!
          bytevector-u64-native-ref
          bytevector-u64-native-set!)
  (cond-expand
    ((library (rnrs bytevectors))
     (import (rnrs bytevectors)))
    (else
     (import (scheme base) (only (srfi 281 base) endianness?))
     (include-library-declarations "internal.scm")
     (include-library-declarations "internal-fixed.scm")
     (begin
       (define-unsigned-for-fixed-width 8
         bytevector-u64-ref
         bytevector-u64-set!
         bytevector-u64-native-ref
         bytevector-u64-native-set!)))))