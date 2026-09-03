; SPDX-FileCopyrightText: 2026 Peter McGoron
;
; SPDX-License-Identifier: MIT

(define-library (srfi 281 s16)
  (import (scheme base) (only (srfi 281 base) endianness?))
  (export bytevector-s16-ref
          bytevector-s16-set!
          bytevector-s16-native-ref
          bytevector-s16-native-set!)
  (include-library-declarations "internal.scm")
  (include-library-declarations "internal-fixed.scm")
  (begin
    (define-signed-for-fixed-width 2
      bytevector-s16-ref
      bytevector-s16-set!
      bytevector-s16-native-ref
      bytevector-s16-native-set!)))