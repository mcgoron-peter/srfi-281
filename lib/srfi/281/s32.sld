; SPDX-FileCopyrightText: 2026 Peter McGoron
;
; SPDX-License-Identifier: MIT

(define-library (srfi 281 s32)
  (import (scheme base) (only (srfi 281 base) endianness?))
  (export bytevector-s32-ref
          bytevector-s32-set!
          bytevector-s32-native-ref
          bytevector-s32-native-set!)
  (include-library-declarations "internal.scm")
  (include-library-declarations "internal-fixed.scm")
  (begin
    (define-signed-for-fixed-width 4
      bytevector-s32-ref
      bytevector-s32-set!
      bytevector-s32-native-ref
      bytevector-s32-native-set!)))