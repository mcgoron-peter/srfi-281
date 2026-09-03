; SPDX-FileCopyrightText: 2026 Peter McGoron
;
; SPDX-License-Identifier: MIT

(define-library (srfi 281)
  (import (scheme base)
          (srfi 281 base) (srfi 281 u8) (srfi 281 serialization))
  (export endianness? native-endianness
          bytevector? make-bytevector bytevector-fill!
          bytevector-length
          bytevector=? bytevector<? bytevector<=?
          bytevector>? bytevector>=?
          bytevector-copy
          bytevector-u8-ref bytevector-u8-set!
          u8-list->bytevector bytevector->u8-list
          bytevector->hex-string hex-string->bytevector
          bytevector->base64 base64->bytevector)
  (cond-expand
    ((or gauche chicken (library (srfi 281 endianness)))
     (import (srfi 281 endianness))
     (export endianness))
    (else))
  (cond-expand
    ((or tr7 gauche chicken (library (srfi 281 s8)))
     (import (srfi 281 s8))
     (export bytevector-s8-ref bytevector-s8-set!))
    (else))
  (cond-expand
    ((or tr7 gauche chicken (library (srfi 281 int)))
     (import (srfi 281 int))
     (export bytevector-uint-ref bytevector-uint-set!
             bytevector-sint-ref bytevector-sint-set!
             uint-list->bytevector bytevector->uint-list
             sint-list->bytevector bytevector->sint-list))
    (else))
  (cond-expand
    ((or tr7 gauche chicken (library (srfi 281 u16)))
     (import (srfi 281 u16))
     (export bytevector-u16-ref bytevector-u16-set!
             bytevector-u16-native-ref bytevector-u16-native-set!))
    (else))
  (cond-expand
    ((or tr7 gauche chicken (library (srfi 281 s16)))
     (import (srfi 281 s16))
     (export bytevector-s16-ref bytevector-s16-set!
             bytevector-s16-native-ref bytevector-s16-native-set!))
    (else))
  (cond-expand
    ((or (and |64bit| tr7) gauche chicken (library (srfi 281 u32)))
     (import (srfi 281 u32))
     (export bytevector-u32-ref bytevector-u32-set!
             bytevector-u32-native-ref bytevector-u32-native-set!))
    (else))
  (cond-expand
    ((or (and |64bit| tr7) gauche chicken (library (srfi 281 s32)))
     (import (srfi 281 s32))
     (export bytevector-s32-ref bytevector-s32-set!
             bytevector-s32-native-ref bytevector-s32-native-set!))
    (else))
  (cond-expand
    ((or gauche chicken (library (srfi 281 s64)))
     (import (srfi 281 s64))
     (export bytevector-s64-ref bytevector-s64-set!
             bytevector-s64-native-ref bytevector-s64-native-set!))
    (else))
  (cond-expand
    ((or gauche chicken (library (srfi 281 u64)))
     (import (srfi 281 u64))
     (export bytevector-u64-ref bytevector-u64-set!
             bytevector-u64-native-ref bytevector-u64-native-set!))
    (else))
  (cond-expand
    ((or gauche chicken tr7 (library (srfi 281 f32)))
     (import (srfi 281 f32))
     (export bytevector-binary32-set!
             bytevector-ieee-single-set!
             bytevector-binary32-ref
             bytevector-ieee-single-ref
             bytevector-binary32-native-set!
             bytevector-ieee-single-native-set!
             bytevector-binary32-native-ref
             bytevector-ieee-single-native-ref)))
  (cond-expand
    ((or gauche chicken tr7 (library (srfi 281 f64)))
     (import (srfi 281 f64))
     (export bytevector-binary64-set!
             bytevector-ieee-single-set!
             bytevector-binary64-ref
             bytevector-ieee-single-ref
             bytevector-binary64-native-set!
             bytevector-ieee-single-native-set!
             bytevector-binary64-native-ref
             bytevector-ieee-single-native-ref)))
  (cond-expand
    ((or gauche chicken tr7 (library (srfi 281 unicode)))
     (import (srfi 281 unicode))
     (export error-handling-mode?
             i/o-decoding-error?
             string->utf8 string->utf16 string->utf32
             utf8->string utf16->string utf32->string)))
  (cond-expand
    ((or gauche chicken (library (srfi 281 error-handling-mode)))
     (import (srfi 281 error-handling-mode))
     (export error-handling-mode))))