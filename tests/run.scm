; SPDX-FileCopyrightText: 2026 Peter McGoron
;
; SPDX-License-Identifier: MIT

;; TODO: This test suite should be refactored to make it easier to test
;; individual components.

(import (except (scheme base)
                make-bytevector
                bytevector
                bytevector-u8-ref
                bytevector-u8-set!
                string->utf8
                utf8->string)
        (scheme inexact)
        (scheme write)
        (scheme eval)                  ; Optional
        (srfi 1)
        (rename (srfi 64) (test-group %test-group))
        (srfi 158)
        (srfi 194)
        (rename (srfi 252)
                (test-property %test-property))
        (srfi 281))

(include "definitions.scm")
(test-begin "SRFI 281")
(test-skip (lambda (x) skip-current-tests?))

(define skip-big-tests? #t)

(test-endianness)

(test-make-bytevector-simple)
(test-make-bytevector-negative-numbers)
(test-make-bytevector-non-negative-numbers)

(test-bytevector=?)

(test-bytevector)

(test-bytevector-fill-partially)
(test-bytevector-fill-exhaustive-negative)
(test-bytevector-fill-exhaustive-non-negative)

(test-u8-list->bytevector-and-bytevector->u8-list)
(test-u8-list-r6rs-examples)

(test-bytevector-s8-ref-negative)
(test-bytevector-s8-ref-non-negative)
(test-bytevector-s8-set!-negative)
(test-bytevector-s8-set!-non-negative)
(test-bytevector-s8-ref-and-bytevector-s8-set!-inverses)

(test-bytevector-uint-ref-endianness)
(test-bytevector-sint-ref-endianness)
(test-bytevector-sint-ref-and-bytevector-sint-set!-inverses)
(when skip-big-tests?
  (set! skip-current-tests? #t))
(test-bytevector->uint-list-and-uint-list->bytevector-inverses)
(test-bytevector->sint-list-and-sint-list->bytevector-inverses)
(set! skip-current-tests? #f)

(when skip-big-tests?
  (set! skip-current-tests? #t))
(bytevector-u16-big-exhaustive)
(bytevector-u16-little-exhaustive)
(bytevector-s16-big-exhaustive)
(bytevector-s16-little-exhaustive)
(set! skip-current-tests? #f)

(test-u32-property)
(test-s32-property)

(test-u64-property)
(test-s64-property)

(test-bytevector->hex-string)
(test-hex-string->bytevector)
(test-hex-string->bytevector-and-bytevector->hex-string-1)
(test-hex-string->bytevector-and-bytevector->hex-string-2)

(test-bytevector->base64)
(test-base64->bytevector)
(test-bytevector->base64-and-base64->bytevector-inverses)
(test-base64->bytevector-start-end)

(test-binary32)
(test-binary32-native)
(test-binary32-subnormals)

(test-binary64)
(test-binary64-native)
(test-binary64-subnormals)

(test-error-handling-mode?)
(test-error-handling-mode)

(test-utf8-extreme)
(test-utf8-truncation)
(test-string->utf8-and-utf8->string-inverses)
(test-above-U+10FFFF)

(when skip-big-tests?
  (set! skip-current-tests? #t))
(exhaustive-utf8)
(exhaustive-two-byte-overlong)
(exhaustive-three-byte-overlong)
(exhaustive-three-byte-truncated)
(exhaustive-utf8-surrogate)
(exhaustive-utf16-scalar)
(exhaustive-utf16-lone-surrogates)
(exhaustive-utf32)
(set! skip-current-tests? #f)

(string->utf16-and-utf16->string-inverses)
(test-utf16-bom)

(string->utf32-and-utf32->string-inverses)
(test-utf32-bom)

(test-end "SRFI 281")
