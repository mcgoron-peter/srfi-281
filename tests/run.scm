; SPDX-FileCopyrightText: 2026 Peter McGoron
;
; SPDX-License-Identifier: MIT

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

(include "prelude.scm")
(test-begin "SRFI 281")
(test-skip (lambda (x) skip-current-tests?))

(define skip-big-tests? #t)

(include "endiannessp.scm")
(test-endianness)

(include "make-bytevector.scm")
(test-make-bytevector-simple)
(test-make-bytevector-negative-numbers)
(test-make-bytevector-non-negative-numbers)

(include "bytevectoreqp.scm")
(test-bytevector=?)

(include "bytevector.scm")
(test-bytevector)

(include "bytevector-fill.scm")
(test-bytevector-fill-partially)
(test-bytevector-fill-exhaustive-negative)
(test-bytevector-fill-exhaustive-non-negative)

(include "u8-list-to-bytevector.scm")
(test-u8-list->bytevector-and-bytevector->u8-list)
(test-u8-list-r6rs-examples)

(include "s8.scm")
(test-bytevector-s8-ref-negative)
(test-bytevector-s8-ref-non-negative)
(test-bytevector-s8-set!-negative)
(test-bytevector-s8-set!-non-negative)
(test-bytevector-s8-ref-and-bytevector-s8-set!-inverses)

(include "uint.scm")
(test-bytevector-uint-ref-endianness)

(include "sint.scm")
(test-bytevector-sint-ref-endianness)
(test-bytevector-sint-ref-and-bytevector-sint-set!-inverses)

(when skip-big-tests?
  (set! skip-current-tests? #t))
(include "uint-list.scm")
(test-bytevector->uint-list-and-uint-list->bytevector-inverses)
(include "sint-list.scm")
(test-bytevector->sint-list-and-sint-list->bytevector-inverses)
(set! skip-current-tests? #f)

(when skip-big-tests?
  (set! skip-current-tests? #t))
(include "u16.scm")
(bytevector-u16-big-exhaustive)
(bytevector-u16-little-exhaustive)
(include "s16.scm")
(bytevector-s16-big-exhaustive)
(bytevector-s16-little-exhaustive)
(set! skip-current-tests? #f)

(include "u32.scm")
(test-u32-property)
(include "s32.scm")
(test-s32-property)

(include "u64.scm")
(test-u64-property)
(include "s64.scm")
(test-s64-property)

(include "bytevector-to-hex-string.scm")
(test-bytevector->hex-string)
(include "hex-string-to-bytevector.scm")
(test-hex-string->bytevector)

(include "hex-string-to-bytevector-inversion.scm")
(test-hex-string->bytevector-and-bytevector->hex-string-1)
(test-hex-string->bytevector-and-bytevector->hex-string-2)

(include "bytevector-to-base64.scm")
(test-bytevector->base64)
(include "base64-to-bytevector.scm")
(test-base64->bytevector)
(test-base64->bytevector-start-end)
(include "bytevector-base64-inverse.scm")
(test-bytevector->base64-and-base64->bytevector-inverses)

(include "binary32.scm")
(test-binary32)
(test-binary32-native)
(test-binary32-subnormals)

(include "binary64.scm")
(test-binary64)
(test-binary64-native)
(test-binary64-subnormals)

(include "error-handling-mode.scm")
(test-error-handling-mode?)
(include "error-handling-mode-macro.scm")
(test-error-handling-mode)

(include "unicode-error.scm")
(include "utf8.scm")
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

(include "utf16.scm")
(exhaustive-utf16-scalar)
(exhaustive-utf16-lone-surrogates)
(set! skip-current-tests? #f)
(string->utf16-and-utf16->string-inverses)
(test-utf16-bom)

(when skip-big-tests?
  (set! skip-current-tests? #t))
(include "utf32.scm")
(exhaustive-utf32)
(set! skip-current-tests? #f)

(string->utf32-and-utf32->string-inverses)
(test-utf32-bom)

(test-end "SRFI 281")
