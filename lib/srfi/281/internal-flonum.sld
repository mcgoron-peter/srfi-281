; SPDX-FileCopyrightText: 2026 Peter McGoron
;
; SPDX-License-Identifier: MIT

(define-library (srfi 281 internal-flonum)
  (import (scheme base) (scheme case-lambda)
          (srfi 143)
          (only (srfi 281 base) endianness?))
  (export convert-to-representation sign-negative? bits->flonum)
  (cond-expand
    ((library (srfi 1))
     (import (only (srfi 1) drop-right)))
    (else
     (begin
       (define (drop-right lst n)
         (let ((len (length lst)))
           (take lst (- len n))))
       (define (take lst n)
         (cond
           ((null? lst) '())
           ((not (positive? n)) '())
           (else (cons (car lst)
                       (take (cdr lst) (- n 1)))))))))
  (include "internal-flonum.scm"))


