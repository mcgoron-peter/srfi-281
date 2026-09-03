; SPDX-FileCopyrightText: 2026 Peter McGoron
;
; SPDX-License-Identifier: MIT

(define-library (srfi 281 endianness)
  (cond-expand
    (chicken
     (import (scheme base)
             (only (chicken syntax)
                   er-macro-transformer)))
    (mit
     (import (scheme base)
             (only (mit legacy runtime)
                   er-macro-transformer)))
    (chibi (import (scheme base) (chibi)))
    (gauche (import (scheme base) (gauche base)))
    (gambit (import (gambit)))
    ((library (rnrs bytevectors)) (import (rnrs bytevectors))))
  (export endianness)
  (cond-expand
    ((or chicken mit chibi gauche)
     (begin
       (define-syntax endianness
         (er-macro-transformer
          (lambda (exp rename compare)
            (unless (and (list? exp)
                         (= (length exp) 2))
              (error "syntax: (endianness symbol)" exp))
            (let ((sym (cadr exp)))
              (unless (symbol? sym)
                (error "not a symbol" sym))
              ;; To avoid issues with uninterned symbols, this macro
              ;; looks at the name of the symbol by converting it to a
              ;; string.
              (let ((str (symbol->string sym)))
                (cond
                  ((string=? str "little") `(,(rename 'quote) little))
                  ((string=? str "big") `(,(rename 'quote) big))
                  (else (error "invalid endianness" sym))))))))))
    (gambit
     (begin
       (define-macro (endianness symbol)
         (unless (symbol? symbol)
           (error "not a symbol" symbol))
         (let (str (symbol->string symbol))
           (cond
             ((string=? str "little") `(quote little))
             ((string=? str "big") `(quote big))
             (else (error "invalid endianness" symbol)))))))
    #|
(has-syntax-case
     (define-syntax endianness
       (lambda (x)
         (syntax-case x ()
           ((_ sym)
            (identifier? sym)
            (let ((sym (syntax->datum sym)))
              (case sym
                ((little big) #'(quote sym))
                (else (syntax-violation 'endianness
                                        "not an endianness"
                                        x
                                        sym)))))))))
|#
    (else)))