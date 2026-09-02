(define-library (srfi 281 error-handling-mode)
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
  (export error-handling-mode)
  (cond-expand
    ((or chicken mit chibi gauche)
     (begin
       (define-syntax error-handling-mode
         (er-macro-transformer
          (lambda (exp rename compare)
            (unless (and (list? exp)
                         (= (length exp) 2))
              (error "syntax: (error-handling-mode symbol)" exp))
            (let ((sym (cadr exp)))
              (unless (symbol? sym)
                (error "not a symbol" sym))
              ;; To avoid issues with uninterned symbols, this macro
              ;; looks at the name of the symbol by converting it to a
              ;; string.
              (let ((str (symbol->string sym)))
                (cond
                  ((member str '("raise" "replace" "ignore"))
                   `(,(rename 'quote) ,sym))
                  (else (error "invalid error-handling-mode" sym))))))))))
    (gambit
     (begin
       (define-macro (error-handling-mode symbol)
         (unless (symbol? symbol)
           (error "not a symbol" symbol))
         (let (str (symbol->string symbol))
           (cond
             ((member str '("raise" "replace" "ignore"))
              `(quote ,symbol))
             (else (error "invalid error-handling-mode" symbol)))))))
    (else)))