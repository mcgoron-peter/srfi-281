;; TODO: Test elements of error object
(define-syntax test-raises-unicode-error
  (syntax-rules ()
    ((_ expression)
     (test-assert
      (guard (x (else (unicode-decoding-error? x)))
        expression
        #f)))))
