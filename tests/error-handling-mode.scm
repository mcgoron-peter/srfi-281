(define (test-error-handling-mode?)
  (test-group "error-handling-mode?"
    (test-assert (error-handling-mode? 'raise))
    (test-assert (error-handling-mode? 'ignore))
    (test-assert (error-handling-mode? 'replace))))

