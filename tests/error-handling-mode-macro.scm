(define (test-error-handling-mode)
  (test-group "error-handling-mode"
    (let ((test
           (lambda (sym)
             (test-equal
              sym
              (eval `(error-handling-mode ,sym)
                    (environment '(srfi 281 error-handling-mode)))))))
      (test 'raise)
      (test 'replace)
      (test 'ignore))))

