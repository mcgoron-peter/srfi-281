(define (test-bytevector->base64-and-base64->bytevector-inverses)
  (test-group "bytevector->base64 and base64->bytevector are inverses, 1"
    (test-property
     (lambda (bv+range)
       (let ((bv (vector-ref bv+range 0))
             (start (vector-ref bv+range 1))
             (end (vector-ref bv+range 2)))
         (equal? (base64->bytevector (bytevector->base64
                                      bv
                                      #f
                                      start
                                      end))
                 (bytevector-copy bv start end))))
     (list (bytevector+range-generator)))))

