(define (test-bytevector->sint-list-and-sint-list->bytevector-inverses)
  (test-group "bytevector->sint-list and sint-list->bytevector are inverses"
    (do ((i 1 (+ i 1)))
        ((= i max-test-integer-width-in-bytes))
      (display i) (newline)
      (test-property
       (test-inversion i bytevector->sint-list sint-list->bytevector)
       (list (make-signed-int-list+range-generator i)
             (endianness-generator))))))

