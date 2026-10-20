(define (test-bytevector->uint-list-and-uint-list->bytevector-inverses)
  (test-group "bytevector->uint-list and uint-list->bytevector are inverses"
    (do ((i 1 (+ i 1)))
        ((= i max-test-integer-width-in-bytes))
      (display i) (newline)
      (test-property
       (test-inversion i bytevector->uint-list uint-list->bytevector)
       (list (make-unsigned-int-list+range-generator i)
             (endianness-generator))))))

