(define (test-bytevector)
  (test-group "bytevector"
    (test-equal #u8(#x80 #xFF 0 1 #x80)
                (bytevector -128 -1 0 1 128))))

