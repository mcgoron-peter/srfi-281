(define (test-bytevector-uint-ref-endianness)
  (test-group "bytevector-uint-ref endianness"
    (test-equal
     "big endian"
     #x123456781234567812345678
     (bytevector-uint-ref #u8(#x12 #x34 #x56 #x78 #x12 #x34 #x56 #x78 #x12 #x34 #x56 #x78)
                          0
                          'big
                          12))
    (test-equal
     "little endian"
     #x123456781234567812345678
     (bytevector-uint-ref #u8(#x78 #x56 #x34 #x12 #x78 #x56 #x34 #x12 #x78 #x56 #x34 #x12)
                          0
                          'little
                          12))))
