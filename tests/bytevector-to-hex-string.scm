(define (test-bytevector->hex-string)
  (test-group "bytevector->hex-string"
    (test-equal "AABBCCDD"
                (bytevector->hex-string #u8(#xAA #xBB #xCC #xDD)))
    (test-equal "BBCCDD"
                (bytevector->hex-string #u8(#xAA #xBB #xCC #xDD)
                                        1))
    (test-equal "BBCC"
                (bytevector->hex-string #u8(#xAA #xBB #xCC #xDD)
                                        1
                                        3))))

