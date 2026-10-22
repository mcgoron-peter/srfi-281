(define (test-endianness)
  (test-group "endianness?"
    (test-assert "big endian" (endianness? 'big))
    (test-assert "little endian" (endianness? 'little))
    (test-assert "native endianness" (endianness? (native-endianness)))
    (test-equal
     "(endianness big)"
     'big
     (eval '(endianness big) (environment '(srfi 281))))
    (test-equal
     "(endianness little)"
     'little
     (eval '(endianness little) (environment '(srfi 281))))))
