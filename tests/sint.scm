(define (test-bytevector-sint-ref-endianness)
  (test-group "bytevector-sint-ref endianness"
    (let ((bv (make-bytevector 12)))
      (test-equal
       "big endian"
       #u8(#x12 #x34 #x56 #x78 #x12 #x34 #x56 #x78 #x12 #x34 #x56 #x78)
       (begin
         (bytevector-sint-set! bv
                               0
                               #x123456781234567812345678
                               'big
                               12)
         bv))
      (test-equal
       "little endian"
       #u8(#x78 #x56 #x34 #x12 #x78 #x56 #x34 #x12 #x78 #x56 #x34 #x12)
       (begin
         (bytevector-sint-set! bv
                               0
                               #x123456781234567812345678
                               'little
                               12)
         bv)))))

(define (test-bytevector-sint-ref-and-bytevector-sint-set!-inverses)
  (test-group "bytevector-sint-ref and bytevector-sint-set! are inverses"
    (test-property
     (lambda (width endianness)
       (let* ((bv (make-bytevector width))
              (generator (make-random-signed-integer-generator width))
              (i (generator)))
         (bytevector-sint-set! bv
                               0
                               i
                               endianness
                               width)
         (= (bytevector-sint-ref bv 0 endianness width)
            i)))
     (list (make-random-integer-generator
            1
            (+ max-test-integer-width-in-bytes))
           (endianness-generator)))))
