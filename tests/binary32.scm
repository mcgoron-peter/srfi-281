(define (test-binary32)
  (test-group "binary32-set! and binary32-ref"
    (letrec-syntax ((test-endian
                     (syntax-rules ()
                       ((_ name expected input endianness)
                        (begin
                          (test-equal
                           (string-append name " set!")
                           expected
                           (let ((bv (make-bytevector 4)))
                             (bytevector-binary32-set! bv
                                                       0
                                                       input
                                                       endianness)
                             bv))
                          (test-assert
                           (string-append name " ref")
                           (= input
                              (bytevector-binary32-ref expected
                                                       0
                                                       endianness)))))))
                    (test
                     (syntax-rules ()
                       ((_ name expected input)
                        (begin
                          (test-endian name expected input 'big)
                          (test-endian name
                                       (bytevector-reverse expected)
                                       input
                                       'little))))))
      (test "1" #u8(#x3F #x80 #x00 #x00) 1.0)
      (test "1.5" #u8(#x3F #xC0 #x00 #x00) 1.5)
      (test "-1" #u8(#xBF #x80 #x00 #x00) -1.0)
      (test "0.5" #u8(#x3f #x00 #x00 #x00) 0.5)
      (test "-0.5" #u8(#xBf #x00 #x00 #x00) -0.5)
      (test "0" #u8(#x00 #x00 #x00 #x00) 0.0)
      (test "+inf.0" #u8(#x7F #x80 #x00 #x00) +inf.0)
      (test "-inf.0" #u8(#xFF #x80 #x00 #x00) -inf.0))
    (test-assert "NaN round tripping"
                 (let ((bv (make-bytevector 4)))
                   (bytevector-binary32-native-set! bv
                                                    0
                                                    +nan.0)
                   (nan? (bytevector-binary32-native-ref bv 0))))))

;;; Only test this when floats are binary32 or more precise

(define (test-binary32-native)
  (test-group "idempotency of binary32-native-set! and binary32-native-ref"
    (test-property
     (lambda (x)
       (let ((bv (make-bytevector 4))
             (bv2 (make-bytevector 4)))
         (bytevector-binary32-native-set! bv 0 x)
         (bytevector-binary32-native-set!
          bv2
          0
          (bytevector-binary32-native-ref bv 0))
         (equal? bv bv2)))
     (list (inexact-real-generator)))))

;;; Only test this when floats are binary32 or more precise

(define (test-binary32-subnormals)
  (test-group "binary32 subnormals"
    (letrec-syntax ((test
                     (syntax-rules ()
                       ((_ expected input)
                        (begin
                          (test-approximate
                           expected
                           (bytevector-binary32-ref input 0 'big)
                           0))))))
      (test 1.40129846432481707092e-45
            #u8(0 0 0 1))
      (test 2.80259692864963414185e-45
            #u8(0 0 0 2))
      (test 4.20389539297445121277e-45
            #u8(0 0 0 3))
      (test 1.17549421069244107549e-38
            #u8(0 #x7F #xFF #xFF)))))
