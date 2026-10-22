(define (test-binary64)
  (test-group "binary64-set! and binary64-ref"
    (letrec-syntax ((test-endian
                     (syntax-rules ()
                       ((_ name expected input endianness)
                        (begin
                          (test-equal
                           (string-append name " set!")
                           expected
                           (let ((bv (make-bytevector 8)))
                             (bytevector-binary64-set! bv
                                                       0
                                                       input
                                                       endianness)
                             bv))
                          (test-assert
                           (string-append name " ref")
                           (= input
                              (bytevector-binary64-ref expected
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
      (test "1" #u8(#x3F #xF0 #x00 #x00 0 0 0 0) 1.0)
      (test "1.5" #u8(#x3F #xF8 #x00 #x00 0 0 0 0) 1.5)
      (test "-1" #u8(#xBF #xF0 #x00 #x00 0 0 0 0) -1)
      (test "0.5" #u8(#x3f #xE0 #x00 #x00 0 0 0 0) 0.5)
      (test "-0.5" #u8(#xBf #xE0 #x00 #x00 0 0 0 0) -0.5)
      (test "0" #u8(#x00 #x00 #x00 #x00 0 0 0 0) 0.0)
      (test "+inf.0" #u8(#x7F #xF0 #x00 #x00 0 0 0 0) +inf.0)
      (test "-inf.0" #u8(#xFF #xF0 #x00 #x00 0 0 0 0) -inf.0))
    (test-assert "NaN round tripping"
                 (let ((bv (make-bytevector 8)))
                   (bytevector-binary64-native-set! bv
                                                    0
                                                    +nan.0)
                   (nan? (bytevector-binary64-native-ref bv 0))))))

;;; Only test this when floats are binary32 or more precise
(define (test-binary64-native)
  (test-group "binary64-native-set! and binary64-native-ref are inverses"
    (test-property
     (lambda (x)
       (let ((bv (make-bytevector 8)))
         (bytevector-binary64-native-set! bv 0 x)
         (cond
           ((nan? x)
            (nan? (bytevector-binary64-native-ref bv 0)))
           (else
            (= x (bytevector-binary64-native-ref bv 0))))))
     (list (inexact-real-generator)))))

(define (test-binary64-subnormals)
  (test-group "binary64 subnormals"
    (letrec-syntax ((test
                     (syntax-rules ()
                       ((_ expected input)
                        (begin
                          (test-approximate
                           expected
                           (bytevector-binary64-ref input 0 'big)
                           0))))))
      (test 4.94065645841246544177e-324
            #u8(0 0 0 0 0 0 0 1))
      (test 9.88131291682493088353e-324
            #u8(0 0 0 0 0 0 0 2))
      (test 1.48219693752373963253e-323
            #u8(0 0 0 0 0 0 0 3))
      (test 2.22507385850720088902e-308
            #u8(0 #x0F #xFF #xFF #xFF #xFF #xFF #xFF)))))
