(define (test-u8-list-r6rs-examples)
  (test-group "r6rs examples for list procedures"
    (let ((b (u8-list->bytevector '(1 2 3 255 1 2 1 2))))
      (test-equal
       '(513 -253 513 513)
       (bytevector->sint-list b (endianness little) 2)))
    (let ((b (u8-list->bytevector '(1 2 3 255 1 2 1 2))))
      (test-equal
       '(513 65283 513 513)
       (bytevector->uint-list b (endianness little) 2)))))

(define (test-u8-s8-r6rs-example-1)
  (let ((b1 (make-bytevector 16 -127))
        (b2 (make-bytevector 16 255)))
    (test-group "r6rs examples for u8 and s8"
      (test-equal -127 (bytevector-s8-ref b1 0))
      (test-equal 129 (bytevector-u8-ref b1 0))
      (test-equal -1 (bytevector-s8-ref b2 0))
      (test-equal 255 (bytevector-u8-ref b2 0)))))

(define (test-u8-s8-r6rs-example-2)
  (test-group "r6rs examples for u8 and s8, 2"
    (let ((b (make-bytevector 16 -127)))
      (bytevector-s8-set! b 0 -126)
      (bytevector-u8-set! b 1 246)
      (test-equal -126 (bytevector-s8-ref b 0))
      (test-equal 130 (bytevector-u8-ref b 0))
      (test-equal -10 (bytevector-s8-ref b 1))
      (test-equal 246 (bytevector-u8-ref b 1)))))

(define (test-r6rs-uint-sint-examples)
  (test-group "uint-ref and sint-ref examples"
    (let ((b (make-bytevector 16 -127)))
      (bytevector-uint-set! b 0 (- (expt 2 128) 3)
                            (endianness little) 16)
      (test-equal #xfffffffffffffffffffffffffffffffd
                  (bytevector-uint-ref b 0 (endianness little) 16))
      (test-equal -3
                  (bytevector-sint-ref b 0 (endianness little) 16))
      (test-equal '(253 255 255 255 255 255 255 255
                    255 255 255 255 255 255 255 255)
                  (bytevector->u8-list b))
      (bytevector-uint-set! b 0 (- (expt 2 128) 3)
                            (endianness big) 16)
      (test-equal #xfffffffffffffffffffffffffffffffd
                  (bytevector-uint-ref b 0 (endianness big) 16))
      (test-equal -3
                  (bytevector-sint-ref b 0 (endianness big) 16))
      (test-equal '(255 255 255 255 255 255 255 255
                    255 255 255 255 255 255 255 253)
                  (bytevector->u8-list b)))))

(define (test-r6rs-u16-examples)
  (test-group "u16 r6rs examples"
    (let ((b (u8-list->bytevector
              '(255 255 255 255 255 255 255 255
                255 255 255 255 255 255 255 253))))
      (test-equal 65023 (bytevector-u16-ref b 14 (endianness little)))
      (test-equal -513 (bytevector-s16-ref b 14 (endianness little)))
      (test-equal 65533 (bytevector-u16-ref b 14 (endianness big)))
      (test-equal -3 (bytevector-s16-ref b 14 (endianness big)))
      (bytevector-u16-set! b 0 12345 (endianness little))
      (test-equal 12345 (bytevector-u16-ref b 0 (endianness little)))
      (bytevector-u16-native-set! b 0 12345)
      (test-equal 12345 (bytevector-u16-native-ref b 0)))))

(define (test-r6rs-u32-examples)
  (test-group "r6rs 32 bit examples"
    (let ((b (u8-list->bytevector
              '(255 255 255 255 255 255 255 255
                255 255 255 255 255 255 255 253))))
      (test-equal 4261412863 (bytevector-u32-ref b 12 (endianness little)))
      (test-equal -33554433 (bytevector-s32-ref b 12 (endianness little)))
      (test-equal 4294967293 (bytevector-u32-ref b 12 (endianness big)))
      (test-equal -3 (bytevector-s32-ref b 12 (endianness big))))))

(define (test-r6rs-u64-examples)
  (test-group "r6rs 64 bit examples"
    (let ((b (u8-list->bytevector
              '(255 255 255 255 255 255 255 255
                255 255 255 255 255 255 255 253))))
      (test-equal 18302628885633695743
                  (bytevector-u64-ref b 8 (endianness little)))
      (test-equal -144115188075855873
                  (bytevector-s64-ref b 8 (endianness little)))
      (test-equal 18446744073709551613 
                  (bytevector-u64-ref b 8 (endianness big)))
      (test-equal -3 (bytevector-s64-ref b 8 (endianness big))))))
