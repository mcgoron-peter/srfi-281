(define (test-s32-property)
  (test-group "s32"
    (test-property
     (lambda (endianness word len)
       (let* ((bv (make-bytevector (* len 4) 0))
              (idx ((make-random-integer-generator 0 len))))
         (bytevector-s32-set! bv idx word endianness)
         (= (bytevector-s32-ref bv idx endianness)
            word)))
     (list (endianness-generator)
           (make-random-signed-integer-generator 4)
           (make-random-integer-generator 1 100)))))
