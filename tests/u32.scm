(define (test-u32-property)
  (test-group "u32"
    (test-property
     (lambda (endianness word len)
       (let* ((bv (make-bytevector (* len 4) 0))
              (idx ((make-random-integer-generator 0 len))))
         (bytevector-u32-set! bv idx word endianness)
         (= (bytevector-u32-ref bv idx endianness)
            word)))
     (list (endianness-generator)
           (make-random-unsigned-integer-generator 4)
           (make-random-integer-generator 1 100)))))
