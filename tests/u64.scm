(define (test-u64-property)
  (test-group "u64"
    (test-property
     (lambda (endianness word len)
       (let* ((bv (make-bytevector (* len 8) 0))
              (idx ((make-random-integer-generator 0 len))))
         (bytevector-u64-set! bv idx word endianness)
         (= (bytevector-u64-ref bv idx endianness)
            word)))
     (list (endianness-generator)
           (make-random-unsigned-integer-generator 8)
           (make-random-integer-generator 1 100)))))
