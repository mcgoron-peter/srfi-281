(define (test-s64-property)
  (test-group "s64"
    (test-property
     (lambda (endianness word len)
       (let* ((bv (make-bytevector (* len 8) 0))
              (idx ((make-random-integer-generator 0 len))))
         (bytevector-s64-set! bv idx word endianness)
         (= (bytevector-s64-ref bv idx endianness)
            word)))
     (list (endianness-generator)
           (make-random-signed-integer-generator 8)
           (make-random-integer-generator 1 100)))))
