(define (s16:exhaustive-test endianness)
  (do ((i #x-8000 (+ i 1)))
      ((>= i #x8000))
    (test-equal
     "bytevector-s16-ref"
     i
     (let ((bv (make-bytevector 2)))
       (bytevector-s16-set! bv 0 i endianness)
       (bytevector-s16-ref bv 0 endianness)))))

(define (bytevector-s16-big-exhaustive)
  (test-group "exhaustive test for s16, big endian"
    (s16:exhaustive-test 'big)))
(define (bytevector-s16-little-exhaustive)
  (test-group "exhaustive test for s16, little endian"
    (s16:exhaustive-test 'little)))

(define (bytevector-s16-random)
  (test-group "u32 random test"
    (test-property
     (lambda (endianness word len)
       (let* ((bv (make-bytevector (* len 4) 0))
              (idx ((make-random-integer-generator 0 len))))
         (bytevector-u16-set! bv idx word endianness)
         (= (bytevector-u16-ref bv idx endianness)
            word)))
     (list (endianness-generator)
           (make-random-unsigned-integer-generator 2)
           (make-random-integer-generator 1 100)))))
