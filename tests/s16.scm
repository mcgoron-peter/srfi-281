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
