(define (u16:exhaustive-test endianness)
  (do ((i 0 (+ i 1)))
      ((> i #xFFFF))
    (test-equal
     "bytevector-u16-ref"
     i
     (let ((bv (make-bytevector 2)))
       (bytevector-u16-set! bv 0 i endianness)
       (bytevector-u16-ref bv 0 endianness)))))

(define (bytevector-u16-big-exhaustive)
  (test-group "exhaustive test for u16, big endian"
    (u16:exhaustive-test 'big)))

(define (bytevector-u16-little-exhaustive)
  (test-group "exhaustive test for u16, little endian"
    (u16:exhaustive-test 'little)))

