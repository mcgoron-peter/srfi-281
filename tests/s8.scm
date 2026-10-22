(define (test-bytevector-s8-ref-negative)
  (test-group "bytevector-s8-ref for all negative integers"
    (do ((input 128 (+ input 1))
         (expected -128 (+ expected 1)))
        ((> input 255))
      (test-equal
       "bytevector-s8-ref"
       expected
       (bytevector-s8-ref (bytevector input) 0)))))

(define (test-bytevector-s8-ref-non-negative)
  (test-group "bytevector-s8-ref for all non-negative integers"
    (do ((input 0 (+ input 1)))
        ((> input 127))
      (test-equal
       "bytevector-s8-ref"
       input
       (bytevector-s8-ref (bytevector input) 0)))))

(define (test-bytevector-s8-set!-negative)
  (test-group "bytevector-s8-set! for all negative integers"
    (do ((input -128 (+ input 1)))
        ((not (negative? input)))
      (test-equal
       "bytevector-s8-set!"
       input
       (let ((bv (make-bytevector 1)))
         (bytevector-s8-set! bv 0 input)
         (bytevector-s8-ref bv 0))))))

(define (test-bytevector-s8-set!-non-negative)
  (test-group "bytevector-s8-set! for all non-negative integers"
    (do ((input 0 (+ input 1)))
        ((> input 127))
      (test-equal
       "bytevector-s8-set!"
       input
       (let ((bv (make-bytevector 1)))
         (bytevector-s8-set! bv 0 input)
         (bytevector-s8-ref bv 0))))))

(define (test-bytevector-s8-ref-and-bytevector-s8-set!-inverses)
  (test-group "bytevector-s8-ref and bytevector-s8-set! are inverses"
    (test-property
     (lambda (signed-byte)
       (let ((bv (make-bytevector 1)))
         (bytevector-s8-set! bv 0 signed-byte)
         (= (bytevector-s8-ref bv 0) signed-byte)))
     (list (make-random-signed-integer-generator 1)))))
