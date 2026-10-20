(define (test-make-bytevector-simple)
  (test-group "make-bytevector"
    (test-equal "empty" #u8() (make-bytevector 0))
    (test-group "length"
      (test-property (lambda (n)
                       (= n (bytevector-length (make-bytevector n))))
                     (list (make-random-unsigned-integer-generator 2))))))

(define (test-make-bytevector-negative-numbers)
  (test-group "make-bytevector exhausive check for negative numbers"
      (do ((input -128 (+ input 1))
           (expected 128 (+ expected 1)))
          ((not (negative? input)))
        (test-equal (bytevector expected)
                    (make-bytevector 1 input)))))

(define (test-make-bytevector-non-negative-numbers)
  (test-group "make-bytevector exhausive check for non-negative numbers"
      (do ((input 0 (+ input 1)))
          ((> input 255))
        (test-equal (bytevector input)
                    (make-bytevector 1 input)))))
