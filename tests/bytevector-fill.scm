(define (test-bytevector-fill-partially)
  (test-group "fill partially"
    (test-property (lambda (n data)
                     (let* ((bv (vector-ref data 0))
                            (start (vector-ref data 1))
                            (end (vector-ref data 2))
                            (cbv (bytevector-copy bv)))
                       (bytevector-fill! cbv n start end)
                       (and (equal? (bytevector-copy bv 0 start)
                                    (bytevector-copy cbv 0 start))
                            (equal? (bytevector-copy cbv end)
                                    (bytevector-copy cbv end))
                            (equal? (make-bytevector (- end start) n)
                                    (bytevector-copy cbv start end)))))
                   (list (make-random-integer-generator -128 256)
                         (bytevector+range-generator)))))

(define (test-bytevector-fill-exhaustive-negative)
  (test-group "exhaustive test of negative values"
    (do ((input -128 (+ input 1))
         (expected 128 (+ expected 1)))
        ((not (negative? input)))
      (test-equal "bytevector-fill!"
                  (bytevector expected expected expected)
                  (let ((bv (make-bytevector 3)))
                    (bytevector-fill! bv input)
                    bv)))))

(define (test-bytevector-fill-exhaustive-non-negative)
  (test-group "exhaustive test of non-negative values"
        (do ((input 0 (+ input 1)))
            ((> input 255))
          (test-equal "bytevector-fill!"
                      (make-bytevector 3 input)
                      (let ((bv (make-bytevector 3)))
                        (bytevector-fill! bv input)
                        bv)))))
