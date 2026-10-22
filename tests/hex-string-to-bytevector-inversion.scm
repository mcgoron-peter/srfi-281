(define (test-hex-string->bytevector-and-bytevector->hex-string-1)
  (test-group "hex-string->bytevector and bytevector->hex-string are inverses, 1"
    (test-property
     (lambda (bv+range)
       (let ((bv (vector-ref bv+range 0))
             (start (vector-ref bv+range 1))
             (end (vector-ref bv+range 2)))
         (equal? (hex-string->bytevector (bytevector->hex-string
                                          bv
                                          start
                                          end))
                 (bytevector-copy bv start end))))
     (list (bytevector+range-generator)))))

(define (test-hex-string->bytevector-and-bytevector->hex-string-2)
  (test-group "hex-string->bytevector and bytevector->hex-string are inverses, 2"
    (test-property
     (lambda (bv+range)
       (let ((bv (vector-ref bv+range 0))
             (start (vector-ref bv+range 1))
             (end (vector-ref bv+range 2)))
         (equal? (hex-string->bytevector
                  (bytevector->hex-string bv)
                  (* start 2)
                  (* end 2))
                 (bytevector-copy bv start end))))
     (list (bytevector+range-generator)))))

