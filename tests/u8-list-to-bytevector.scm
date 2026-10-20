(define (test-u8-list->bytevector-and-bytevector->u8-list)
  (test-group "u8-list->bytevector and bytevector->u8-list"
  (test-group "inverses 1"
    (test-property (lambda (list)
                     (equal? list (bytevector->u8-list
                                   (u8-list->bytevector list))))
                   (list (list-generator-of (make-random-u8-generator)))))
  (test-group "inverses 2"
    (test-property (lambda (bv)
                     (equal? bv (u8-list->bytevector
                                 (bytevector->u8-list bv))))
                   (list (bytevector-generator))))
  (test-group "inverses with range 1"
    (test-property (lambda (list+range)
                     (let ((list (vector-ref list+range 0))
                           (start (vector-ref list+range 1))
                           (end (vector-ref list+range 2)))
                       (equal? (take (drop list start) (- end start))
                               (bytevector->u8-list
                                (u8-list->bytevector list start end)))))
                   (list (list+range-generator (make-random-u8-generator)))))
  (test-group "inverses with range 2"
    (test-property (lambda (list+range)
                     (let ((list (vector-ref list+range 0))
                           (start (vector-ref list+range 1))
                           (end (vector-ref list+range 2)))
                       (equal? (take (drop list start) (- end start))
                               (bytevector->u8-list
                                (u8-list->bytevector list)
                                start
                                end))))
                   (list (list+range-generator (make-random-u8-generator)))))
    (test-group "inverses with range 3"
      (test-property (lambda (bv+range)
                       (let ((bv (vector-ref bv+range 0))
                             (start (vector-ref bv+range 1))
                             (end (vector-ref bv+range 2)))
                         (equal? (bytevector-copy bv start end)
                                 (u8-list->bytevector
                                  (bytevector->u8-list bv start end)))))
                     (list (bytevector+range-generator))))
    (test-group "inverses with range 4"
      (test-property (lambda (bv+range)
                       (let ((bv (vector-ref bv+range 0))
                             (start (vector-ref bv+range 1))
                             (end (vector-ref bv+range 2)))
                         (equal? (bytevector-copy bv start end)
                                 (u8-list->bytevector
                                  (bytevector->u8-list bv)
                                  start
                                  end))))
                     (list (bytevector+range-generator))))))

(define (test-u8-list-r6rs-examples)
  (test-group "r6rs examples for list procedures"
    (let ((b (u8-list->bytevector '(1 2 3 255 1 2 1 2))))
      (test-equal
       '(513 -253 513 513)
       (bytevector->sint-list b (endianness little) 2)))
    (let ((b (u8-list->bytevector '(1 2 3 255 1 2 1 2))))
      (test-equal
       '(513 65283 513 513)
       (bytevector->uint-list b (endianness little) 2)))))
