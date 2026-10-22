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
