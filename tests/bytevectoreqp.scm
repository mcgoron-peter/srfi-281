(define (test-bytevector=?)
  (test-group "bytevector=?"
    (test-group "equal to self"
      (test-property (lambda (bv)
                       (bytevector=? bv bv))
                     (list (bytevector-generator))))
    (test-assert "truncated left" (not (bytevector=? #u8(1 2 3) #u8(1 2 3 4))))
    (test-assert "truncated right" (not (bytevector=? #u8(1 2 3 4) #u8(1 2 3))))
    (test-assert "multiple equal followed by unequal" (not (bytevector=? #u8() #u8() #u8(1))))))
