(define (test-base64->bytevector)
  (test-group "base64->bytevector"
    (test-equal #u8() (base64->bytevector ""))
    (test-equal #u8(102) (base64->bytevector "Zg=="))
    (test-equal #u8(102 111) (base64->bytevector "Zm8="))
    (test-equal #u8(102 111 111) (base64->bytevector "Zm9v"))
    (test-assert "error when reading non-alphabet character"
      (let ((s "Zm9!v"))
        (guard (x (else (and (deserialization-error? x)
                             (eqv? (deserialization-error-string x) s))))
          (base64->bytevector s))))
    (test-equal #u8(102 111 111 98) (base64->bytevector "Zm9vYg=="))
    (test-equal #u8(102 111 111 98 97) (base64->bytevector "Zm9vYmE="))
    (test-equal #u8(102 111 111 98 97 114) (base64->bytevector "Zm9vYmFy"))
    (let ((s "    Z m 9 v\n Y m F y    "))
      (guard (x (else (and (deserialization-error? x)
                           (eqv? (deserialization-error-string x) s))))
        (base64->bytevector s)))
    (test-equal #u8(#b11111011 #b11111111 #b10111110)
                (base64->bytevector "+/++"))
    (test-equal #u8(#b11111011 #b11111111 #b10111110)
                (base64->bytevector "-_--" "-_"))))

(define (test-base64->bytevector-start-end)
  (test-group "base64->bytevector with start and end"
    (test-property
     (lambda (bv s1 s2)
       (let* ((base64 (bytevector->base64 bv))
              (padded (string-append s1
                                     base64
                                     s2)))
         (equal? (base64->bytevector padded
                                     #f
                                     (string-length s1)
                                     (- (string-length padded)
                                        (string-length s2)))
                 bv)))
     (list (bytevector-generator)
           (string-generator)
           (string-generator)))))

