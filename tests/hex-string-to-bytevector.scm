(define (test-hex-string->bytevector)
  (test-group "hex-string->bytevector"
    (test-equal #u8(#xAA #xBB #xCC #xDD)
                (hex-string->bytevector "AaBBcCdd"))
    (let ((s "AAB!CCDD"))
      (guard (x (else (and (deserialization-error? x)
                           (eqv? (deserialization-error-message x) s))))
        (hex-string->bytevector s)))
    (test-equal #u8(#xBB #xCC #xDD)
                (hex-string->bytevector "AaBBcCdd"
                                        2))
    (test-equal #u8(#xBB #xCC)
                (hex-string->bytevector "AaBBcCdd"
                                        2
                                        6))))

