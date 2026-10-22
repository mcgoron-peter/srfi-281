(define (exhaustive-utf16-scalar)
  (test-group "exhaustive check for round-tripping of UTF-16 for scalar values"
    (for-each
     (lambda (endianness)
       (do ((i 0 (+ i 1)))
           ((>= i #xD800))
         (let* ((s (string (integer->char i)))
                (n (number->string i 16)))
           (test-assert n (equal? (utf16->string (string->utf16 s endianness)
                                                 endianness
                                                 #t)
                                  s))))
       (do ((i #xE000 (+ i 1)))
           ((> i #x10FFFF))
         (when (zero? (modulo i #x1000))
           (display (number->string i 16))
           (newline))
         (let ((s (string (integer->char i)))
               (n (number->string i 16)))
           (test-assert n (equal? (utf16->string (string->utf16 s
                                                                endianness)
                                                 endianness
                                                 #t)
                                  s)))))
     '(big little))))

(define (exhaustive-utf16-lone-surrogates)
  (test-group "UTF-16 behavior on lone surrogates"
    (do ((i #xD800 (+ i 1)))
        ((= i #xDFFF))
      (let ((bv (make-bytevector 2)))
        (bytevector-u16-set! bv 0 i 'big)
        (test-raises-unicode-error (utf16->string bv 'big))
        (test-equal "\xFFFD;"
                    (utf16->string bv 'big #f 0 2 'replace))
        (test-equal "" (utf16->string bv 'big #f 0 2 'ignore))
        (bytevector-u16-set! bv 0 i 'little)
        (test-raises-unicode-error (utf16->string bv 'little))
        (test-equal "\xFFFD;"
                    (utf16->string bv 'little #f 0 2 'replace))
        (test-equal ""
                    (utf16->string bv 'little #f 0 2 'ignore))))))

(define (string->utf16-and-utf16->string-inverses)
  (test-group "string->utf16 and utf16->string are inverses"
    (test-property
     (lambda (string+range endianness)
       (let* ((string (vector-ref string+range 0))
              (start (vector-ref string+range 1))
              (end (vector-ref string+range 2))
              (string-out
               (utf16->string (string->utf16 string
                                             endianness
                                             start
                                             end)
                              endianness
                              #t)))
         (equal? string-out (substring string start end))))
     (list (string+range-generator)
           (endianness-generator)))))

(define (test-utf16-bom)
  (test-group "utf16: BOM"
    (test-equal "\x20;"
                (utf16->string #u8(#xFE #xFF #x00 #x20) 'little))
    (test-equal "\x20;"
                (utf16->string #u8(#xFF #xFE #x20 #x00) 'big))
    (test-equal "\xFEFF;\x0020;" (utf16->string #u8(#xFE #xFF #x00 #x20)
                                                'big
                                                #t))
    (test-equal "\xFFFE;\x2000;" (utf16->string #u8(#xFE #xFF #x00 #x20)
                                                'little
                                                #t))))
