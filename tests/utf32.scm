(define (exhaustive-utf32)
  (test-group "exhaustive check for round-tripping of UTF-32 for scalar values"
      (for-each
       (lambda (endianness)
         (do ((i 0 (+ i 1)))
             ((>= i #xD800))
           (let* ((s (string (integer->char i)))
                  (n (number->string i 16)))
             (test-assert n (equal? (utf32->string (string->utf32 s endianness)
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
             (test-assert n (equal? (utf32->string (string->utf32 s
                                                                  endianness)
                                                   endianness
                                                   #t)
                                    s)))))
       '(big little))))

(define (string->utf32-and-utf32->string-inverses)
  (test-group "string->utf32 and utf32->string are inverses"
    (test-property
     (lambda (string+range endianness)
       (let ((string (vector-ref string+range 0))
             (start (vector-ref string+range 1))
             (end (vector-ref string+range 2)))
         (equal? (utf32->string (string->utf32 string
                                               endianness
                                               start
                                               end)
                                endianness
                                #t)
                 (substring string start end))))
     (list (string+range-generator)
           (endianness-generator)))))

(define (test-utf32-bom)
  (test-group "utf32: BOM"
    (test-equal "\x20;"
                (utf32->string #u8(#x00 #x00 #xFE #xFF #x00 #x00 #x00 #x20) 'little))
    (test-equal "\x20;"
                (utf32->string #u8(#xFF #xFE #x00 #x00 #x20 #x00 #x00 #x00) 'big))
    (test-equal "\xFEFF;\x0020;" (utf32->string #u8(#x00 #x00 #xFE #xFF #x00 #x00 #x00 #x20)
                                                'big
                                                #t))
    (test-equal "\xFFFE;\x0020;" (utf32->string #u8(#xFE #xFF #x00 #x00 #x20 #x00 #x00 #x00)
                                              'little
                                              #t))))

