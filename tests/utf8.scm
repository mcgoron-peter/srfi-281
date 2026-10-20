;;; This test is preferred, but may take very long.
(define (exhaustive-utf8)
  (test-group "exhaustive check for round-tripping of UTF-8 for scalar values"
    (do ((i 0 (+ i 1)))
        ((>= i #xD800))
      (let* ((s (string (integer->char i)))
             (n (number->string i 16)))
        (test-assert n (equal? (utf8->string (string->utf8 s))
                               s))))
    (do ((i #xE000 (+ i 1)))
        ((> i #x10FFFF))
      (let ((s (string (integer->char i)))
            (n (number->string i 16)))
        (test-assert n (equal? (utf8->string (string->utf8 s))
                               s))))))

(define (test-utf8-extreme)
  (test-group "extreme values for UTF-8 values"
    (let-syntax ((test2
                  (syntax-rules ()
                    ((_ bv s)
                     (begin
                       (test-equal bv (string->utf8 s))
                       (test-equal s (utf8->string bv)))))))
      (test2 #u8(0) "\x0;")
      (test2 #u8(#x7F) "\x7F;")
      (test2 #u8(#xC2 #x80) "\x80;")
      (test2 #u8(#xDF #xBF) "\x7FF;")
      (test2 #u8(#xE0 #xA0 #x80) "\x800;")
      (test2 #u8(#xED #x9F #xBF) "\xD7FF;")
      (test2 #u8(#xEE #x80 #x80) "\xE000;")
      (test2 #u8(#xEF #xBF #xBF) "\xFFFF;")
      (test2 #u8(#xF0 #x90 #x80 #x80) "\x10000;")
      (test2 #u8(#xF4 #x8F #xBF #xBF) "\x10FFFF;"))))

(define (test-utf8-truncation)
  (test-group "Truncation examples"
    (let-syntax ((test
                  (syntax-rules ()
                    ((_ expected input)
                     (let ((b input))
                       (test-equal expected
                                   (utf8->string
                                    b
                                    0
                                    (bytevector-length b)
                                    'replace)))))))
      (test "\xFFFD;" #u8(#xE0 #xA0))
      (test "\xFFFD;\xFFFD;" #u8(#xC0 #x80))
      (test "\xFFFD;\xFFFD;" #u8(#xC1 #xBF))
      (test "\xFFFD;\xFFFD;" #u8(#xFE #xFF))
      (test "\xFFFD;\x20;" #u8(#xF0 #x90 #x80 #x20)))))

(define (test-string->utf8-and-utf8->string-inverses)
  (test-group "string->utf8 and utf8->string are inverses"
    (test-property
     (lambda (string+range)
       (let ((string (vector-ref string+range 0))
             (start (vector-ref string+range 1))
             (end (vector-ref string+range 2)))
         (let ((rev (utf8->string (string->utf8 string
                                                start
                                                end)))
               (sub (substring string start end)))
           (equal? rev sub))))
     (list (string+range-generator)))))

(define (exhaustive-two-byte-overlong)
  (test-group "exhaustive check for two-byte overlong encodings"
    (do ((i #xC0 (+ i 1)))
        ((= i #xC2))
      (test-raises-unicode-error
       (utf8->string (bytevector i)))
      (test-equal (string (integer->char #xFFFD))
                  (utf8->string (bytevector i)
                                0
                                1
                                'replace))
      (test-equal ""
                  (utf8->string (bytevector i)
                                0
                                1
                                'ignore))
      (do ((j #x80 (+ j 1)))
          ((> j #xBF))
        (test-raises-unicode-error
         (utf8->string (bytevector i j)))
        (test-equal (string (integer->char #xFFFD)
                            (integer->char #xFFFD))
                    (utf8->string (bytevector i j)
                                  0
                                  2
                                  'replace))
        (test-equal ""
                    (utf8->string (bytevector i j) 0 2 'ignore))))))

(define (exhaustive-three-byte-overlong)
  (test-group "exhaustive check for three byte overlong encoding"
    (do ((i #x80 (+ i 1)))
        ((= i #xA0))
      (test-raises-unicode-error
       (utf8->string (bytevector #xE0 i)))
      (test-equal (string (integer->char #xFFFD)
                          (integer->char #xFFFD))
                  (utf8->string (bytevector #xE0 i)
                                0
                                2
                                'replace))
      (test-equal ""
                  (utf8->string (bytevector #xE0 i)
                                0
                                1
                                'ignore)))))

(define (exhaustive-three-byte-truncated)
  (test-group "check for three byte truncated encoding"
    (do ((i #xA0 (+ i 1)))
        ((> i #xBF))
      (test-raises-unicode-error
       (utf8->string (bytevector #xE0 i)))
      (test-equal (string (integer->char #xFFFD))
                  (utf8->string (bytevector #xE0 i)
                                0
                                2
                                'replace))
      (test-equal ""
                  (utf8->string (bytevector #xE0 i)
                                0
                                1
                                'ignore)))))

(define (exhaustive-utf8-surrogate)
  (test-group "exhaustive check of surrogate encodings"
    (do ((i #xA0 (+ i 1)))
        ((> i #xBF))
      (do ((j #x80 (+ j 1)))
          ((> j #xBF))
        (test-raises-unicode-error
         (utf8->string (bytevector #xED i j)))
        (test-equal (string (integer->char #xFFFD)
                            (integer->char #xFFFD)
                            (integer->char #xFFFD))
                    (utf8->string (bytevector #xED i j)
                                  0
                                  3
                                  'replace))
        (test-equal ""
                    (utf8->string (bytevector #xED i j)
                                  0
                                  3
                                  'ignore))))))

(define (test-above-U+10FFFF)
  (test-group "encoding above U+10FFFF"
    (test-raises-unicode-error
     (utf8->string #u8(#xF4 #x90 #x80 #x80)))
    (test-equal (string (integer->char #xFFFD)
                        (integer->char #xFFFD)
                        (integer->char #xFFFD)
                        (integer->char #xFFFD))
                (utf8->string #u8(#xF4 #x90 #x80 #x80)
                              0
                              4
                              'replace))
    (test-equal ""
                (utf8->string #u8(#xF4 #x90 #x80 #x80)
                              0
                              4
                              'ignore))))
