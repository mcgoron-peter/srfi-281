(define (bytevector-copy! source source-start target target-start k)
  (r7rs:bytevector-copy! target target-start source
                         source-start (+ source-start k)))

(define utf8->string
  (case-lambda
    ((bv) (utf8->string bv 0))
    ((bv start) (utf8->string bv start (bytevector-length bv)))
    ((bv start end)
     (r7rs:utf8->string bv start end 'replace))
    ((bv start end mode)
     (r7rs:utf8->string bv start end mode))))

(define utf16->string
  (case-lambda
    ((bv) (utf16->string bv 'big))
    ((bv endianness) (utf16->string endianness #t))
    ((bv endianness m?) (utf16->string endianness m? 0))
    ((bv endianness m? start)
     (utf16->string bv endianness m? start (bytevector-length bv)))
    ((bv endianness m? start end)
     (r7rs:utf16->string endianness m? bv start end 'replace))
    ((bv endianness m? start end mode)
     (r7rs:utf16->string endianness m? bv start end mode))))

(define utf32->string
  (case-lambda
    ((bv) (utf32->string bv 'big))
    ((bv endianness) (utf32->string endianness #t))
    ((bv endianness m?) (utf32->string endianness m? 0))
    ((bv endianness m? start)
     (utf32->string bv endianness m? start (bytevector-length bv)))
    ((bv endianness m? start end)
     (r7rs:utf32->string endianness m? bv start end 'replace))
    ((bv endianness m? start end mode)
     (r7rs:utf32->string endianness m? bv start end mode))))
