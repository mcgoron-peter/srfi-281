(define u8->s8
  (unsigned->signed-factory 1))

(define s8->u8
  (signed->unsigned-factory 1))

(define (bytevector-s8-ref bv k)
  (unless (bytevector? bv)
    (error "not a bytevector" bv))
  (unless (and (exact-integer? k)
               (<= 0 k)
               (< k (bytevector-length bv)))
    (error "not a valid index" k bv))
  (u8->s8 (bytevector-u8-ref bv k)))

(define (bytevector-s8-set! bv k n)
  (unless (bytevector? bv)
    (error "not a bytevector" bv))
  (unless (and (exact-integer? k)
               (<= 0 k)
               (< k (bytevector-length bv)))
    (error "not a valid index" k bv))
  (unless (and (exact-integer? n)
               (<= -128 n 127))
    (error "not an exact integer in [-128,127]" n))
  (bytevector-u8-set! bv k (s8->u8 n)))
