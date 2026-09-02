(define (bytevector-binary64-set! bv k x endianness)
  (unless (bytevector? bv)
    (error "not a bytevector bv"))
  (unless (exact-integer? k)
    (error "not an exact integer" k))
  (unless (and (not (negative? k))
               (< (+ k 7) (bytevector-length bv)))
    (error "invalid index" k))
  (unless (real? x)
    (error "not a real number" x))
  (unless (endianness? endianness)
    (error "not an endianness" endianness))
  (let ((x (inexact x)))
    (cond
      ((infinite? x)
       (infinite-set! bv (sign-negative? x) k endianness))
      ((nan? x)
       (nan-set! bv k x endianness))
      ((zero? x)
       (zero-set! bv (sign-negative? x) k endianness))
      (else
       (let*-values (((sign bits actual-exponent)
                      (convert-to-representation x
                                                 -1022
                                                 53))
                     ((exponent) (fx+ actual-exponent 1023)))
         (cond
           ((null? bits) (zero-set! bv sign k endianness))
           ((fx>? actual-exponent 1023)
            (infinite-set! bv sign k endianness))
           (else
            (finite-set! bv
                         sign
                         bits
                         exponent
                         k
                         endianness))))))))

(define (collect-byte list)
  (do ((i 0 (fx+ i 1))
       (byte 0 (fxior byte
                      (fxarithmetic-shift-left
                       (car list)
                       i)))
       (list list (cdr list)))
      ((or (fx=? i 8) (null? list))
       (values list byte))))

(define (bits->bytevector bits)
  (let*-values (((bits b1) (collect-byte bits))
                ((bits b2) (collect-byte bits))
                ((bits b3) (collect-byte bits))
                ((bits b4) (collect-byte bits))
                ((bits b5) (collect-byte bits))
                ((bits b6) (collect-byte bits))
                ((bits b7) (collect-byte bits)))
    (bytevector 0 b7 b6 b5 b4 b3 b2 b1)))

(define (finite-set! bv sign bits exponent k endianness)
  (let ((scratch (bits->bytevector bits)))
    (bytevector-u8-set! scratch
                        0
                        (fxior (fxarithmetic-shift-left
                                (if sign 1 0)
                                7)
                               (fxarithmetic-shift-right
                                exponent
                                4)))
    (bytevector-u8-set! scratch
                        1
                        (fxior (fxarithmetic-shift-left
                                (fxand exponent #xF)
                                4)
                               (bytevector-u8-ref scratch 1)))
    (case endianness
      ((big) (bytevector-copy! bv k scratch))
      ((little) (u8vector-reverse-copy! bv k scratch)))))

(define (zero-set! bv sign k endianness)
  (if (not sign)
      (bytevector-fill! bv 0 k (+ k 4))
      (case endianness
        ((big)
         (bytevector-copy! bv k #u8(#x80 0 0 0 0 0 0 0)))
        ((little)
         (bytevector-copy! bv k #u8(0 0 0 0 0 0 0 #x80))))))

(define (infinite-set! bv sign k endianness)
  (case endianness
    ((big)
     (if sign
         (bytevector-copy! bv k #u8(#xFF #xF0 0 0 0 0 0 0))
         (bytevector-copy! bv k #u8(#x7F #xF0 0 0 0 0 0 0))))
    ((little)
     (if sign
         (bytevector-copy! bv k #u8(0 0 0 0 0 0 #xF0 #xFF))
         (bytevector-copy! bv k #u8(0 0 0 0 0 0 #xF0 #x7F))))))

(cond-expand
  ((library (srfi 208))
   (define (nan-set! bv k x endianness)
     (let* ((sign? (nan-negative? x))
            (quiet? (nan-quiet? x))
            (payload (nan-payload x))
            (b1 (if sign? #xFF #x7F))
            (b2part (if quiet? #xF8 #xF0)))
       (bytevector-u64-set! bv k payload endianness)
       (case endianness
         ((big)
          (bytevector-u8-set! bv k b1)
          (bytevector-u8-set! bv (+ k 1)
                              (fxior b2part
                                     (bytevector-u8-ref
                                      bv (+ k 1)))))
         ((little)
          (bytevector-u8-set! bv (+ k 7) b1)
          (bytevector-u8-set! bv (+ k 6)
                              (fxior b2part
                                     (bytevector-u8-ref
                                      bv (+ k 6)))))))))
  (else
   (define (nan-set! bv k x endianness)
     (case endianness
       ((big) (bytevector-copy! bv k #u8(#x7F #xF8 0 0 0 0 0 0)))
       ((little) (bytevector-copy! bv k #u8(0 0 0 0 0 0 #xF8 #x7F)))))))

(define (normalize-to-big-endian bv k endianness)
  (case endianness
    ((big) (values (bytevector-u8-ref bv k)
                   (bytevector-u8-ref bv (+ k 1))
                   (bytevector-u8-ref bv (+ k 2))
                   (bytevector-u8-ref bv (+ k 3))
                   (bytevector-u8-ref bv (+ k 4))
                   (bytevector-u8-ref bv (+ k 5))
                   (bytevector-u8-ref bv (+ k 6))
                   (bytevector-u8-ref bv (+ k 7))))
    ((little) (values (bytevector-u8-ref bv (+ k 7))
                      (bytevector-u8-ref bv (+ k 6))
                      (bytevector-u8-ref bv (+ k 5))
                      (bytevector-u8-ref bv (+ k 4))
                      (bytevector-u8-ref bv (+ k 3))
                      (bytevector-u8-ref bv (+ k 2))
                      (bytevector-u8-ref bv (+ k 1))
                      (bytevector-u8-ref bv k)))))

(define (bytevector-binary64-ref bv k endianness)
  (unless (bytevector? bv)
    (error "not a bytevector" bv))
  (unless (exact-integer? k)
    (error "not an exact integer" k))
  (unless (and (not (negative? k))
               (< (+ k 7) (bytevector-length bv)))
    (error "not a valid index" k))
  (unless (endianness? endianness)
    (error "invalid endianness" endianness))
  (let*-values (((b1 b2 b3 b4 b5 b6 b7 b8)
                 (normalize-to-big-endian bv k endianness))
                ((sign) (fx=? (fxarithmetic-shift-right b1 7) 1))
                ((exponent) (fxior
                             (fxarithmetic-shift-left
                              (fxand b1 #x7F)
                              4)
                             (fxarithmetic-shift-right b2 4)))
                ((b2-sig) (fxand b2 #xF)))
    (cond
      ((fx=? exponent #x7FF)
       (infinity-or-nan sign b2-sig b3 b4 b5 b6 b7 b8))
      ((fxzero? exponent)
       (subnormal sign b2-sig b3 b4 b5 b6 b7 b8))
      (else
       (normal sign
               (fx- exponent 1023)
               b2-sig
               b3
               b4
               b5
               b6
               b7
               b8)))))

(define (infinity-or-nan sign? b2 b3 b4 b5 b6 b7 b8)
  (if (and (fxzero? b2) (fxzero? b3) (fxzero? b4) (fxzero? b5)
           (fxzero? b6) (fxzero? b7) (fxzero? b8))
      (case sign?
        ((#t) -inf.0)
        ((#f) +inf.0))
      (cond-expand
        ((library (srfi 208))
         (let ((quiet? (= (fxarithmetic-shift-right b2 6) 1))
               (payload (+ b7
                           (* b6 256)
                           (* b5 256 256)
                           (* b4 256 256 256)
                           (* b3 256 256 256 256)
                           (* b2 256 256 256 256))))
           (make-nan sign? quiet? payload)))
        (else +nan.0))))

(define (subnormal sign? b2 b3 b4 b5 b6 b7 b8)
  (if (and (fxzero? b2) (fxzero? b3) (fxzero? b4) (fxzero? b5)
           (fxzero? b6) (fxzero? b7) (fxzero? b8))
      (case sign?
        ((#t) -0.0)
        ((#f) +0.0))
      (* (if sign? -1 +1)
         (+ (bits->flonum b8 -1074)
            (bits->flonum b7 (fx+ -1074 8))
            (bits->flonum b6 (fx+ -1074 16))
            (bits->flonum b5 (fx+ -1074 24))
            (bits->flonum b4 (fx+ -1074 32))
            (bits->flonum b3 (fx+ -1074 40))
            (bits->flonum b2 (fx+ -1074 48))))))

(define (normal sign? exponent b2 b3 b4 b5 b6 b7 b8)
  (* (if sign? -1 +1)
     (+ (bits->flonum b8 (fx- exponent 52))
        (bits->flonum b7 (fx- exponent 44))
        (bits->flonum b6 (fx- exponent 36))
        (bits->flonum b5 (fx- exponent 28))
        (bits->flonum b4 (fx- exponent 20))
        (bits->flonum b3 (fx- exponent 12))
        (bits->flonum b2 (fx- exponent 4))
        (expt 2.0 exponent))))

(define (bytevector-binary64-native-ref bv k)
  (unless (and (exact-integer? k)
               (zero? (modulo k 8)))
    (error "index must be multiple of 8" k))
  (bytevector-binary64-ref bv k (native-endianness)))

(define (bytevector-binary64-native-set! bv k x)
  (unless (and (exact-integer? k)
               (zero? (modulo k 8)))
    (error "index must be multiple of 8" k))
  (bytevector-binary64-set! bv k x (native-endianness)))
