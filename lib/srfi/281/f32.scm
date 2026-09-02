(define (bytevector-binary32-set! bv k x endianness)
  (unless (bytevector? bv)
    (error "not a bytevector bv"))
  (unless (exact-integer? k)
    (error "not an exact integer" k))
  (unless (and (not (negative? k))
               (< (+ k 3) (bytevector-length bv)))
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
                                                 -126
                                                 24))
                     ((exponent) (fx+ actual-exponent 127)))
         (cond
           ((null? bits)
            (zero-set! bv sign k endianness))
           ((fx>? actual-exponent 127)
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
                ((bits b3) (collect-byte bits)))
    (bytevector 0 b3 b2 b1)))

(define (finite-set! bv sign bits exponent k endianness)
  (let ((scratch (bits->bytevector bits)))
    (bytevector-u8-set! scratch
                        0
                        (fxior (fxarithmetic-shift-left
                                (if sign 1 0)
                                7)
                               (fxarithmetic-shift-right
                                exponent
                                1)))
    (bytevector-u8-set! scratch
                        1
                        (fxior (fxarithmetic-shift-left
                                (fxand exponent 1)
                                7)
                               (bytevector-u8-ref scratch 1)))
    (case endianness
      ((big) (bytevector-copy! bv k scratch))
      ((little) (u8vector-reverse-copy! bv k scratch)))))

(define (zero-set! bv sign k endianness)
  (if (not sign)
      (bytevector-fill! bv 0 k (+ k 4))
      (case endianness
        ((big)
         (bytevector-copy! bv k #u8(#x80 0 0 0)))
        ((little)
         (bytevector-copy! bv k #u8(0 0 0 #x80))))))

(define (infinite-set! bv sign k endianness)
  (case endianness
    ((big)
     (if sign
         (bytevector-copy! bv k #u8(#xFF #x80 0 0))
         (bytevector-copy! bv k #u8(#x7F #x80 0 0))))
    ((little)
     (if sign
         (bytevector-copy! bv k #u8(0 0 #x80 #xFF))
         (bytevector-copy! bv k #u8(0 0 #x80 #x7F))))))

(cond-expand
  ((library (srfi 208))
   (define (nan-set! bv k x endianness)
     (let* ((sign? (nan-negative? x))
            (quiet? (nan-quiet? x))
            (payload (nan-payload x))
            (b1 (if sign? #xFF #x7F))
            (b2part (if quiet? #xC0 #x80)))
       (bytevector-u32-set! bv k payload endianness)
       (case endianness
         ((big)
          (bytevector-u8-set! bv k b1)
          (bytevector-u8-set! bv (+ k 1)
                              (fxior b2part
                                     (bytevector-u8-ref
                                      bv (+ k 1)))))
         ((little)
          (bytevector-u8-set! bv (+ k 3) b1)
          (bytevector-u8-set! bv (+ k 2)
                              (fxior b2part
                                     (bytevector-u8-ref
                                      bv (+ k 2)))))))))
  (else
   (define (nan-set! bv k x endianness)
     (case endianness
       ((big) (bytevector-copy! bv k #u8(#x7F #xC0 0 0)))
       ((little) (bytevector-copy! bv k #u8(0 0 #xC0 #x7F)))))))

(define (normalize-to-big-endian bv k endianness)
  (case endianness
    ((big) (values (bytevector-u8-ref bv k)
                   (bytevector-u8-ref bv (+ k 1))
                   (bytevector-u8-ref bv (+ k 2))
                   (bytevector-u8-ref bv (+ k 3))))
    ((little) (values (bytevector-u8-ref bv (+ k 3))
                      (bytevector-u8-ref bv (+ k 2))
                      (bytevector-u8-ref bv (+ k 1))
                      (bytevector-u8-ref bv k)))))

(define (bytevector-binary32-ref bv k endianness)
  (unless (bytevector? bv)
    (error "not a bytevector" bv))
  (unless (exact-integer? k)
    (error "not an exact integer" k))
  (unless (and (not (negative? k))
               (< (+ k 3) (bytevector-length bv)))
    (error "not a valid index" k))
  (unless (endianness? endianness)
    (error "invalid endianness" endianness))
  (let*-values (((b1 b2 b3 b4)
                 (normalize-to-big-endian bv k endianness))
                ((sign) (fx=? (fxarithmetic-shift-right b1 7) 1))
                ((exponent) (fxior
                             (fxarithmetic-shift-left
                              (fxand b1 #x7F)
                              1)
                             (fxarithmetic-shift-right b2 7)))
                ((b2-sig) (fxand b2 #x7F)))
    (cond
      ((fx=? exponent #xFF)
       (infinity-or-nan sign b2-sig b3 b4))
      ((fxzero? exponent)
       (subnormal sign b2-sig b3 b4))
      (else
       (normal sign
               (fx- exponent 127)
               b2-sig
               b3
               b4)))))

(define (infinity-or-nan sign? b2 b3 b4)
  (if (and (fxzero? b2) (fxzero? b3) (fxzero? b4))
      (case sign?
        ((#t) -inf.0)
        ((#f) +inf.0))
      (cond-expand
        ((library (srfi 208))
         (let ((quiet? (= (fxarithmetic-shift-right b2 6) 1))
               (payload (+ b4
                           (* b3 256)
                           (* (fxand b2 #x3F) 256))))
           (make-nan sign? quiet? payload)))
        (else +nan.0))))

(define (subnormal sign? b2 b3 b4)
  (if (and (fxzero? b2) (fxzero? b3) (fxzero? b4))
      (case sign?
        ((#t) -0.0)
        ((#f) +0.0))
      (* (if sign? -1 +1)
         (+ (bits->flonum b4 -149)
            (bits->flonum b3 (fx+ -149 8))
            (bits->flonum b2 (fx+ -149 16))))))

(define (normal sign? exponent b2 b3 b4)
  (* (if sign? -1 +1)
     (+ (bits->flonum b4 (fx- exponent 23))
        (bits->flonum b3 (fx- exponent 15))
        (bits->flonum b2 (fx- exponent 7))
        (expt 2.0 exponent))))

(define (bytevector-binary32-native-ref bv k)
  (unless (and (exact-integer? k)
               (zero? (modulo k 4)))
    (error "index must be multiple of 4" k))
  (bytevector-binary32-ref bv k (native-endianness)))

(define (bytevector-binary32-native-set! bv k x)
  (unless (and (exact-integer? k)
               (zero? (modulo k 4)))
    (error "index must be multiple of 4" k))
  (bytevector-binary32-set! bv k x (native-endianness)))
