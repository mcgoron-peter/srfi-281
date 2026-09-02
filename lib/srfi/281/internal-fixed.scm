(begin
  (define-syntax branch
    (syntax-rules ()
      ((_ #t x y) x)
      ((_ #f x y) y)
      ((_ not-a-boolean x y)
       (syntax-error "must be boolean" not-a-boolean))))
  (define-syntax multibyte-ref
    (syntax-rules ()
      ((_ size fixnum?)
       (lambda (bv k e)
         (letrec-syntax ((%+
                          (syntax-rules ()
                            ((_) (branch fixnum? fx+ +))))
                         (%*
                          (syntax-rules ()
                            ((_) (branch fixnum? fx* *))))
                         (%<
                          (syntax-rules ()
                            ((_) (branch fixnum? fx<? <)))))
           (unless (bytevector? bv)
             (error "not a bytevector" bv))
           (unless (exact-integer? k)
             (error "not an exact integer" k))
           (unless (endianness? e)
             (error "not an endianness" e))
           (unless (<= 0 (+ k size) (bytevector-length bv))
             (error "not a valid index" k size (bytevector-length bv)))
           (case e
             ((big)
              (do ((acc 0 ((%+) ((%*) 256 acc)
                                (bytevector-u8-ref bv i)))
                   (i k (+ i 1)))
                  ((= i (+ k size)) acc)))
             ((little)
              (do ((acc 0 ((%+) ((%*) 256 acc)
                                (bytevector-u8-ref bv i)))
                   (i (+ k size -1) (- i 1)))
                  ((< i k) acc)))))))))
  (define-syntax multibyte-set
    (syntax-rules ()
      ((_ size lo hi fixnum?)
       (lambda (bv k n e)
         (letrec-syntax ((floor/256
                          (syntax-rules ()
                            ((_) (branch fixnum?
                                         fix:floor/256
                                         big:floor/256)))))
           (unless (bytevector? bv)
             (error "not a bytevector" bv))
           (unless (exact-integer? k)
             (error "not an exact integer" k))
           (unless (endianness? e)
             (error "not an endianness" e))
           (unless (<= 0 (+ k size) (bytevector-length bv))
             (error "not a valid index" k size (bytevector-length bv)))
           (unless (<= lo n hi)
             (error "not a valid sized integer" lo n hi))
           (case e
             ((little)
              (let loop ((n n) (i k))
                (unless (= i (+ k size))
                  (let-values (((remainder byte) ((floor/256) n)))
                    (bytevector-u8-set! bv i byte)
                    (loop remainder (+ i 1))))))
             ((big)
              (let loop ((n n) (i (+ k size -1)))
                (unless (< i k)
                  (let-values (((remainder byte) ((floor/256) n)))
                    (bytevector-u8-set! bv i byte)
                    (loop remainder (- i 1))))))))))))
  (define-syntax define-unsigned-for-fixed-width
    (syntax-rules ()
      ((_ size uint-ref uint-set! uint-native-ref uint-native-set!)
       (define-values (uint-ref uint-set! uint-native-ref uint-native-set!)
         (letrec* ((fits-in-fixnum? (< size fx-width))
                   (lo 0)
                   (hi (- (expt 256 size) 1))
                   (uint-ref
                    (if fits-in-fixnum?
                        (multibyte-ref size #t)
                        (multibyte-ref size #f)))
                   (uint-set!
                    (if fits-in-fixnum?
                        (multibyte-set size lo hi #t)
                        (multibyte-set size lo hi #f)))
                   (uint-native-ref
                    (lambda (bv k)
                      (unless (zero? (floor-remainder k size))
                        (error "not a multiple" k size))
                      (uint-ref bv k (native-endianness))))
                   (uint-native-set!
                    (lambda (bv k n)
                      (unless (zero? (floor-remainder k size))
                        (error "not a multiple" k size))
                      (uint-set! bv k n (native-endianness)))))
           (values uint-ref uint-set!
                   uint-native-ref uint-native-set!))))))
  (define-syntax define-signed-for-fixed-width
    (syntax-rules ()
      ((_ size
          int-ref
          int-set!
          int-native-ref
          int-native-set!)
       (define-values (int-ref
                       int-set!
                       int-native-ref
                       int-native-set!)
         (let ()
           (define-unsigned-for-fixed-width size
             uint-ref uint-set!
             uint-native-ref uint-native-set!)
           (letrec ((signed->unsigned
                     (signed->unsigned-factory size))
                    (unsigned->signed
                     (unsigned->signed-factory size))
                    (int-ref
                     (lambda (bv k endianness)
                       (unsigned->signed
                        (uint-ref bv k endianness))))
                    (int-set!
                     (lambda (bv k n endianness)
                       (uint-set! bv k (signed->unsigned n) endianness)))
                    (int-native-ref
                     (lambda (bv k)
                       (unless (zero? (floor-remainder k size))
                         (error "not a multiple" k size))
                       (int-ref bv k (native-endianness))))
                    (int-native-set!
                     (lambda (bv k n)
                       (unless (zero? (floor-remainder k size))
                         (error "not a multiple" k size))
                       (int-set! bv k n (native-endianness)))))
             (values int-ref int-set!
                     int-native-ref int-native-set!))))))))