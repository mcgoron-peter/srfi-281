;;; CHANGE ME!
;;;
;;; If your implementation does not support bixnums, change this to a
;;; smaller number.
(define max-test-integer-width-in-bytes 16)
(define property-test-iterations
  (cond-expand
    (chibi 20)
    (gauche 30)
    (sagittarius 30)
    (else 100)))
(define-syntax test-property
  (syntax-rules ()
    ((_ prop gens)
     (%test-property prop gens property-test-iterations))))

(cond-expand
  ((or chibi tr7 sagittarius)
   (define-syntax test-group
     (syntax-rules ()
       ((_ name expr ...)
        (begin
          (display name)
          (newline)
          (%test-group name expr ...))))))
  (else
   (define-syntax test-group
     (syntax-rules ()
       ((_ name expr ...)
        (%test-group name expr ...))))))

;;; ;;;;;;;;;;
;;; Generators
;;; ;;;;;;;;;;

(define (make-random-unsigned-integer-generator size)
  (let ((gen-u8 (make-random-u8-generator)))
    (gcons*
     0
     (- (expt 256 size) 1)
     (lambda ()
       (do ((i 0 (+ (* i 256) (gen-u8)))
            (j 0 (+ j 1)))
           ((= j size) i))))))

(define (make-random-signed-integer-generator size)
  (let ((gen-us (make-random-unsigned-integer-generator size))
        (max (- (/ (expt 256 size) 2) 1)))
    (gcons*
     (- (/ (expt 256 size) 2))
     max
     (lambda ()
       (let ((v (gen-us)))
         (if (< max v)
             (- v (expt 256 size))
             v))))))

(define (bytevector-generator-of-length n)
  (lambda ()
    (do ((i 0 (+ i 1))
         (bv (make-bytevector n))
         (u8-generator (make-random-u8-generator)))
        ((= i n) bv)
      (bytevector-u8-set! bv i (u8-generator)))))

(define (make-range-generator generator get-length)
  (lambda ()
    (let* ((obj (generator))
           (len (get-length obj))
           (start ((make-random-integer-generator
                    0
                    (+ len 1))))
           (end ((make-random-integer-generator
                  start
                  (+ len 1)))))
        (vector obj start end))))

(define (make-codepoint-generator)
  (gmap integer->char
   (gremove (lambda (x)
              (<= #xD800 x #xDFFF))
            (make-random-integer-generator 0 #x110000))))

(define (make-unicode-string-generator)
  (gcons*
   ""
   (let ((char-gen (make-codepoint-generator))
         (len-gen (make-random-integer-generator 0 100)))
     (gmap
      (lambda (len)
        (generator->string char-gen len))
      len-gen))))

(define (string+range-generator)
  (make-range-generator (make-unicode-string-generator)
                        string-length))

(define (bytevector+range-generator)
  (make-range-generator (bytevector-generator)
                        bytevector-length))

(define (list+range-generator subgenerator)
  (make-range-generator (list-generator-of subgenerator)
                        length))

(define (endianness-generator)
  (gmap (lambda (boolean)
          (if boolean
              'little
              'big))
        (boolean-generator)))

(define (make-width-generator)
  (make-random-integer-generator
   1
   (+ max-test-integer-width-in-bytes)))

(define (make-unsigned-int-list+range-generator width)
  (list+range-generator (make-random-unsigned-integer-generator
                         width)))
(define (make-signed-int-list+range-generator width)
  (list+range-generator (make-random-signed-integer-generator
                         width)))

(define (test-inversion size bytevector-> ->bytevector)
  (lambda (list+range endianness)
    (let ((list (vector-ref list+range 0))
          (start (vector-ref list+range 1))
          (end (vector-ref list+range 2)))
      (let ((r1 (take (drop list start) (- end start)))
            (r2 (bytevector-> (->bytevector list
                                            endianness
                                            size
                                            start
                                            end)
                              endianness
                              size)))
        (equal? r1 r2)))))

(define (bytevector-reverse bv)
  (do ((new (make-bytevector (bytevector-length bv)))
       (i 0 (+ i 1)))
      ((= i (bytevector-length bv)) new)
    (bytevector-u8-set! new i (bytevector-u8-ref bv
                                                 (- (bytevector-length bv)
                                                    i
                                                    1)))))


(define skip-current-tests? #f)

