(define-library (srfi 281)
  (import (rename (scheme base)
                  (make-bytevector r7rs:make-bytevector)
                  (bytevector r7rs:bytevector))
          (chicken base)
          (chicken type)
          (chicken fixnum))
  (export endianness? native-endianness make-bytevector bytevector)
  (cond-expand
    (little-endian
      (begin (define-inline %native-endianness (lambda () 'little))))
    (big-endian
      (begin (define-inline %native-endianness (lambda () 'big)))))
  (begin
    (define-inline %endianness?
      (lambda (obj)
        (pair? (memq obj '(little big)))))
    (: endianness? (* --> boolean))
    (define endianness? %endianness?)

    (: native-endianness (--> symbol))
    (define native-endianness %native-endianness)

    (define-inline b8->u8
      (lambda (b8)
        (if (fx< b8 0)
            (fxand b8 #xFF)
            b8)))
    (: make-bytevector (fixnum #!optional (or fixnum false) -> bytevector))
    ;; TODO: define-specialization?
    (define (make-bytevector k #!optional (b8 #f))
      (cond
        ((not b8) (r7rs:make-bytevector k))
        (else (r7rs:make-bytevector k (b8->u8 b8)))))

    (: bytevector (#!rest fixnum -> bytevector))
    (define (bytevector . b8s)
      (do ((bv (make-bytevector (length b8s)))
           (i 0 (fx+ i 1))
           (b8s b8s (cdr b8s)))
          ((null? b8s) bv)
        (bytevector-u8-set! bv i (b8->u8 (car b8s)))))
))
