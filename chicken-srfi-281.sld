(define-library (srfi 281)
  (import (rename (scheme base)
                  (make-bytevector r7rs:make-bytevector)
                  (bytevector r7rs:bytevector)
                  (define r7rs:define))
          (chicken base)
          (chicken type)
          (chicken condition)
          (chicken foreign)
          (chicken fixnum))
  (export endianness? native-endianness make-bytevector bytevector
          bytevector=? bytevector-fill! endianness bytevector->hex-string)
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

    (define binary-bytevector=?
      (foreign-lambda* bool ((scheme-object bv1)
                             (scheme-object bv2))
        "\
C_return(C_u_i_bytevector_length(bv1) == C_u_i_bytevector_length(bv2) &&
         memcmp(C_data_pointer(bv1), C_data_pointer(bv2),
                C_header_size(bv1)) == 0);"))

    (define (check! predicate obj who description)
      (unless (predicate obj)
        (raise (condition `(exn location ,who
                                message ,(string-append
                                          "expected "
                                          description)
                                arguments (,obj))
                          '(type)))))

    (: bytevector=? (#!rest bytevector --> boolean))
    (define (bytevector=? . bvs)
      (cond
        ((null? bvs) #t)
        ((null? (cdr bvs))
         (check! bytevector? (car bvs) 'bytevector=? "bytevector"))
        ((null? (cddr bvs))
         (let ((b1 (car bvs))
               (b2 (cadr bvs)))
          (check! bytevector? b1 'bytevector=? "bytevector")
          (check! bytevector? b2 'bytevector=? "bytevector")
          (binary-bytevector=? b1 b2)))
        (else
          (check! bytevector? (car bvs) 'bytevector=? "bytevector")
          (let loop ((x (car bvs)) (y (cadr bvs)) (rest (cddr bvs)))
            (check! bytevector? y 'bytevector=? "bytevector")
            (and (binary-bytevector=? x y)
                 (or (null? rest)
                     (loop y (car rest) (cdr rest))))))))

    (define memset
      (foreign-lambda* void ((bytevector bv)
                             (unsigned-byte b8)
                             (size_t start)
                             (size_t end))
        "memset(bv+start, b8, end-start);"))

    (define (raise-bounds-error who arguments)
      (raise (condition `(exn location ,who
                              message "invalid bounds"
                              arguments ,arguments))))

    (: bytevector-fill! (bytevector fixnum
                                    #!optional fixnum fixnum
                                    ->))
    (define (bytevector-fill! bv b8 #!optional (start 0)
                                               (end (bytevector-length bv)))
      (unless (and (fixnum? start) (fixnum? end)
                   (<= 0 start end (bytevector-length bv)))
        (raise-bounds-error 'bytevector-fill! (list bv start end)))
      (memset bv b8 start end))

    (define-syntax endianness
      (er-macro-transformer
       (lambda (exp rename compare)
         (let* ((sym (list-ref exp 1))
                (str (symbol->string sym)))
           (if (member str '("little" "big"))
               `(,(rename quote) ,sym)
               (error "unknown endianness" sym))))))

    (define %bytevector->hex-string
      (foreign-lambda* scheme-object ((bytevector bv)
                                      (size_t start)
                                      (size_t end)
                                      (scheme-object strobj))
        "\
char *str = C_data_pointer(C_block_item(strobj, 0));
bv += start;
for (; start < end; start++) {
  *str++ = \"0123456789ABCDEF\"[(*bv >> 4) & 0xF];
  *str++ = \"0123456789ABCDEF\"[*bv++ & 0xF];
};
C_return(strobj);"))

    (: bytevector->hex-string (bytevector #!optional fixnum fixnum
                               -> string))
    (define (bytevector->hex-string bv #!optional
                                       (start 0)
                                       (end (bytevector-length bv)))
      (unless (and (fixnum? start) (fixnum? end)
                   (<= 0 start end (bytevector-length bv)))
        (raise-bounds-error 'bytevector->hex-string (list bv start end)))
      (%bytevector->hex-string bv
                               start
                               end
                               (make-string (fx* (fx- end start) 2))))
))
