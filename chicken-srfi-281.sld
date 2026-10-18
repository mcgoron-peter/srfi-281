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
          bytevector=? bytevector-fill! endianness
          bytevector->hex-string hex-string->bytevector)
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

    (define deserialization-error?
      (let ((p1 (condition-predicate 'deserialization))
            (p2 (condition-predicate 'exn)))
        (lambda (x) (and (p1 x) (p2 x)))))
    (define deserialization-error-message
      (condition-property-accessor 'exn 'message))
    (define deserialization-error-string
      (condition-property-accessor 'deserialization 'string))
    (define deserialization-error-start
      (condition-property-accessor 'deserialization 'start))
    (define deserialization-error-end
      (condition-property-accessor 'deserialization 'end))

(foreign-declare "\
static inline int hex_char_to_half(char c)
{
	switch (c) {
	case '0': return 0;
	case '1': return 1;
	case '2': return 2;
	case '3': return 3;
	case '4': return 4;
	case '5': return 5;
	case '6': return 6;
	case '7': return 7;
	case '8': return 8;
	case '9': return 9;
	case 'a': case 'A': return 10;
	case 'b': case 'B': return 11;
	case 'c': case 'C': return 12;
	case 'd': case 'D': return 13;
	case 'e': case 'E': return 14;
	case 'f': case 'F': return 15;
	default: return -1;
	}
}

static inline int base64_to_portion(char c, char c62, char c63)
{
	if (c == c62)
		return 62;
	else if (c == c63)
		return 63;
	else switch (c) {
	case 'A': return 0; case 'B': return 1; case 'C': return 2;
	case 'D': return 3; case 'E': return 4; case 'F': return 5;
	case 'G': return 6; case 'H': return 7; case 'I': return 8;
	case 'J': return 9; case 'K': return 10; case 'L': return 11;
	case 'M': return 12; case 'N': return 13; case 'O': return 14;
	case 'P': return 15; case 'Q': return 16; case 'R': return 17;
	case 'S': return 18; case 'T': return 19; case 'U': return 20;
	case 'V': return 21; case 'W': return 22; case 'X': return 23;
	case 'Y': return 24; case 'Z': return 25; case 'a': return 26;
	case 'b': return 27; case 'c': return 28; case 'd': return 29;
	case 'e': return 30; case 'f': return 31; case 'g': return 32;
	case 'h': return 33; case 'i': return 34; case 'j': return 35;
	case 'k': return 36; case 'l': return 37; case 'm': return 38;
	case 'n': return 39; case 'o': return 40; case 'p': return 41;
	case 'q': return 42; case 'r': return 43; case 's': return 44;
	case 't': return 45; case 'u': return 46; case 'v': return 47;
	case 'w': return 48; case 'x': return 49; case 'y': return 50;
	case 'z': return 51; case '0': return 52; case '1': return 53;
	case '2': return 54; case '3': return 55; case '4': return 56;
	case '5': return 57; case '6': return 58; case '7': return 59;
	case '8': return 60; case '9': return 61;
	default: return -1;
	}
}
")

    (define %hex-string->bytevector
      (foreign-lambda* scheme-object ((scheme-object strobj)
                                      (size_t start_char)
                                      (size_t number_of_chars)
                                      (bytevector output))
        "\
char c1, c2;
int tmp;
size_t start = C_unfix(C_utf_position(strobj, C_fix(start_char)));
char *buf = C_data_pointer(C_block_item(strobj, 0)) + start;

for (start = 0; start < number_of_chars; start += 2, buf += 2, output++) {
  c1 = buf[0];
  c2 = buf[1];

  tmp = hex_char_to_half(c1);
  if (tmp < 0) {
    C_return(C_fix(start));
  }

  *output = (unsigned char) tmp;

  tmp = hex_char_to_half(c2);
  if (tmp < 0) {
    C_return(C_fix(start));
  }

  *output = (*output << 4) | (unsigned char) tmp;
}

C_return(C_SCHEME_FALSE);"))

    (: hex-string->bytevector (string #!optional fixnum fixnum
                               -> bytevector))
    (define (hex-string->bytevector str #!optional
                                    (start 0)
                                    (end (string-length str)))
      (unless (and (fixnum? start) (fixnum? end)
                   (<= 0 start end (string-length str)))
        (raise-bounds-error 'hex-string->bytevector (list str start end)))
      (unless (fx= 0 (fxrem (- end start) 2))
        (raise (condition `(exn location hex-string->bytevector
                                message "Invalid length for hex string segment"
                                arguments (,start ,end)))))
      (let* ((bv (make-bytevector (fx/ (fx- end start) 2)))
             (bad-index (%hex-string->bytevector str start (fx- end start) bv)))
        (when bad-index
          (raise (condition `(exn location hex-string->bytevector
                                  message "invalid character in hex string"
                                  arguments (,str ,bad-index))
                            `(deserialization string ,str
                                              start ,bad-index
                                              end ,(fx+ bad-index 1)))))
        bv))
))
