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
          bytevector->hex-string hex-string->bytevector
          bytevector->base64)

  (cond-expand
    (little-endian
      (begin (define-inline %native-endianness (lambda () 'little))))
    (big-endian
      (begin (define-inline %native-endianness (lambda () 'big)))))
  (begin
    (define-syntax unless-unsafe-and
      (syntax-rules ()
        ((_ control expr ...)
         (cond-expand
           (unsafe)
           (else (unless control expr ...))))))
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
    (define bytevector
      (let ((%bytevector
             (foreign-lambda* scheme-object ((scheme-object args)
                                             (nonnull-bytevector bv)
                                             (size_t len))
               "\
size_t i;
C_word tmp;

for (i = 0; i < len; i++) {
	tmp = C_u_i_car(args);
	if (!C_truep(C_fixnump(tmp))) {
		break;
	}

	*bv++ = (int8_t)C_unfix(tmp);
	args = C_u_i_cdr(args);
}

C_return(args);")))
      (lambda b8s
        (let* ((len (length b8s))
               (bv (r7rs:make-bytevector len))
               (offending (%bytevector b8s bv len)))
          (unless-unsafe-and (null? offending)
            (raise (condition `(exn message "invalid arguments"
                                    location bytevector
                                    arguments ,offending)
                              '(type))))
          bv))))

    (define (raise-type-error obj who description)
      (raise (condition `(exn location ,who
                              message ,(string-append
                                        "expected "
                                        description)
                              arguments (,obj))
                        '(type))))

    (define-inline check!
      (lambda (predicate obj who description)
        (unless-unsafe-and (predicate obj)
          (raise-type-error obj who description))))

    (: bytevector=? (#!rest bytevector --> boolean))
    (define bytevector=?
      (let ((%bytevector=?
             (foreign-lambda* scheme-object ((scheme-object lst)
                                             (scheme-object return_info))
               "\
C_word car;
C_word cadr;
if (C_truep(C_i_nullp(lst)))
	C_return(C_SCHEME_TRUE);

car = C_u_i_car(lst);
if (C_immediatep(car) || !C_truep(C_bytevectorp(car))) {
	C_u_i_car(return_info) = C_SCHEME_FALSE;
	C_return(car);
}

lst = C_u_i_cdr(lst);

while (!C_truep(C_i_nullp(lst))) {
	cadr = C_u_i_car(lst);
	if (C_immediatep(cadr)
	    || !C_truep(C_bytevectorp(cadr))) {
		C_u_i_car(return_info) = C_SCHEME_FALSE;
		C_return(cadr);
	} else if (C_u_i_bytevector_length(car)
	           == C_u_i_bytevector_length(cadr)
	           && memcmp(C_data_pointer(car),
	                     C_data_pointer(cadr),
	                     C_u_i_bytevector_length(car))) {
		car = cadr;
		lst = C_u_i_cdr(lst);
	} else {
		C_return(C_SCHEME_FALSE);
	}
}

C_return(C_SCHEME_TRUE);")))
        (lambda bvs
          (let* ((rp (cons #t #t))
                 (rv (%bytevector=? bvs rp)))
            (unless-unsafe-and (car rp)
              (raise-type-error rv 'bytevector=? "bytevector"))
            rv))))

    (define (raise-bounds-error who arguments)
      (raise (condition `(exn location ,who
                              message "invalid bounds"
                              arguments ,arguments))))

    (: bytevector-fill! (bytevector fixnum
                                    #!optional fixnum fixnum
                                    -> undefined))
    (define bytevector-fill!
      (let ((memset
             (foreign-lambda* void ((nonnull-bytevector bv)
                                    (unsigned-byte b8)
                                    (size_t start)
                                    (size_t end))
               "memset(bv+start, b8, end-start);")))
        (lambda (bv b8 #!optional (start 0) (end #f))
          (check! bytevector? bv 'bytevector-fill! "bytevector")
          (let ((len (##sys#size bv)))
            (unless-unsafe-and
                (and (fixnum? start) (or (not end) (fixnum? end))
                     (fx<= 0 start)
                     (if end
                         (and (fx<= start end)
                              (fx<= end len))
                         (fx<= start len))
                     (or (not end) (fx<= start end)))
              (raise-bounds-error 'bytevector-fill! (list bv start end)))
            (memset bv b8 start end)))))

    (define-syntax endianness
      (er-macro-transformer
       (lambda (exp rename compare)
         (let* ((sym (list-ref exp 1))
                (str (symbol->string sym)))
           (if (member str '("little" "big"))
               `(,(rename quote) ,sym)
               (error "unknown endianness" sym))))))

    (: bytevector->hex-string (bytevector #!optional fixnum fixnum
                               -> string))
    (define bytevector->hex-string
      (let ((%bytevector->hex-string
             (foreign-lambda* scheme-object ((nonnull-bytevector bv)
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
C_return(strobj);")))
      (lambda (bv #!optional (start 0) (end #f))
        (check! bytevector? bv 'bytevector->hex-string "bytevecotr")
        (let ((size (##sys#size bv)))
          (unless-unsafe-and
              (and (fixnum? start) (or (not end) (fixnum? end))
                   (fx<= 0 start)
                   (if end
                       (and (fx<= start end) (fx<= end size))
                       (fx<= start size))
            (raise-bounds-error 'bytevector->hex-string (list bv start end))))
          (%bytevector->hex-string bv
                                   start
                                   end
                                   (make-string (fx* (fx- end start) 2)))))))

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

    (: hex-string->bytevector (string #!optional fixnum fixnum
                               -> bytevector))
    (define hex-string->bytevector
      (let ((%hex-string->bytevector
             (foreign-lambda* scheme-object ((scheme-object strobj)
                                             (size_t start_char)
                                             (size_t number_of_chars)
                                             (nonnull-bytevector output))
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

C_return(C_SCHEME_FALSE);")))
      (lambda (str #!optional (start 0) (end (string-length str)))
        (unless-unsafe-and
            (and (fixnum? start) (fixnum? end)
                 (fx<= 0 start)
                 (fx<= start end)
                 (fx<= end (string-length str)))
          (raise-bounds-error 'hex-string->bytevector (list str start end)))
        (unless-unsafe-and (fx= 0 (fxrem (- end start) 2))
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
          bv))))

    (: bytevector->base64 (bytevector
                           #!optional
                           string fixnum fixnum
                           -> string))
    (define bytevector->base64
      (let ((%bytevector->base64
             (foreign-lambda* scheme-object ((nonnull-bytevector bv)
                                             (unsigned-byte c1)
                                             (unsigned-byte c2)
                                             (size_t start)
                                             (size_t end)
                                             (scheme-object strobj))
        "\
char *str = C_data_pointer(C_block_item(strobj, 0));
static const char *conv =
\"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789\";
size_t diff;
unsigned char tmp;
bv += start;

#define BV2B64(b) \\
  ((b) == 62 ? c1 : (b) == 63 ? c2 : conv[(b)])
for (;;) {
  diff = end - start;
  if (diff == 0) {
    break;
  }
  tmp = (unsigned char)*bv >> 2;
  *str++ = BV2B64(tmp);

  tmp = ((unsigned char)*bv & 0x3) << 4;
  if (diff == 1) {
    *str++ = BV2B64(tmp);
    break;
  }

  bv++;
  tmp |= ((unsigned char)*bv >> 4) & 0xF;
  *str++ = BV2B64(tmp);

  tmp = ((unsigned char)*bv & 0xF) << 2;
  if (diff == 2) {
    *str++ = BV2B64(tmp);
    break;
  }

  bv++;
  tmp |= ((unsigned char) *bv >> 6) & 0x3;
  *str++ = BV2B64(tmp);
  *str++ = BV2B64((unsigned char)*bv & 0x3F);
  bv++;

  start += 3;
}
#undef BV2B64
C_return(strobj);")))
      (lambda (bv #!optional (digits "+/") (start 0) (end #f))
        (check! bytevector? bv 'bytevector->base64 "bytevector")
        (unless-unsafe-and (and (string? digits) (fx= (string-length digits) 2))
          (raise-type-error digits 'bytevector->base64 "string with two elements"))
        (let* ((len (##sys#size bv))
               (end (or end len)))
          (unless-unsafe-and
              (and (fixnum? start) (fixnum? end)
                   (fx<= 0 start)
                   (fx<= start end)
                   (fx<= end len))
            (raise-bounds-error 'bytevector->base64 (list bv start end)))
          (let ((c1 (char->integer (string-ref digits 0)))
                (c2 (char->integer (string-ref digits 1))))
            (unless-unsafe-and (and (fx<= 0 c2) (fx<= c2 #x7F)
                                    (fx<= 0 c2) (fx<= c2 #x7F))
              (raise (condition `(exn message "digits must be ASCII"
                                      arguments (,digits)))))
            (let* ((complete-blocks (fx/ (fx- end start) 3))
                   (leftover (fxrem (fx- end start) 3))
                   (str (make-string (fx+ (fx* complete-blocks 4)
                                          (if (fx= leftover 0)
                                              0
                                              4))
                                      #\=)))
              (%bytevector->base64 bv c1 c2
                                   start end str)))))))

    (define-inline (valid-extra-base64-char? ch)
      (and (fx<= ch #x7F)
           (not (and (fx<= #x30 ch) (fx<= ch #x39)))
           (not (and (fx<= #x41 ch) (fx<= ch #x5A)))
           (not (and (fx<= #x61 ch) (fx<= ch #x7A)))))

    (: base64->bytevector (string
                           #!optional
                           fixnum fixnum string -> bytevector))
    (define base64->bytevector
      (let ((%base64->bytevector
             (foreign-lambda* scheme-object ((scheme-object str)
                                             (size_t start_char)
                                             (size_t end)
                                             (nonnull-bytevector bv)
                                             (unsigned-byte c62)
                                             (unsigned-byte c63))
        "\
size_t start = C_unfix(C_utf_position(str, C_fix(start_char)));
char *buf = C_data_pointer(C_block_item(str, 0)) + start;
int tmp, tmp2;

for (; start < end; start += 4, buf += 4, bv += 3) {
	tmp = base64_to_portion(buf[0], c62, c63);
	if (tmp < 0) {
		C_return(C_fix(start));
	}

	tmp2 = base64_to_portion(buf[1], c62, c63);
	if (tmp2 < 0) {
		C_return(C_fix(start));
	}
	bv[0] = (tmp << 2) | (tmp2 >> 4);

	if (buf[2] == '=') {
		if (buf[3] == '=' && start + 4 == end) {
			break;
		} else {
			C_return(C_fix(start));
		}
	}

	tmp = base64_to_portion(buf[2], c62, c63);
	if (tmp < 0) {
		C_return(C_fix(start));
	}

	bv[1] = ((tmp2 & 0xF) << 4) | (tmp >> 2);
	if (buf[3] == '=') {
		if (start + 4 == end) {
			break;
		} else {
			C_return(C_fix(start));
		}
	}

	tmp = base64_to_portion(buf[3], c62, c63);
	if (tmp < 0) {
		C_return(C_fix(start));
	}

	bv[2] = ((tmp2 & 3) << 2) | tmp;
}
C_return(C_SCHEME_FALSE);")))
        (lambda (str #!optional
                     (start 0)
                     (end #f)
                     (digits "+/"))
          (check! string? str 'base64->bytevector "string")
          (unless-unsafe-and (and (string? digits)
                                  (fx= (string-length digits) 2))
            (raise-type-error digits 'base64->bytevector "2-char string"))
          (let* ((len (string-length str))
                 (end (or end len)))
            (unless-unsafe-and (and (fixnum? start) (fixnum? end)
                                    (fx<= 0 start)
                                    (fx<= start end)
                                    (fx<= end len))
              (raise-bounds-error 'base64->bytevector
                                  (list str start end)))
            (let* ((c1 (char->integer (string-ref digits 0)))
                   (c2 (char->integer (string-ref digits 1))))
              (unless-unsafe-and (and (valid-extra-base64-char? c1)
                                      (valid-extra-base64-char? c2)
                                      (not (fx= c1 c2)))
                (raise (condition `(exn location base64->bytevector
                                        message "Invalid digits"
                                        arguments (,digits)))))
              (let ((groups (fx/ (fx- end start) 4))
                    (junk (fxrem (fx- end start) 4)))
                (unless-unsafe-and (fx= junk 0)
                  (raise (condition `(exn location base64->bytevector
                                          message "base64 string not padded"
                                          arguments (,str ,start ,end)))))
                (let* ((bv (make-bytevector (fx* groups 3)))
                       (bad-idx (%base64->bytevector str start end bv c1 c2)))
                  (when bad-idx
                    (raise (condition `(exn location base64->bytevector
                                            message "non-alphabetic character in base64"
                                            arguments (,str ,start ,end))
                                      `(deserialization string ,str
                                                        start ,bad-idx
                                                        end ,(fx+ bad-idx 4)))))
                  bv)))))))
))
