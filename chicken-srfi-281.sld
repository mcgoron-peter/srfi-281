(define-library (srfi 281)
  (import (rename (scheme base)
                  (make-bytevector r7rs:make-bytevector)
                  (bytevector r7rs:bytevector)
                  (define r7rs:define))
          (chicken base)
          (chicken type)
          (chicken condition)
          (chicken foreign)
          (chicken fixnum)
          (chicken bitwise)
          (srfi 1))
  (export endianness? native-endianness make-bytevector bytevector
          bytevector-length bytevector-u8-ref bytevector-u8-set!
          bytevector=? bytevector-fill! endianness
          bytevector->hex-string hex-string->bytevector
          deserialization-error?
          deserialization-error-message
          deserialization-error-string
          deserialization-error-start
          deserialization-error-end
          u8-list->bytevector bytevector->u8-list
          bytevector-s8-ref bytevector-s8-set!
          bytevector-u16-ref bytevector-u16-native-ref
          bytevector-u16-set! bytevector-u16-native-set!
          bytevector-sint-set! bytevector-sint-ref
          bytevector-uint-set! bytevector-uint-ref
          bytevector->uint-list uint-list->bytevector
          bytevector->sint-list sint-list->bytevector)

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

    (define (raise-bounds-error who arguments)
      (raise (condition `(exn location ,who
                              message "invalid bounds"
                              arguments ,arguments))))

    (define (raise-index-error who bv i)
      (raise (condition `(exn location ,who
                              message "invalid index"
                              arguments (,bv ,i)))))

    (define (raise-unaligned-error who i alignment)
      (raise (condition `(exn location ,who
                              message "unaligned index"
                              arguments (,i 4)))))


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
	} else if (C_header_size(car) == C_header_size(cadr)
	           && memcmp(C_data_pointer(car),
	                     C_data_pointer(cadr),
	                     C_header_size(car)) == 0) {
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

    (: bytevector-fill! (bytevector fixnum
                                    #!optional fixnum fixnum
                                    -> undefined))
    (define bytevector-fill!
      (let ((%bytevector-fill
             (foreign-lambda* short ((scheme-object bv)
                                     (scheme-object b8)
                                     (scheme-object start)
                                     (scheme-object end))
               "\
unsigned char byte;
size_t len;
if (C_immediatep(bv) || !C_truep(C_bytevectorp(bv))) {
	C_return(1);
}

len = C_u_i_bytevector_length(bv);

if (!C_truep(C_fixnump(b8))) {
	C_return(2);
}

if (C_unfix(b8) < -128 || C_unfix(b8) > 255) {
	C_return(3);
}

byte = (unsigned char)C_unfix(b8);
if (!C_truep(C_fixnump(start))) {
	C_return(4);
}

if (!C_truep(end)) {
	end = C_fix(len);
} else if (!C_truep(C_fixnump(end))) {
	C_return(5);
}

if (C_unfix(start) < 0 || C_unfix(start) > C_unfix(end)
    || C_unfix(end) > len) {
	C_return(6);
}

memset(C_c_string(bv) + C_unfix(start),
       byte,
       C_unfix(end) - C_unfix(start));
C_return(0);")))
        (lambda (bv b8 #!optional (start 0) (end #f))
          (let ((return-code (%bytevector-fill bv b8 start end)))
            (cond-expand
              (unsafe return-code)
              (else
                (case return-code
                  ((0) #t)
                  ((1) (raise-type-error bv 'bytevector-fill! "bytevector"))
                  ((2) (raise-type-error b8 'bytevector-fill! "fixnum"))
                  ((3) (raise-type-error b8 'bytevector-fill! "-128 <= b8 <= 255"))
                  ((4) (raise-type-error start 'bytevector-fill! "fixnum"))
                  ((5) (raise-type-error end 'bytevector-fill! "fixnum or #f"))
                  ((6) (raise-bounds-error 'bytevector-fill!
                                           (list bv start end))))))))))

    (define-syntax endianness
      (er-macro-transformer
       (lambda (exp rename compare)
         (let* ((sym (list-ref exp 1))
                (str (symbol->string sym)))
           (if (member str '("little" "big"))
               `(,(rename 'quote) ,sym)
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
        (check! bytevector? bv 'bytevector->hex-string "bytevector")
        (let* ((size (##sys#size bv))
               (end (or end size)))
          (unless-unsafe-and
              (and (fixnum? start) (fixnum? end)
                   (fx<= 0 start)
                   (fx<= start end) (fx<= end size))
            (raise-bounds-error 'bytevector->hex-string (list bv start end)))
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
        (unless-unsafe-and (fx= 0 (fxrem (fx- end start) 2))
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

    (: u8-list->bytevector
       ((list-of fixnum) #!optional
                         fixnum
                         (or false fixnum)
        -> bytevector))
    (define u8-list->bytevector
      (let* ((%u8-list->bytevector
              (foreign-lambda* scheme-object ((nonnull-bytevector bv)
                                              (size_t start)
                                              (size_t end)
                                              (scheme-object lst)
                                              (scheme-object return_info))
                "\
size_t i;

for (i = 0; i < start; i++) {
	if (C_immediatep(lst) || !C_truep(C_pairp(lst))) {
		C_u_i_car(return_info) = C_fix(3);
		C_return(lst);
	}
	lst = C_u_i_cdr(lst);
}

for (; i < end; i++, bv++) {
	if (C_immediatep(lst) || !C_truep(C_pairp(lst))) {
		C_u_i_car(return_info) = C_fix(3);
		C_return(lst);
	}

	if (!C_truep(C_fixnump(C_u_i_car(lst)))) {
		C_u_i_car(return_info) = C_fix(4);
		C_return(C_u_i_car(lst));
	}
	if (C_unfix(C_u_i_car(lst)) < 0 || C_unfix(C_u_i_car(lst)) > 255) {
		C_u_i_car(return_info) = C_fix(4);
		C_return(C_u_i_car(lst));
	}

	*bv = C_unfix(C_u_i_car(lst));
	lst = C_u_i_cdr(lst);
}

C_return(C_SCHEME_TRUE);"))
             (with-error-handling
              (lambda (lst len start end)
                (let* ((bv (make-bytevector len))
                       (return-info (cons 0 #f))
                       (irritant (%u8-list->bytevector bv start end lst
                                                       return-info)))
                  (case (car return-info)
                    ((0) bv)
                    ((3) (raise-type-error irritant
                                           'u8-list->bytevector
                                           "pair"))
                    ((4) (raise-type-error irritant
                                           'u8-list->bytevector
                                           "unsigned byte")))))))
      (lambda (lst #!optional (start 0) (end #f))
        (unless-unsafe-and (and (fixnum? start)
                                (fx<= 0 start))
          (raise-bounds-error 'u8-list->bytevector
                              (list lst start end)))
        (if end
            (begin
              (unless-unsafe-and (and (fixnum? end)
                                      (fx<= start end))
                (raise-bounds-error 'u8-list->bytevector
                                    (list lst start end)))
              (with-error-handling lst (fx- end start) start end))
            (let* ((lst (list-tail lst start))
                   (len (length+ lst)))
              (unless-unsafe-and len
                (raise-type-error lst 'u8-list->bytevector "list"))
              (with-error-handling lst len 0 len))))))

    (: bytevector->u8-list
       (bytevector #!optional
                   fixnum
                   (or false fixnum)
        -> (list-of fixnum)))
    (define bytevector->u8-list
      (let* ((%bytevector->u8-list
              (foreign-lambda* scheme-object ((nonnull-bytevector bv)
                                              (size_t start)
                                              (size_t end)
                                              (scheme-object head))
                "\
C_word lst = head;
bv += start;

for (; start < end; start++, bv++) {
	C_u_i_car(lst) = C_fix(*bv);
	lst = C_u_i_cdr(lst);
}
C_return(head);")))
        (lambda (bv #!optional (start 0) (end #f))
          (check! bytevector? bv 'bytevector->u8-list "bytevector")
          (let* ((len (bytevector-length bv))
                 (end (or end len)))
            (unless-unsafe-and (and (fixnum? start)
                                    (fixnum? end)
                                    (fx<= 0 start)
                                    (fx<= start end)
                                    (fx<= end len))
             (raise-bounds-error 'bytevector->u8-list
                                 (list bv start end)))
            (%bytevector->u8-list bv start end
                                  (make-list (fx- end start)))))))

    (: bytevector-s8-ref (bytevector fixnum --> fixnum))
    (define bytevector-s8-ref
      (let ((%bytevector-s8-ref
             (foreign-lambda* scheme-object ((scheme-object bv)
                                             (scheme-object n))
               "\
size_t ind;
if (C_immediatep(bv) || !C_bytevectorp(bv)) {
	C_return(C_fix(128));
}

if (!C_truep(C_fixnump(n)) || C_unfix(n) < 0 || C_unfix(n) > SIZE_MAX) {
	C_return(C_fix(129));
}

ind = C_unfix(n);
if (ind >= C_u_i_bytevector_length(bv)) {
	C_return(C_fix(129));
}

C_return(C_fix((int8_t)C_c_string(bv)[ind]));")))
        (lambda (bv k)
          (let ((r (%bytevector-s8-ref bv k)))
          (case r
            ((128)
             (raise-type-error bv 'bytevector-s8-ref "bytevector"))
            ((129)
             (raise-index-error 'bytevector-s8-ref bv k))
            (else r))))))

    (: bytevector-s8-set! (bytevector fixnum fixnum -> undefined))
    (define bytevector-s8-set!
      (let ((%bytevector-s8-set
             (foreign-lambda* int ((scheme-object bv)
                                   (scheme-object in)
                                   (scheme-object byte))
               "\
size_t i;
if (C_immediatep(bv) || !C_truep(C_bytevectorp(bv))) {
	C_return(1);
} else if (!C_truep(C_fixnump(in)) ||
           C_unfix(in) < 0 || C_unfix(in) >= SIZE_MAX) {
	C_return(2);
}

i = C_unfix(in);

if (i >= C_u_i_bytevector_length(bv)) {
	C_return(3);
}

if (!C_truep(C_fixnump(byte) ||
    C_unfix(byte) < -128 ||
    C_unfix(byte) > 127)) {
	C_return(4);
}

(C_c_string(bv))[i] = (uint8_t)C_unfix(byte);
C_return(0);")))
        (lambda (bv k byte)
          (let ((rcode (%bytevector-s8-set bv k byte)))
            (case rcode
              ((0) (void))
              ((1) (raise-type-error bv 'bytevector-s8-set!
                                     "bytevector"))
              ((2) (raise-type-error k 'bytevector-s8-set! "index"))
              ((3) (raise-index-error 'bytevector-s8-set! bv k))
              ((4) (raise-type-error byte 'bytevector-s8-set! "signed byte")))))))

    (foreign-declare "\
static inline void reverse(void *p, size_t len)
{
	unsigned char *c = p;
	unsigned char tmp;
	size_t i;

	for (i = 0; i < len/2; i++) {
		tmp = c[i];
		c[i] = c[len - i - 1];
		c[len - i - 1] = tmp;
	}
}

static int check_bytevector_args(C_word bv, C_word n, size_t size)
{
	size_t ind;

	if (C_immediatep(bv) || !C_bytevectorp(bv)) {
		return 1;
	}

	if (!C_truep(C_fixnump(n)) || C_unfix(n) < 0 || C_unfix(n) > SIZE_MAX) {
		return 2;
	}

	ind = C_unfix(n);
	if (C_u_i_bytevector_length(bv) == 0
	    || ind > C_u_i_bytevector_length(bv) - size) {
		return 3;
	}

	return 0;
}")

    (define-syntax create-ref
      ;; (create-regular-ref c-type limit reverse?)
      (ir-macro-transformer
        (lambda (exp rename compare)
          (let ((ctype (list-ref exp 1))
                (limit (number->string (list-ref exp 2)))
                (reverse (if (list-ref exp 3)
                             "reverse(&v, sizeof v);"
                             "")))
            (define inner #<#EOF
#{ctype} v;
int e = check_bytevector_args(bv, n, sizeof v);
size_t i;

if (e != 0) {
	C_return(C_fix(#{limit} + e));
} else {
	memcpy(&v, C_c_string(bv) + C_unfix(n), sizeof v);
	#reverse
	C_return(C_fix(v));
}
EOF
)
            `(foreign-lambda* scheme-object ((scheme-object bv)
                                             (scheme-object n))
               ,inner)))))

    (define-syntax create-set
      ;; (create-regular-set c-type lolim hilim reverse?)
      (ir-macro-transformer
        (lambda (exp inject compare)
          (let ((ctype (list-ref exp 1))
                (lolim (number->string (list-ref exp 2)))
                (hilim (number->string (list-ref exp 3)))
                (reverse (if (list-ref exp 4)
                             "reverse(&cv, sizeof cv);"
                             "")))
            (define inner #<#EOF
#ctype cv;
int e = check_bytevector_args(bv, n, sizeof cv);
if (!C_truep(C_fixnump(v)) || C_unfix(v) < #lolim || C_unfix(v) > #hilim) {
	e = 3;
}

if (e == 0) {
	cv = C_unfix(v);
	#reverse
	memcpy(C_c_string(bv) + C_unfix(n), &cv, sizeof(cv));
}
C_return(C_fix(e));
EOF
)
            `(foreign-lambda* scheme-object ((scheme-object bv)
                                             (scheme-object n)
                                             (scheme-object v))
               ,inner)))))

    (define-syntax make-fixnum-ref
      (syntax-rules ()
        ((_ ctype lolim hilim ref set native-ref native-set)
         (begin
           (: ref (bytevector fixnum symbol --> fixnum))
           (define ref
             (let ((%regular-ref (create-ref ctype hilim #f))
                   (%reversed-ref (create-ref ctype hilim #t)))
               (lambda (bv k endianness)
                 (let-syntax ((c     ; (c expression)
                               (ir-macro-transformer
                                 (lambda (exp inject compare)
                                   (let ((v (list-ref exp 1))
                                         (bv (list-ref exp 2))
                                         (k (list-ref exp 3)))
                                     `(let ((r ,v))
                                        (case r
                                          ((,(fx+ hilim 2))
                                           (raise-type-error ,bv
                                                             (quote ref)
                                                             "bytevector"))
                                          ((,(fx+ hilim 3))
                                           (raise-index-error (quote ref)
                                                              ,bv
                                                              ,k))
                                          (else r))))))))
                   (cond-expand
                     (little-endian
                       (case endianness
                         ((little) (c (%regular-ref bv k) bv k))
                         ((big) (c (%reversed-ref bv k) bv k))
                         (else (raise-type-error endianness
                                                 (quote ref)
                                                 "endianness"))))
                     (big-endian
                       (case endianness
                         ((big) (c (%regular-ref bv k) bv k))
                         ((little) (c (%reversed-ref bv k) bv k))
                         (else (raise-type-error endianness
                                                 (quote ref)
                                                 "endianness")))))))))
          (: set (bytevector fixnum fixnum symbol -> undefined))
          (define set
            (let ((%regular-set
                   (create-set ctype lolim hilim #f))
                  (%reverse-set
                   (create-set ctype lolim hilim #t)))
              (lambda (bv k n endianness)
                (let-syntax ((c
                              (syntax-rules ()
                                ((_ v)
                                 (let ((r v))
                                   (case r
                                     ((0) (void))
                                     ((1)
                                      (raise-type-error bv (quote set)
                                                        "bytevector"))
                                     ((2)
                                      (raise-index-error (quote set) bv k))
                                     ((3)
                                      (raise-type-error n (quote set)
                                                        "unsigned 16 bit word"))))))))
                  (cond-expand
                    (little-endian
                      (case endianness
                        ((little) (c (%regular-set bv k n)))
                        ((big) (c (%reverse-set bv k n)))
                        (else (raise-type-error endianness
                                                (quote set)
                                                "endianness"))))
                    (big-endian
                      (case endianness
                        ((big) (c (%regular-set bv k n)))
                        ((little) (c (%reverse-set bv k n)))
                        (else (raise-type-error endianness
                                                (quote set)
                                                "endianness")))))))))
          ;;; Native refs and sets are just wrappers around the regular refs
          ;;; and sets. Presumably a Sufficiently Smart Compiler™ would be
          ;;; able to optimize these inline, but CHICKEN doesn't do that.
          (: native-ref (bytevector fixnum --> fixnum))
          (define (native-ref bv k)
            (ref bv k (native-endianness)))
          (: native-set (bytevector fixnum fixnum -> undefined))
          (define (native-set bv k n)
            (set bv k n (native-endianness)))))))

      (make-fixnum-ref "uint16_t" 0 #xFFFF
                       bytevector-u16-ref
                       bytevector-u16-set!
                       bytevector-u16-native-ref
                       bytevector-u16-native-set!)

      (: bytevector-uint-ref
         (bytevector fixnum symbol fixnum --> integer))
      (define (bytevector-uint-ref bv k endianness size)
        (check! bytevector? bv 'bytevector-uint-ref "bytevector")
        (unless-unsafe-and (and (fixnum? k)
                                (fx<= 0 k)
                                (fx<= k (fx- (bytevector-length bv)
                                             size)))
          (raise-index-error 'bytevector-uint-ref
                             bv
                             k))
        (unless-unsafe-and (and (fixnum? size)
                                (fx>= size 1))
          (raise-type-error k 'bytevector-uint-ref "index"))
        (case endianness
          ((big)
           (do ((i 0 (fx+ i 1))
                (k k (fx+ k 1))
                (n 0 (bitwise-ior (arithmetic-shift n 8)
                                  (bytevector-u8-ref bv k))))
               ((fx= i size) n)))
          ((little)
           (do ((i 0 (fx+ i 1))
                (k (fx+ k (fx- size 1)) (fx- k 1))
                (n 0 (bitwise-ior (arithmetic-shift n 8)
                                  (bytevector-u8-ref bv k))))
               ((fx= i size) n)))
          (else (raise-type-error endianness 'bytevector-uint-ref "endianness"))))

      (: bytevector-sint-ref
         (bytevector fixnum symbol fixnum --> integer))
      (define (bytevector-sint-ref bv k endianness size)
        (let ((v (bytevector-uint-ref bv k endianness size)))
          (if (= (integer-length v) (* size 8))
              (bitwise-ior (arithmetic-shift -1 (* size 8))
                           v)
              v)))

      (define (%bytevector-arb-set! bv k n endianness size)
        (case endianness
          ((little)
           (do ((i 0 (fx+ i 1))
                (k k (fx+ k 1))
                (n n (arithmetic-shift n -8)))
               ((fx= i size))
             (bytevector-u8-set! bv k (bitwise-and n #xFF))))
          ((big)
           (do ((i 0 (fx+ i 1))
                (k (fx+ k (fx- size 1)) (fx- k 1))
                (n n (arithmetic-shift n -8)))
               ((fx= i size))
             (bytevector-u8-set! bv k (bitwise-and n #xFF))))
          (else (raise-type-error endianness
                                  'bytevector-uint-set!
                                  "endianness"))))
      (: bytevector-uint-set!
         (bytevector fixnum integer symbol fixnum -> undefined))
      (define (bytevector-uint-set! bv k n endianness size)
        (check! bytevector? bv 'bytevector-uint-set! "bytevector")
        (unless-unsafe-and (and (fixnum? k)
                                (fx<= 0 k)
                                (fx<= k (fx- (bytevector-length bv)
                                             size)))
          (raise-index-error 'bytevector-uint-set!
                             bv
                             k))
        (unless-unsafe-and (and (fixnum? size)
                                (fx>= size 1))
          (raise-type-error k 'bytevector-uint-set! "index"))
        (unless-unsafe-and (and (exact-integer? size)
                                (fx<= 0 n)
                                (fx<= (integer-length n)
                                      (fx* size 8)))
          (raise-type-error n 'bytevector-uint-set! "sized number"))
        (%bytevector-arb-set! bv k n endianness size))

      (: bytevector-sint-set!
         (bytevector fixnum integer symbol fixnum -> undefined))
      (define (bytevector-sint-set! bv k n endianness size)
        (check! bytevector? bv 'bytevector-sint-set! "bytevector")
        (unless-unsafe-and (and (fixnum? k)
                                (fx<= 0 k)
                                (fx<= k (fx- (bytevector-length bv)
                                             size)))
          (raise-index-error 'bytevector-sint-set!
                             bv
                             k))
        (unless-unsafe-and (and (fixnum? size)
                                (fx>= size 1))
          (raise-type-error k 'bytevector-sint-set! "index"))
        (unless-unsafe-and (and (exact-integer? size)
                                (fx<= (fx+ 1 (integer-length n))
                                      (fx* size 8)))
          (raise-type-error n 'bytevector-sint-set! "sized number"))
        (%bytevector-arb-set! bv k n endianness size))

      ;;; TODO: Special-case these procedures for numbers that are known
      ;;; to fit in a fixnum.

      (define (make-list->bytevector set! who)
        (lambda (%lst endianness size #!optional (start 0) (end #f))
          (define (do-bytevector lst len)
            (do ((bv (make-bytevector (fx* len size)))
                 (k 0 (fx+ k size))
                 (lst lst (cdr lst)))
                ((fx= k (bytevector-length bv)) bv)
              (set! bv k (car lst) endianness size)))
  
          (check! fixnum? size who "fixnum")
          (unless-unsafe-and (fx> size 0)
            (raise-type-error size who "positive fixnum"))
          (unless-unsafe-and (and (fixnum? start) (fx<= 0 start))
            (raise-bounds-error who (list %lst start)))
          (let ((lst (list-tail %lst start)))
            (cond
              (end
               (unless-unsafe-and (and (fixnum? end) (fx<= start end))
                 (raise-bounds-error who
                                     (list lst start end)))
               (do-bytevector lst (fx- end start)))
              ((length+ lst)
               => (lambda (len)
                    (do-bytevector lst len)))
              (else
                (raise-type-error lst who "list"))))))

      (: uint-list->bytevector
         ((list-of integer) symbol fixnum
          #!optional fixnum (or false fixnum)
          -> bytevector))
      (define uint-list->bytevector
        (make-list->bytevector bytevector-uint-set! 'uint-list->bytevector))

      (: sint-list->bytevector
         ((list-of integer) symbol fixnum
          #!optional fixnum (or false fixnum)
          -> bytevector))
      (define sint-list->bytevector
        (make-list->bytevector bytevector-sint-set! 'sint-list->bytevector))

      (define (make-bytevector->list ref who)
        (lambda (bv endianness size #!optional (start 0) (end #f))
          (check! bytevector? bv who "bytevector")
          (let* ((len (bytevector-length bv))
                 (end (or end len)))
            (unless-unsafe-and (and (fixnum? start) (fixnum? end)
                                    (fx<= 0 start)
                                    (fx<= start end)
                                    (fx<= end len))
              (raise-bounds-error who (list bv start end)))
            (let loop ((start start))
              (if (fx= start end)
                  '()
                  (cons (ref bv start endianness size)
                        (loop (fx+ start size))))))))

      (: bytevector->uint-list
         (bytevector symbol fixnum fixnum (or fixnum false)
          -> (list-of integer)))
      (define bytevector->uint-list
        (make-bytevector->list bytevector-uint-ref 'bytevector->uint-list))
      (: bytevector->sint-list
         (bytevector symbol fixnum fixnum (or fixnum false)
          -> (list-of integer)))
      (define bytevector->sint-list
        (make-bytevector->list bytevector-sint-ref 'bytevector->sint-list))
))
