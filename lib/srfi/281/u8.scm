(define u8-list->bytevector
  (case-lambda
    ((list) (u8-list->bytevector list 0))
    ((list start)
     (unless (list? list)
       (error "not a list" list))
     (u8-list->bytevector list start (length list)))
    ((whole-list start end)
     (unless (<= 0 start end)
       (error "invalid bounds" start end))
     (do ((bv (make-bytevector (- end start)))
          (list (list-tail whole-list start)
                (cdr list))
          (i 0 (+ i 1)))
         ((= i (bytevector-length bv)) bv)
       (unless (pair? list)
         (error "not a list of the required size"
                whole-list
                start
                end))
       (let ((byte (car list)))
         (unless (<= 0 byte 255)
           (error "not a byte" byte))
         (bytevector-u8-set! bv i byte))))))

(define bytevector->u8-list
  (case-lambda
    ((bv) (bytevector->u8-list bv 0))
    ((bv start)
     (unless (bytevector? bv)
       (error "not a bytevector" bv))
     (bytevector->u8-list bv start
                          (bytevector-length bv)))
    ((bv start end)
     (unless (bytevector? bv)
       (error "not a bytevector" bv))
     (unless (<= 0 start end (bytevector-length bv))
       (error "invalid bounds" start end (bytevector-length bv)))
     (let loop ((i start))
       (if (= i end)
           '()
           (cons (bytevector-u8-ref bv i)
                 (loop (+ i 1))))))))