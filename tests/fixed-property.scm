(define (test-fixed-property name width signed? ref set!)
  (test-group name
    (test-property
     (lambda (endianness word len)
       (let ((bv (make-bytevector len 0))
             (idx ((make-random-integer-generator 0 (- len width -1)))))
         (set! bv idx word endianness)
         (= (ref bv idx endianness) word)))
     (list (endianness-generator)
           (if signed?
               (make-random-signed-integer-generator width)
               (make-random-unsigned-integer-generator width))
           (make-random-integer-generator width 100)))))

(define (test-native-fixed-property name width signed? ref set!)
  (test-group name
    (test-property
     (lambda (word len)
       (let* ((bv (make-bytevector (* len width) 0))
              (idx (*
                    ((make-random-integer-generator 0 len))
                    width)))
         (set! bv idx word)
         (= (ref bv idx) word)))
     (list (if signed?
               (make-random-signed-integer-generator width)
               (make-random-unsigned-integer-generator width))
           (make-random-integer-generator 1 100)))))
