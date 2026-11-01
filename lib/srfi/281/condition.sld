(define-library (srfi 281 condition)
  (export &unicode-decoding-condition
          make-unicode-decoding-condition
          unicode-decoding-condition?
          make-bytevector-range-condition
          &bytevector-range-condition
          bytevector-range-condition?
          bytevector-range-condition-start
          bytevector-range-condition-end
          bytevector-range-condition-bytevector)
  (cond-expand
    ((library (rnrs conditions))
     (import (only (rnrs conditions)
                   define-condition-type
                   &condition))
     (begin
       (define-condition-type &unicode-decoding-condition &condition
         make-unicode-decoding-condition
         unicode-decoding-condition?)
       (define-condition-type &bytevector-range-condition &condition
         make-bytevector-range-condition
         bytevector-range-condition?
         (bytevector bytevector-range-condition-bytevector)
         (start bytevector-range-condition-start)
         (end bytevector-range-condition-end))))))
