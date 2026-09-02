;;;; This set of library declarations will specialize certain operations
;;;; to optimized (fixnum) procedures.

(cond-expand
  (chicken
   (import (rename (only (chicken fixnum)
                         fx+ fx* fx< fx- fx<=
                         fxand fxshr fixnum-bits)
                   (fx< fx<?)
                   (fx<= fx<=?)
                   (fxshr fxarithmetic-shift-right)
                   (fixnum-bits fx-width))
           (only (chicken base) fixnum?))
   (import (only (chicken bitwise)
                 bitwise-and arithmetic-shift))
   (begin
     (define (fxnegative? x)
       (fx< x 0))))
  ((library (srfi 143))
   (import (only (srfi 143)
                 fixnum? fxnegative? fx+ fx- fx<=?
                 fx* fx<?
                 fxand fxarithmetic-shift-right
                 fx-width))))

(cond-expand
  ((library (srfi 151))
   (import (only (srfi 151)
                 bitwise-and arithmetic-shift)))
  (else))

;;; ;;;;;;;;;;;;;;
;;; Signed<->unsigned conversion
;;; ;;;;;;;;;;;;;;

(begin
  (define (big:signed->unsigned-factory period)
    ;; Convert a signed number to its representation in two's
    ;; complement.
    ;; 
    ;; 8 bit example:
    ;; -128 maps to 128
    ;; -127 maps to 129
    ;; ...
    ;; -1 maps to 255
    ;; 
    ;; so the map is x + 256 for x < 0.
    (lambda (word)
      (if (negative? word)
          (+ word period)
          word)))
  (define (big:unsigned->signed-factory period limit)
    ;; Convert an unsigned number that represents a number in
    ;; two's complement into a signed number with the actual value.
    ;; 
    ;; 8 bit example:
    ;; 128 maps to -128
    ;; 129 maps to -127
    ;; ...
    ;; 255 maps to -1
    ;; 
    ;; so the map is x - 256 for x >= 128.
    (lambda (word)
      (if (<= limit word)
          (- word period)
          word))))

(cond-expand
  ((or (library (srfi 143)) chicken)
   (begin
     (define (signed->unsigned-factory bytes)
       (let ((period (expt 256 bytes)))
         (if (fixnum? period)
             (lambda (word)
               (if (fxnegative? word)
                   (fx+ word period)
                   word))
             (big:signed->unsigned-factory period))))
     (define (unsigned->signed-factory bytes)
       (let ((limit (/ (expt 256 bytes) 2))
             (period (expt 256 bytes)))
         (if (fixnum? period)
             (lambda (word)
               (if (fx<=? limit word)
                   (fx- word period)
                   word))
             (big:unsigned->signed-factory period limit))))))
  (else
   (begin
     (define (signed->unsigned-factory bytes)
       (let ((period (expt 256 bytes)))
         (big:signed->unsigned-factory bytes)))
     (define (unsigned->signed-factory bytes)
       (let ((limit (/ (expt 256 bytes) 2))
             (period (expt 256 bytes)))
         (big:unsigned->signed-factory period limit))))))

;;; ;;;;;;;;;;;;;;;;;;;;;
;;; Decomposing numbers into bytes
;;; ;;;;;;;;;;;;;;;;;;;;;

(cond-expand
  ((or chicken (library (srfi 151)))
   (begin
     (define (big:floor/256 word)
       (values (arithmetic-shift word -8)
               (bitwise-and word #xFF)))))
  (else
   (begin
     (define (big:floor/256 word)
       (floor/ word 256)))))

(begin
  (define (fix:floor/256 word)
    (values (fxarithmetic-shift-right word 8)
            (fxand word #xFF))))
