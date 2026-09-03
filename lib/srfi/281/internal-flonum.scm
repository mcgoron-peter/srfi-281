; SPDX-FileCopyrightText: 2026 Peter McGoron
;
; SPDX-License-Identifier: MIT

;;; This code assumes that inexact reals are radix-2 IEEE floats.

(define fl-radix 2)

(define distinguishes-negative-zero?
  (not (eqv? -0.0 +0.0)))

(define (sign-negative? fl)
  (cond
   ((negative? fl) #t)
   ((positive? fl) #f)
   ((and distinguishes-negative-zero?
         (eqv? fl -0.0))
    #t)
   (else #f)))

(define (normalized-sign-significand-exponent fl)
  ;; Returns
  ;; 1. The sign bit as a boolean.
  ;; 2. The significand as a flonum (1 <= fl < fl-radix)
  ;; 3. The exponent as an exact integer.
  (if (< (abs fl) fl-radix)

      (do ((sign (sign-negative? fl))
           (fl (abs fl) (* fl fl-radix))
           (exp 0 (fx- exp 1)))
          ((and (<= 1 fl) (< fl fl-radix))
           (values sign fl exp)))

      (do ((sign (sign-negative? fl))
           (fl (abs fl) (/ fl fl-radix))
           (exp 0 (fx+ exp 1)))
          ((and (<= 1 fl) (< fl fl-radix))
           (values sign fl exp)))))

(define (get-sigfigs exponent
		     least-normalized-exponent
		     representation-precision)
  ;; If the exponent is below the normal exponent, then reduce
  ;; the number of sigfigs in the significand to correctly capture
  ;; the subnormal representation.
  (do ((sigfigs representation-precision (fx- sigfigs 1))
       (exponent exponent (fx+ exponent 1)))
      ((or (fx>=? exponent least-normalized-exponent)
           (zero? sigfigs))
       (values exponent sigfigs))))

(define (significand->digits significand sigfigs)
  ;; Convert the significand (1 <= significand < fl-radix)
  ;; to a list of digits, least significant first.
  ;;
  ;; The function returns two values: the accumulated list, and the rest of
  ;; the significand. This is useful for rounding.
  (let loop ((i 0) (significand significand) (acc '()))
    (cond
      ((fx=? i sigfigs) (values acc significand))
      (else
       (let ((bit (exact (truncate significand))))
         (loop (fx+ i 1)
               (* fl-radix (- significand bit))
               (cons bit acc)))))))

(define (plus1 list exponent)
  ;; Add 1 to the list that represents the significand. When the list is
  ;; made up of all 1s, return (0 0 ... 1) with a bumped exponent.
  (let loop ((list list))
    (cond
      ((and (null? (cdr list))
            (= (car list) 1))
       (values '(1) (+ exponent 1)))
      ((= (car list) 0)
       (values (cons 1 (cdr list))
               exponent))
      (else (let-values (((rest exponent) (loop (cdr list))))
              (values (cons 0 rest) exponent))))))

(define (round-to-even list exponent remainder)
  (cond
    ((null? list) (values list exponent)) ; zero
    ((and (= (car list) 1)             ; At an odd number ...
          (= remainder 1))           ; and at tie-breaking point
     (plus1 list exponent))
    ((> remainder 1) (plus1 list exponent))
    (else (values list exponent))))

(define (convert-to-representation fl
                                   least-normalized-exponent
                                   representation-precision)
  (let*-values (((sign significand exponent)
                 (normalized-sign-significand-exponent fl))
                ((exponent sigfigs)
                 (get-sigfigs exponent
			      least-normalized-exponent
			      representation-precision))
                ((bits remainder)
                 (significand->digits significand
				      sigfigs))
                ((bits exponent)
                 (round-to-even bits exponent remainder)))
    (values sign
            (if (null? bits)
                bits
                (drop-right bits 1))
            exponent)))

(define (bits->flonum byte exponent)
  (do ((byte byte (fxarithmetic-shift-right byte 1))
       (i 0 (fx+ i 1))
       (exponent exponent (fx+ exponent 1))
       (flonum 0.0
               (if (fxzero? (fxand byte 1))
                   flonum
                   (+ flonum (expt 2.0 exponent)))))
      ((fx=? i 8) flonum)))

