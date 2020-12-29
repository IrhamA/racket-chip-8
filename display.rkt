#lang racket

(provide (all-defined-out))

;;------------------------------------------------------------------------------

(define display-width 64)
(define display-height 32)

;; A Display is a (vectorof Byte)
(define make-display
  (λ () (make-bytes (/ (* display-width display-height) 8) 0)))

;; (display-clear! display) clears the given display by setting all pixels to 0.

;; display-clear!: Display -> Void
(define (display-clear! display)
  (bytes-fill! display 0))

;; (xor-display-byte! display index value)
(define (xor-display-byte! display index value)
  (bytes-set! display (bitwise-xor (bytes-ref display index) value) index))

;;------------------------------------------------------------------------------