#lang racket

(provide (all-defined-out))

;;------------------------------------------------------------------------------

;; A Registers is a
;; (make-registers Word Word Byte Byte Byte (vectorof Byte)
(define-struct registers (pc i sp dt st v) #:mutable #:transparent)

;; (registers-vn registers n) returns the vn register of a Registers
;; registers-vn: Registers Nat -> Byte
;; Requires:
;;     0 <= n <= 15
(define (registers-vn registers n)
  (vector-ref (registers-v registers) n))

;; (set-registers-vn! registers n val) sets the value in vn of a Registers
;; set-registers-vn!: Registers Nat Byte -> Void
;; Requires:
;;     0 <= n <= 15
(define (set-registers-vn! registers n val)
  (vector-set! (registers-v registers) n val))

;;------------------------------------------------------------------------------