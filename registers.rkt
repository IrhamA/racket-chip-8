#lang racket

(provide (all-defined-out))

;;------------------------------------------------------------------------------

;; A Registers is a
;; (make-registers Word Word Byte Byte Byte (vectorof Byte)
(define-struct registers (pc i sp dt st v) #:mutable #:transparent)

;; (registers-vn reg n) returns the vn register of a Registers
;; registers-vn: Registers Nat -> Byte
;; Requires:
;;     0 <= n <= 15
(define (registers-vn reg n)
  (vector-ref (registers-v reg) n))

;; (set-registers-vn! reg n val) sets the value in vn of a Registers
;; set-registers-vn!: Registers Nat Byte -> Registers
;; Requires:
;;     0 <= n <= 15
(define (set-registers-vn reg n val)
  (struct-copy registers reg [v
    (vector-append (vector-take (registers-v reg) (sub1 n))
                   (vector val)
                   (vector-drop (registers-v reg) n))]))

;;------------------------------------------------------------------------------