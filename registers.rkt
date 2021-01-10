#lang racket

(provide (all-defined-out))

;;------------------------------------------------------------------------------

;; A Registers is a
;; (make-registers Word Word Word Byte Byte (vectorof Byte)
(define-struct registers (pc i sp dt st v) #:transparent #:mutable)

;; The structure fields correspond to these registers:
;; pc ... Program Counter
;; i .... Index register
;; sp ... Stack Pointer
;; dt ... Delay Timer
;; st ... Sound Timer

;; (registers-vn reg n) returns the vn register of a Registers
;; registers-vn: Registers Nat -> Byte

;; Requires:
;;     0 <= n <= 15
(define (registers-vn reg n)
  (vector-ref (registers-v reg) n))

;; (set-registers-vn! reg n val) updates nth vn register in a Registers
;; set-registers-vn!: Registers Nat Byte -> Void

;; Requires:
;;     0 <= n <= 15
(define (set-registers-vn! reg n val)
  (vector-set! (registers-v reg) n val))

;;------------------------------------------------------------------------------