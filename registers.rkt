#lang racket

(provide (all-defined-out))

;;------------------------------------------------------------------------------

;; A Registers is a
;; (make-registers Word Word Byte Byte Byte (vectorof Byte)
(define-struct registers (pc i sp dt st v) #:transparent)

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

;; (registers-vn-set reg n val) returns the Registers with register vn updated
;; registers-vn-set: Registers Nat Byte -> Registers

;; Requires:
;;     0 <= n <= 15
(define (registers-vn-set reg n val)
  (struct-copy registers reg
    [v (vector-append (vector-take (registers-v reg) (sub1 n)) (vector val)
                      (vector-drop (registers-v reg) n))]))

;;------------------------------------------------------------------------------