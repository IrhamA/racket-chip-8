#lang racket

(provide (all-defined-out))

;;------------------------------------------------------------------------------

(define display-width 64)
(define display-height 32)

;; A Display is a (vectorof Byte)
(define make-display
  (λ () (make-vector (/ (* display-width display-height) 8) 0)))

;;------------------------------------------------------------------------------