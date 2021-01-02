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

;; Bitfields for character glyphs
(define char-0 (list #b00000000
                     #b00111000
                     #b01000100
                     #b01000100
                     #b01000100
                     #b01000100
                     #b01000100
                     #b00111000))

(define char-1 (list #b00000000
                     #b00010000
                     #b00110000
                     #b00010000
                     #b00010000
                     #b00010000
                     #b00010000
                     #b01111100))

(define char-2 (list #b00000000
                     #b00111000
                     #b01000100
                     #b00000100
                     #b00001000
                     #b00010000
                     #b00100000
                     #b01111100))

(define char-3 (list #b00000000
                     #b00111000
                     #b01000100
                     #b00000100
                     #b00011000
                     #b00000100
                     #b01000100
                     #b00111000))

(define char-4 (list #b00000000
                     #b00001000
                     #b00011000
                     #b00101000
                     #b01001000
                     #b11111000
                     #b00001000
                     #b00001000))

(define char-5 (list #b00000000
                     #b01111100
                     #b01000000
                     #b01000000
                     #b01111000
                     #b00000100
                     #b01000100
                     #b00111000))

(define char-6 (list #b00000000
                     #b01111000
                     #b01000000
                     #b01000000
                     #b01111000
                     #b01000100
                     #b01000100
                     #b00111000))

(define char-7 (list #b00000000
                     #b01111110
                     #b00000010
                     #b00000100
                     #b00001000
                     #b00010100
                     #b00100100
                     #b01000000))

(define char-8 (list #b00000000
                     #b00111100
                     #b01000010
                     #b01000010
                     #b00111100
                     #b01000010
                     #b01000010
                     #b00111100))

(define char-9 (list #b00000000
                     #b00111000
                     #b01000100
                     #b01000100
                     #b00111100
                     #b00000100
                     #b00000100
                     #b00000100))

(define char-a (list #b00000000
                     #b00010000
                     #b00101000
                     #b01000100
                     #b01111100
                     #b01000100
                     #b01000100
                     #b01000100))

(define char-b (list #b00000000
                     #b01111000
                     #b01000100
                     #b01000100
                     #b01111000
                     #b01000100
                     #b01000100
                     #b01111000))

(define char-c (list #b00000000
                     #b00111000
                     #b01000100
                     #b01000000
                     #b01000000
                     #b01000000
                     #b01000100
                     #b00111000))

(define char-d (list #b00000000
                     #b00111100
                     #b01000100
                     #b01000100
                     #b01000100
                     #b01000100
                     #b01000100
                     #b00111100))

(define char-e (list #b00000000
                     #b01111100
                     #b01000000
                     #b01000000
                     #b01111100
                     #b01000000
                     #b01000000
                     #b01111100))

(define char-f (list #b00000000
                     #b01111100
                     #b01000000
                     #b01000000
                     #b01111100
                     #b01000000
                     #b01000000
                     #b0100000))

;;------------------------------------------------------------------------------