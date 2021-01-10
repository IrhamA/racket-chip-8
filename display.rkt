#lang racket/gui

(provide (all-defined-out))

;;------------------------------------------------------------------------------

(define display-width 64)
(define display-height 32)

;; Foreground and background colors in ARGB format
(define fg-color (bytes 255 134 122 222))
(define bg-color (bytes 255 72 58 170))

;; A Display is a (vectorof Byte)
(define make-display
  (λ () (make-bytes (/ (* display-width display-height) 8) 0)))

;; (display-clear! display) clears the given display by setting all pixels to 0.

;; display-clear!: Display -> Void
(define (display-clear! display)
  (bytes-fill! display 0))

;; (xor-display-byte! display index value) bitwise-xors an entire byte of pixel
;; values onto the screen.

;; xor-display-byte!: Display Nat Nat -> Void
(define (xor-display-byte! display index value)
  (bytes-set! display index (bitwise-xor (bytes-ref display index) value)))

;; (get-bit display x y) returns the bit at (x, y) on the display

;; get-bit: Display Nat Nat -> (anyof 1 0)
(define (get-bit display x y)
  (let ([index (+ x (* y display-width))])
    (quotient (bytes-ref display (quotient index 8)) (expt 2 (modulo x 8)))))

;; (xor-bit! display x y bit) sets the bit at (x, y) on the display to bit

;; xor-bit!: Display Nat Nat (anyof 1 0) -> Void
(define (xor-bit! display x y bit)
  (let ([byte (bytes-ref display (quotient (+ x (* y display-width)) 8))])
  (bytes-set! display
              (quotient (+ x (* y display-width)) 8)
              (if (zero? bit) (bitwise-xor byte (expt 2 (modulo x 8))) byte))))

;;------------------------------------------------------------------------------

;; (generate-bitmap display) creates a new bitmap% object from the display
;; using some pallette colors defined above

;; generate-bitmap: Display -> Bitmap
(define (generate-bitmap display)            
  (letrec ([bitmap (make-object bitmap% display-width display-height)]
           [generate-pixels (λ (n)
             (cond [(equal? n (* (/ display-width 8) display-height)) #""]
                   [else (bytes-append
                     (byte->pixels (bytes-ref display n))
                     (generate-pixels (add1 n)))]))])
    (send bitmap set-argb-pixels 0 0 display-width display-height (generate-pixels 0))
    bitmap))

(define (byte->pixels byte)
  (bytes-append (if (zero? (bitwise-and byte 128)) bg-color fg-color)
                (if (zero? (bitwise-and byte 64)) bg-color fg-color)
                (if (zero? (bitwise-and byte 32)) bg-color fg-color)
                (if (zero? (bitwise-and byte 16)) bg-color fg-color)
                (if (zero? (bitwise-and byte 8)) bg-color fg-color)
                (if (zero? (bitwise-and byte 4)) bg-color fg-color)
                (if (zero? (bitwise-and byte 2)) bg-color fg-color)
                (if (zero? (bitwise-and byte 1)) bg-color fg-color)))

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
                     #b00111000
                     #b01000100
                     #b01000000
                     #b01111000
                     #b01000100
                     #b01000100
                     #b00111000))

(define char-7 (list #b00000000
                     #b01111100
                     #b00000100
                     #b00001000
                     #b00010000
                     #b00010000
                     #b00100000
                     #b00100000))

(define char-8 (list #b00000000
                     #b00111000
                     #b01000100
                     #b01000100
                     #b00111000
                     #b01000100
                     #b01000100
                     #b00111000))

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
                     #b01111000
                     #b01000100
                     #b01000100
                     #b01000100
                     #b01000100
                     #b01000100
                     #b01111000))

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
                     #b01000000))

(define chars (vector char-0 char-1 char-2 char-3
                      char-4 char-5 char-6 char-7
                      char-8 char-9 char-a char-b
                      char-c char-d char-e char-f))

(define char-0-new (list #b01100000
                         #b10010000
                         #b10010000
                         #b10010000
                         #b01100000))

(define char-1-new (list #b01000000
                         #b11000000
                         #b01000000
                         #b01000000
                         #b11100000))

(define char-2-new (list #b01100000
                         #b10010000
                         #b00100000
                         #b01000000
                         #b11110000))

(define char-3-new (list #b01100000
                         #b10010000
                         #b00100000
                         #b10010000
                         #b01100000))

(define char-4-new (list #b10010000
                         #b10010000
                         #b11110000
                         #b00010000
                         #b00010000))

(define char-5-new (list #b11110000
                         #b10000000
                         #b11100000
                         #b00010000
                         #b11100000))

(define char-6-new (list #b01110000
                         #b10000000
                         #b11110000
                         #b10010000
                         #b11110000))

(define char-7-new (list #b11110000
                         #b10010000
                         #b00100000
                         #b01000000
                         #b01000000))

(define char-8-new (list #b11110000
                         #b10010000
                         #b01100000
                         #b10010000
                         #b11110000))

(define char-9-new (list #b01110000
                         #b10010000
                         #b01110000
                         #b00010000
                         #b00010000))

(define char-a-new (list #b01100000
                         #b10010000
                         #b11110000
                         #b10010000
                         #b10010000))

(define char-b-new (list #b11100000
                         #b10010000
                         #b11100000
                         #b10010000
                         #b11100000))

(define char-c-new (list #b01110000
                         #b10000000
                         #b10000000
                         #b10000000
                         #b01110000))

(define char-d-new (list #b11100000
                         #b10010000
                         #b10010000
                         #b10010000
                         #b11100000))

(define char-e-new (list #b11110000
                         #b10000000
                         #b11110000
                         #b10000000
                         #b11110000))

(define char-f-new (list #b11110000
                         #b10000000
                         #b11110000
                         #b10000000
                         #b10000000))

(define chars-new (vector char-0-new char-1-new char-2-new char-3-new
                          char-4-new char-5-new char-6-new char-7-new
                          char-8-new char-9-new char-a-new char-b-new
                          char-c-new char-d-new char-e-new char-f-new))

;;------------------------------------------------------------------------------