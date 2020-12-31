#lang racket

;; For (check-expect)s
(require test-engine/racket-tests) 

(provide (all-defined-out))

;;------------------------------------------------------------------------------

;; (hex->dec str) consumes a HexStr (str) and returns its decimal value

;; hex->dec: HexStr -> Nat
(define (hex->dec str)
  (letrec (;; (chars->dec loc) converts a list of HexChar into a natural number
           ;; chars->dec: (listof HexChar) -> Nat
           [chars->dec (λ (loc)
             (cond [(empty? loc) 0]
                   [else (+ (* (expt 16 (length (rest loc)))
                               (char->dec (first loc)))
                            (chars->dec (rest loc)))]))])

  (chars->dec (string->list str))))

;; Tests:
(check-expect (hex->dec "1af31b") 1766171)
(check-expect (hex->dec "ba") 186)
(check-expect (hex->dec "cc") 204)
(check-expect (hex->dec "de4") 3556)

;;------------------------------------------------------------------------------

;; (char->dec char) takes a HexChar and produces its corrsesponding decimal value

;; char->dec HexChar -> Nat
(define (char->dec char)
  (cond [(char=? #\a char) #xa]
        [(char=? #\b char) #xb]
        [(char=? #\c char) #xc]
        [(char=? #\d char) #xd]
        [(char=? #\e char) #xe]
        [(char=? #\f char) #xf]
        [else (- (char->integer char) (char->integer #\0))]))

;; Tests:
(check-expect (char->dec #\0) 0)
(check-expect (char->dec #\9) 9)
(check-expect (char->dec #\a) 10)
(check-expect (char->dec #\f) 15)

;;------------------------------------------------------------------------------

;; (dec->hex num) converts a decimal number to a string of hexadecimal chars,
;; If the string is less than one byte then it is formatted as two chars 

;; dec->hex Nat -> HexStr
(define (dec->hex num)
  (let ([str (number->string num 16)])
    (if (< (string-length str) 2)
        (string-append "0" str) str)))

;; Tests:
(check-expect (dec->hex 65535) "ffff")
(check-expect (dec->hex 1766171) "1af31b")

;;------------------------------------------------------------------------------

;; (get-hex-digit-n number n) returns the nth digit (from the right) of a number
;; in hexadecimal

;; get-hex-digit-n: Nat Nat -> Nat
(define (get-hex-digit-n number n)
  (quotient (bitwise-and number (* (expt #x10 n) #xf)) (expt #x10 n)))

(check-expect (get-hex-digit-n #x1f2ce31a 1) 1)
(check-expect (get-hex-digit-n #x8afe2 0) 2)
(check-expect (get-hex-digit-n #xa 0) 10)
(check-expect (get-hex-digit-n #xfccabfdcbd3f619c36bfdcba96dc3baf69da1 26) 3)

;;------------------------------------------------------------------------------

;; (bitwise-shift-left byte n) shifts a byte left n times

;; bitwise-shift-left: Byte Nat -> Byte
(define (bitwise-shift-left byte n)
  (cond [(= 0 n) byte]
        [else (bitwise-shift-left (modulo (* 2 byte) 256) (sub1 n))]))

;; (bitwise-shift-right byte n) shifts a byte right n times

;; bitwise-shift-right: Byte Nat -> Byte
(define (bitwise-shift-right byte n)
  (cond [(= 0 n) byte]
        [else (bitwise-shift-right (quotient byte 2) (sub1 n))]))

;;------------------------------------------------------------------------------