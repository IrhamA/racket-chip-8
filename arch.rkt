#lang racket

;; Required for (check-expect ...), put (test) at the end of the file to see the
;; test results
(require test-engine/racket-tests) 

;; Required to make these functions accessible in other files
(provide (all-defined-out))

;;------------------------------------------------------------------------------

;; A Byte is a Nat in the range [0, 255]
;; A Word is a Nat in the range [0, 65535]

;; A HexChar is one of:
;;     #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\a #\b #\c #\d #\e #\f
;; A HexStr is a string consisting only of HexChars

;; A Program is a HexStr

;; To-do: Maybe we should stop doing all this (begin ...) and Void and #:mutable
;; stuff. It may make the program easier to write but it's unracketlike

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

;; The standard RAM size for CHIP-8 is 4K
(define max-ram 4096)

;; A Ram is a (vectorof Byte)
(define make-ram (λ () (make-vector max-ram 0)))

;; To-do: these are just wrappers but they might be useful if we switch to a
;; using structs for Ram

;; (ram-set! ram offset val) sets the specified value in a Ram
;; ram-set!: Ram Nat Byte -> Void
(define (ram-set! ram offset val)
  (vector-set! ram offset val))

;; (ram-ref ram n) returns the n-th byte in Ram
;; ram-ref: Ram Nat -> Byte
(define (ram-ref ram n)
  (vector-ref ram n))

;; Test Ram
(define ram (make-ram))

;;------------------------------------------------------------------------------

;; (load-program! program offset ram) consumes a Program and an offset number
;; and loads the program into RAM at that offset
;; load-program!: Program Nat Ram -> Void
(define (load-program! program offset ram)
  (cond [(string=? program "") 0]
        [(> (+ offset (/ (string-length program) 2)) max-ram)
         (error 'load-program! "program can't be loaded into memory at +" offset)]
        [else (begin (ram-set! ram offset (hex->dec (substring program 0 2)))
                     (load-program! (substring program 2 (string-length program))
                                    (add1 offset) ram))]))

;; Tests:
(check-error (load-program! p 5555555 (make-ram)))

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
;; Requires:
;;     (length char) = 1
(define (char->dec char)
  (cond [(equal? #\a char) 10]
        [(equal? #\b char) 11]
        [(equal? #\c char) 12]
        [(equal? #\d char) 13]
        [(equal? #\e char) 14]
        [(equal? #\f char) 15]
        [else (- (char->integer char) (char->integer #\0))]))

;; Tests:
(check-expect (char->dec #\0) 0)
(check-expect (char->dec #\9) 9)
(check-expect (char->dec #\a) 10)
(check-expect (char->dec #\f) 15)

;;------------------------------------------------------------------------------