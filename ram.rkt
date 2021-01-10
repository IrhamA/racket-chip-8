#lang racket

;; For (check-expect)s
(require test-engine/racket-tests)
(require "util.rkt")

(provide (all-defined-out))

;;------------------------------------------------------------------------------

;; The standard RAM size for CHIP-8 is 4K
(define max-ram 4096)

;; A Ram is a (vectorof Byte)
(define make-ram (λ () (make-vector max-ram 0)))

;; To-do: these are just wrappers but they might be useful if we switch to a
;; using structs for Ram

;; (ram-set! ram offset val) sets the byte at offset in ram to val

;; ram-set!: Ram Nat Byte -> Void
(define ram-set! vector-set!)

;; (ram-ref ram n) returns the n-th byte in Ram

;; ram-ref: Ram Nat -> Byte
(define ram-ref vector-ref)

;; (ram-size ram) returns the length of Ram

;; ram-size: Ram -> Nat
(define ram-size vector-length)

;; (ram-set-list! ram offset data) sets a range of bytes starting at offset in
;; ram to the values in a given list

;; ram-set-list!: Ram Nat (listof Byte) -> Void
(define (ram-set-list! ram offset data)
  (cond [(empty? data) (void)]
        [else (begin (ram-set! ram offset (first data))
                     (ram-set-list! ram (add1 offset) (rest data)))]))

;;------------------------------------------------------------------------------

;; (load-program! program offset ram) consumes a Program and an offset number
;; and returns Ram with the Program loaded in decimal at that offset

;; load-program!: Program Nat Ram -> Ram
(define (load-program! program offset ram)
  (cond [(string=? program "") ram]
        [(> (+ offset (/ (string-length program) 2)) (ram-size ram))
         (error 'load-program "program can't be loaded into memory at +" offset)]
        [else (begin (ram-set! ram offset (hex->dec (substring program 0 2)))
                     (load-program! (substring program 2 (string-length program))
                                    (add1 offset) ram)
                     (void))]))

;; Tests:
(check-error (load-program! p 5555555 (make-ram)))

;;------------------------------------------------------------------------------