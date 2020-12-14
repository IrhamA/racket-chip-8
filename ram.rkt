#lang racket

;; For (check-expect)s
(require test-engine/racket-tests)

(provide (all-defined-out))

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