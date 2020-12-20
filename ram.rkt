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

;; (ram-set ram offset val) returns Ram with the given offset byte set to val

;; ram-set: Ram Nat Byte -> Ram
(define (ram-set ram offset val)
  (vector-append (vector-take ram offset) (vector val)
                 (vector-drop ram (add1 offset))))

;; (ram-ref ram n) returns the n-th byte in Ram

;; ram-ref: Ram Nat -> Byte
(define ram-ref vector-ref)

;; (ram-size ram) returns the length of Ram

;; ram-size: Ram -> Nat
(define ram-size vector-length)

;;------------------------------------------------------------------------------

;; (load-program program offset ram) consumes a Program and an offset number
;; and returns Ram with the Program loaded in decimal at that offset

;; load-program: Program Nat Ram -> Ram
(define (load-program program offset ram)
  (cond [(string=? program "") ram]
        [(> (+ offset (/ (string-length program) 2)) (ram-size ram))
         (error 'load-program "program can't be loaded into memory at +" offset)]
        [else (load-program
                (substring program 2) (add1 offset)
                (ram-set ram offset (hex->dec (substring program 0 2))))]))

;; Tests:
(check-error (load-program p 5555555 (make-ram)))

;;------------------------------------------------------------------------------