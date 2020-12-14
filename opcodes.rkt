#lang racket

;; Allows you to access functions from "arch.rkt"
(require "arch.rkt")

;; Required to make these functions accessible in other files
(provide (all-defined-out))

;;------------------------------------------------------------------------------

;; (opcode_0nnn ram registers nnn) calls machine code routine at address nnn
;; opcode_0nnn: Ram Registers -> Void
(define (opcode_00e0 ram registers)
  (void)) ;; Remove this void when you write the function body

;;------------------------------------------------------------------------------

;; (opcode_00e0 ram registers) clears the screen
;; opcode_00e0: Ram Registers -> Void
(define (opcode_00e0 ram registers)
  (void)) ;; Remove this void when you write the function body

;;------------------------------------------------------------------------------

;; ... and so on, remove this comment when you're done ...