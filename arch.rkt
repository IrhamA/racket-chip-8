#lang racket

(require "opcodes.rkt")
(require "ram.rkt")
(require "registers.rkt")

(provide (all-defined-out))

;;------------------------------------------------------------------------------

;; (cpu ram registers) simulates one clock cycle of the CHIP-8 cpu

;; cpu: Ram Registers -> Ram Registers
(define (cpu ram registers)
  (let* ([pc (registers-pc registers)]
         [opcode (+ (* 256 (ram-ref ram pc)) (ram-ref ram (add1 pc)))])
    (cond [(equal? #x00e0 opcode) (opcode-00e0 ram registers)] ; 00e0
          [(equal? #x00ee opcode) (opcode-00ee ram registers)] ; 00ee
          [((pred-mnnn #x0) opcode) (opcode-0nnn ram registers opcode)] ; 0nnn
          [((pred-mnnn #x1) opcode) (opcode-1nnn ram registers (modulo opcode #x1000))] ; 1nnn
          [((pred-mnnn #x2) opcode) (opcode-2nnn ram registers (modulo opcode #x2000))] ; 2nnn 
          [((pred-mnnn #x3) opcode) (opcode-3xnn ram registers (modulo opcode #x3000))] ; 3xnn
          [((pred-mnnn #x4) opcode) (void)] ; 4xnn
          [((pred-pnnq #x5 #x0) opcode)] ; 5xy0
          [((pred-mnnn #x6) opcode) (void)] ; 6xnn
          [((pred-mnnn #x7) opcode) (void)] ; 7xnn
          [((pred-pnnq #x8 #x0) opcode) (void)] ; 8xy0
          [((pred-pnnq #x8 #x1) opcode) (void)] ; 8xy1
          [((pred-pnnq #x8 #x2) opcode) (void)] ; 8xy2
          [((pred-pnnq #x8 #x3) opcode) (void)] ; 8xy3
          [((pred-pnnq #x8 #x4) opcode) (void)] ; 8xy4
          [((pred-pnnq #x8 #x5) opcode) (void)] ; 8xy5
          [((pred-pnnq #x8 #x6) opcode) (void)] ; 8xy6
          [((pred-pnnq #x8 #x7) opcode) (void)] ; 8xy7
          [((pred-pnnq #x8 #xe) opcode) (void)] ; 8xye
          [((pred-pnnq #x9 #x0) opcode) (void)] ; 9xy0
          [((pred-mnnn #xa) opcode) (opcode-annn ram registers (modulo opcode #xa000))] ; annn
          [((pred-mnnn #xb) opcode) (opcode-bnnn ram registers (modulo opcode #xb000))] ; bnnn
          [((pred-mnnn #xc) opcode) (void)] ; cxnn
          [((pred-mnnn #xd) opcode) (void)] ; dxyn
          [((pred-sxtt #xe #x9e) opcode) (void)] ; ex9e
          [((pred-sxtt #xe #xa1) opcode) (void)] ; exa1
          [((pred-sxtt #xf #x07) opcode) (void)] ; fx07
          [((pred-sxtt #xf #x0a) opcode) (void)] ; fx0a
          [((pred-sxtt #xf #x15) opcode) (void)] ; fx15
          [((pred-sxtt #xf #x18) opcode) (void)] ; fx18
          [((pred-sxtt #xf #x1e) opcode) (void)] ; fx1e
          [((pred-sxtt #xf #x29) opcode) (void)] ; fx29
          [((pred-sxtt #xf #x33) opcode) (void)] ; fx33
          [((pred-sxtt #xf #x55) opcode) (void)] ; fx55
          [((pred-sxtt #xf #x65) opcode) (void)]))) ; ex9e

;; (pred-mnnn m) returns a predicate that checks if the opcode is of the form
;; mnnn where m is a constant

;; pred-mnnn: Nat -> (Nat -> Bool)
(define (pred-mnnn m)
  (λ (opcode)
    (and (<= opcode (+ (* m #x1000) #x0fff))
         (>= opcode (* m #x1000)))))

;; (pred-pnnq p q) returns a predicate that checks if the opcode is of the form
;; pnnq where p and q are constants

;; pred-pnnq: Nat Nat -> (Nat -> Bool)
(define (pred-pnnq p q)
  (λ (opcode)
    (and (<= opcode (+ (* p #x1000) #x0ff0 q))
         (>= opcode (+ (* p #x1000) q)))))

;; (pred-sxtt) returns a predicate that checks if the opcode is of the form sxtt
;; where s and tt are constants

;; pred-sxtt: Nat Nat -> (Nat -> Bool)
(define (pred-sxtt s tt)
  (λ (opcode)
    (and (<= opcode (+ (* s #x1000) #x0f00 tt))
         (>= opcode (+ (* s #x1000) tt)))))