#lang racket

(require "registers.rkt")
(require "ram.rkt")
(require "arch.rkt")

(provide (all-defined-out))

;; (disassembler a b) Takes two bytes (a and b) and returns
;; the corresponding string relating to (connect a b)

;; disassembler: Byte Byte -> Str
(define (disassembler a b)
  (let* ([connect (+ (* 256 a) b)])
    
    (cond [(equal? #x00e0 connect) "CLS"] ; 00e0
          [(equal? #x00ee connect) "RET"] ; 00ee
          [((pred-mnnn #x0) connect) (string-append "SYS" (number->string connect)) ] ; 0nnn
          [((pred-mnnn #x1) connect) (string-append "JP" (number->string (bitwise-and connect #x0fff)) )] ; 1nnn 
          [((pred-mnnn #x2) connect) (string-append "CALL" (number->string (bitwise-and connect #x0fff)))] ; 2nnn 
          [((pred-mnnn #x3) connect) (string-append  "SE" (number->string (/ (bitwise-and connect #x0f00) #x0100)) (number->string (bitwise-and connect #x00ff)))] ; 3xnn  SE Vx, byte
          [((pred-mnnn #x4) connect) (string-append  "SNE" (number->string (/ (bitwise-and connect #x0f00) #x0100)) (number->string (bitwise-and connect #x00ff)))] ; 4xnn SNE Vx, byte
          [((pred-pnnq #x5 #x0) connect) (string-append "SE" (number->string (/ (bitwise-and connect #x0f00) #x0100)) (number->string (/ (bitwise-and connect #x00f0) #x0010)))] ; 5xy0
          [((pred-mnnn #x6) connect) (string-append "LD" (number->string (/ (bitwise-and connect #x0f00) #x0100)) (number->string (bitwise-and connect #x00ff)))] ; 6xnn  LD Vx, byte
          [((pred-mnnn #x7) connect) (string-append "LD" (number->string (/ (bitwise-and connect #x0f00) #x0100)) (number->string (bitwise-and connect #x00ff)))] ; 7xnn ADD Vx, byte
          [((pred-pnnq #x8 #x0) connect)  (string-append "LD" (number->string (/ (bitwise-and connect #x0f00) #x0100)) (number->string (/ (bitwise-and connect #x00f0) #x0010)))] ; 8xy0 LD Vx, Vy
          [((pred-pnnq #x8 #x1) connect) (string-append "OR" (number->string (/ (bitwise-and connect #x0f00) #x0100)) (number->string (/ (bitwise-and connect #x00f0) #x0010)))] ; 8xy1 OR Vx, Vy
          [((pred-pnnq #x8 #x2) connect) (string-append "AND" (number->string (/ (bitwise-and connect #x0f00) #x0100)) (number->string (/ (bitwise-and connect #x00f0) #x0010)))] ; 8xy2 AND Vx, Vy
          [((pred-pnnq #x8 #x3) connect) (string-append "XOR" (number->string (/ (bitwise-and connect #x0f00) #x0100)) (number->string (/ (bitwise-and connect #x00f0) #x0010)))] ; 8xy3 XOR Vx, Vy
          [((pred-pnnq #x8 #x4) connect) (string-append "ADD" (number->string (/ (bitwise-and connect #x0f00) #x0100)) (number->string (/ (bitwise-and connect #x00f0) #x0010)))] ; 8xy4 ADD Vx, Vy
          [((pred-pnnq #x8 #x5) connect) (string-append "SUB" (number->string (/ (bitwise-and connect #x0f00) #x0100)) (number->string (/ (bitwise-and connect #x00f0) #x0010)))] ; 8xy5 SUB Vx, Vy
          [((pred-pnnq #x8 #x6) connect) (string-append "SHR" (number->string (/ (bitwise-and connect #x0f00) #x0100)) (number->string (/ (bitwise-and connect #x00f0) #x0010)))] ; 8xy6 SHR Vx, Vy
          [((pred-pnnq #x8 #x7) connect) (string-append "SUBN" (number->string (/ (bitwise-and connect #x0f00) #x0100)) (number->string (/ (bitwise-and connect #x00f0) #x0010)))] ; 8xy7 SUBN Vx, Vy
          [((pred-pnnq #x8 #xe) connect) (string-append "SHL" (number->string (/ (bitwise-and connect #x0f00) #x0100)) (number->string (/ (bitwise-and connect #x00f0) #x0010)))] ; 8xye SHL, Vx, Vy
          [((pred-pnnq #x9 #x0) connect) (string-append "SNE" (number->string (/ (bitwise-and connect #x0f00) #x0100)) (number->string (/ (bitwise-and connect #x00f0) #x0010)))] ; 9xy0 SNE, Vx, Vy
          [((pred-mnnn #xa) connect) (string-append "LD" (number->string connect))] ; annn LD I, addr
          [((pred-mnnn #xb) connect) (string-append "JP V0" (number->string connect))] ; bnnn JP V0, addr
          [((pred-mnnn #xc) connect) (string-append "RND" (number->string (/ (bitwise-and connect #x0f00) #x0100)) (number->string (bitwise-and connect #x00ff)))] ; cxnn RND Vx, byte
          [((pred-mnnn #xd) connect) (string-append "DRW" (number->string (/ (bitwise-and connect #x0f00) #x0100)) (number->string (/ (bitwise-and connect #x00f0) #x0010)) (number->string (bitwise-and #x000f connect))) ] ; dxyn DRW Vx, Vy, nibble
          [((pred-sxtt #xe #x9e) connect) (string-append "SKP" (number->string (/ (bitwise-and connect #x0f00) #x0100)))] ; ex9e
          [((pred-sxtt #xe #xa1) connect) (string-append "SKNP" (number->string (/ (bitwise-and connect #x0f00) #x0100)))] ; exa1
          [((pred-sxtt #xf #x07) connect) (string-append "LD" (number->string (/ (bitwise-and connect #x0f00) #x0100)) "DT")] ; fx07 LD Vx, DT
          [((pred-sxtt #xf #x0a) connect) (string-append "LD" (number->string (/ (bitwise-and connect #x0f00) #x0100)) "K")] ; fx0a LD Vx, K
          [((pred-sxtt #xf #x15) connect) (string-append "LD DT" (number->string (/ (bitwise-and connect #x0f00) #x0100)))] ; fx15 LD DT, Vx
          [((pred-sxtt #xf #x18) connect) (string-append "LD ST" (number->string (/ (bitwise-and connect #x0f00) #x0100)))] ; fx18 LD ST, Vx
          [((pred-sxtt #xf #x1e) connect) (string-append "ADD I" (number->string (/ (bitwise-and connect #x0f00) #x0100)))] ; fx1e ADD I, VX
          [((pred-sxtt #xf #x29) connect) (string-append "LD F" (number->string (/ (bitwise-and connect #x0f00) #x0100)))] ; fx29 LD F, Vx
          [((pred-sxtt #xf #x33) connect) (string-append "LD B" (number->string (/ (bitwise-and connect #x0f00) #x0100)))] ; fx33 LD B, Vx
          [((pred-sxtt #xf #x55) connect) (string-append "LD [I]" (number->string (/ (bitwise-and connect #x0f00) #x0100)))] ; fx55 LD [I], Vx
          [((pred-sxtt #xf #x65) connect) (string-append "LD" (number->string (/ (bitwise-and connect #x0f00) #x0100)) "[I]")]))) ; fx65 LD Vx, [I]

;; (connect-d ram s e) takes ram, a starting point (s), and an ending
;; point (e), it then returns a string with all elements from s to e
;; connect-d: Ram Nat Nat -> String
(define (connect-d ram s e)
  (cond [(= s e) ""]
        [else (string-append (disassembler (vector-ref ram s)
                                           (vector-ref ram (add1 s)))
                             (connect-d ram (+ 2 s) e))]))
