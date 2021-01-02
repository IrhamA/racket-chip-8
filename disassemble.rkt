#lang racket

(require "registers.rkt")
(require "ram.rkt")

(provide (all-defined-out))

;; (disassembler a b) Takes two bytes (a and b) and returns
;; the corresponding string relating to (connect a b)

;; disassembler: Byte Byte -> Str
(define (disassembler a b)
  (let* ([connect (+ (* 256 a) b)])
    
    (cond [(equal? #x00e0 connect) "CLS"] ; 00e0
          [(equal? #x00ee connect) "RET"] ; 00ee
          [((pred-mnnn #x0) connect) (format "SYS ~a" (number->string connect)) ] ; 0nnn
          [((pred-mnnn #x1) connect) (format "JMP ~a" (number->string (bitwise-and connect #x0fff)) )] ; 1nnn 
          [((pred-mnnn #x2) connect) (format "CALL ~a" (number->string (bitwise-and connect #x0fff)))] ; 2nnn 
          [((pred-mnnn #x3) connect) (format  "SE V~a, ~a" (number->string (/ (bitwise-and connect #x0f00) #x0100)) (number->string (bitwise-and connect #x00ff)))] ; 3xnn  SE Vx, byte
          [((pred-mnnn #x4) connect) (format  "SNE V~a, ~a" (number->string (/ (bitwise-and connect #x0f00) #x0100)) (number->string (bitwise-and connect #x00ff)))] ; 4xnn SNE Vx, byte
          [((pred-pnnq #x5 #x0) connect) (format "SE ~a, ~a" (number->string (/ (bitwise-and connect #x0f00) #x0100)) (number->string (/ (bitwise-and connect #x00f0) #x0010)))] ; 5xy0
          [((pred-mnnn #x6) connect) (format "LD V~a, ~a" (number->string (/ (bitwise-and connect #x0f00) #x0100)) (number->string (bitwise-and connect #x00ff)))] ; 6xnn  LD Vx, byte
          [((pred-mnnn #x7) connect) (format "ADD V~a, ~a" (number->string (/ (bitwise-and connect #x0f00) #x0100)) (number->string (bitwise-and connect #x00ff)))] ; 7xnn ADD Vx, byte
          [((pred-pnnq #x8 #x0) connect)  (format "LD V~a, V~a" (number->string (/ (bitwise-and connect #x0f00) #x0100)) (number->string (/ (bitwise-and connect #x00f0) #x0010)))] ; 8xy0 LD Vx, Vy
          [((pred-pnnq #x8 #x1) connect) (format "OR V~a, V~a" (number->string (/ (bitwise-and connect #x0f00) #x0100)) (number->string (/ (bitwise-and connect #x00f0) #x0010)))] ; 8xy1 OR Vx, Vy
          [((pred-pnnq #x8 #x2) connect) (format "AND V~a, V~a" (number->string (/ (bitwise-and connect #x0f00) #x0100)) (number->string (/ (bitwise-and connect #x00f0) #x0010)))] ; 8xy2 AND Vx, Vy
          [((pred-pnnq #x8 #x3) connect) (format "XOR V~a, V~a" (number->string (/ (bitwise-and connect #x0f00) #x0100)) (number->string (/ (bitwise-and connect #x00f0) #x0010)))] ; 8xy3 XOR Vx, Vy
          [((pred-pnnq #x8 #x4) connect) (format "ADD V~a, V~a" (number->string (/ (bitwise-and connect #x0f00) #x0100)) (number->string (/ (bitwise-and connect #x00f0) #x0010)))] ; 8xy4 ADD Vx, Vy
          [((pred-pnnq #x8 #x5) connect) (format "SUB V~a, V~a" (number->string (/ (bitwise-and connect #x0f00) #x0100)) (number->string (/ (bitwise-and connect #x00f0) #x0010)))] ; 8xy5 SUB Vx, Vy
          [((pred-pnnq #x8 #x6) connect) (format "SHR V~a, V~a" (number->string (/ (bitwise-and connect #x0f00) #x0100)) (number->string (/ (bitwise-and connect #x00f0) #x0010)))] ; 8xy6 SHR Vx, Vy
          [((pred-pnnq #x8 #x7) connect) (format "SUBN V~a, V~a" (number->string (/ (bitwise-and connect #x0f00) #x0100)) (number->string (/ (bitwise-and connect #x00f0) #x0010)))] ; 8xy7 SUBN Vx, Vy
          [((pred-pnnq #x8 #xe) connect) (format "SHL V~a, V~a" (number->string (/ (bitwise-and connect #x0f00) #x0100)) (number->string (/ (bitwise-and connect #x00f0) #x0010)))] ; 8xye SHL, Vx, Vy
          [((pred-pnnq #x9 #x0) connect) (format "SNE V~a, V~a" (number->string (/ (bitwise-and connect #x0f00) #x0100)) (number->string (/ (bitwise-and connect #x00f0) #x0010)))] ; 9xy0 SNE, Vx, Vy
          [((pred-mnnn #xa) connect) (format "LD I, ~a" (number->string (bitwise-and connect #x0fff)))] ; annn LD I, addr
          [((pred-mnnn #xb) connect) (format "JMP V0, ~a" (number->string connect))] ; bnnn JP V0, addr
          [((pred-mnnn #xc) connect) (format "RND V~a, ~a" (number->string (/ (bitwise-and connect #x0f00) #x0100)) (number->string (bitwise-and connect #x00ff)))] ; cxnn RND Vx, byte
          [((pred-mnnn #xd) connect) (format "DRW V~a, V~a, ~a" (number->string (/ (bitwise-and connect #x0f00) #x0100)) (number->string (/ (bitwise-and connect #x00f0) #x0010)) (number->string (bitwise-and #x000f connect)))] ; dxyn DRW Vx, Vy, nibble
          [((pred-sxtt #xe #x9e) connect) (format "SKP V~a" (number->string (/ (bitwise-and connect #x0f00) #x0100)))] ; ex9e
          [((pred-sxtt #xe #xa1) connect) (format "SKNP V~a" (number->string (/ (bitwise-and connect #x0f00) #x0100)))] ; exa1
          [((pred-sxtt #xf #x07) connect) (format "LD V~a, DT" (number->string (/ (bitwise-and connect #x0f00) #x0100)))] ; fx07 LD Vx, DT
          [((pred-sxtt #xf #x0a) connect) (format "LD V~a, Key" (number->string (/ (bitwise-and connect #x0f00) #x0100)))] ; fx0a LD Vx, K
          [((pred-sxtt #xf #x15) connect) (format "LD DT, V~a" (number->string (/ (bitwise-and connect #x0f00) #x0100)))] ; fx15 LD DT, Vx
          [((pred-sxtt #xf #x18) connect) (format "LD ST, V~a" (number->string (/ (bitwise-and connect #x0f00) #x0100)))] ; fx18 LD ST, Vx
          [((pred-sxtt #xf #x1e) connect) (format "ADD I, V~a" (number->string (/ (bitwise-and connect #x0f00) #x0100)))] ; fx1e ADD I, Vx
          [((pred-sxtt #xf #x29) connect) (format "LD F, V~a" (number->string (/ (bitwise-and connect #x0f00) #x0100)))] ; fx29 LD F, Vx
          [((pred-sxtt #xf #x33) connect) (format "LD B, V~a" (number->string (/ (bitwise-and connect #x0f00) #x0100)))] ; fx33 LD B, Vx
          [((pred-sxtt #xf #x55) connect) (format "LD [I], V~a" (number->string (/ (bitwise-and connect #x0f00) #x0100)))] ; fx55 LD [I], Vx
          [((pred-sxtt #xf #x65) connect) (format "LD V~a, [I]" (number->string (/ (bitwise-and connect #x0f00) #x0100)))]))) ; fx65 LD Vx, [I]

;;------------------------------------------------------------------------------

;; (connect-d ram s e) takes ram, a starting point (s), and an ending
;; point (e), it then returns a string with all elements from s to e

;; connect-d: Ram Nat Nat -> String
(define (connect-d ram s e)
  (cond [(= s e) ""]
        [else (string-append (disassembler (vector-ref ram s)
                                           (vector-ref ram (add1 s)))
                             (connect-d ram (+ 2 s) e))]))

;;------------------------------------------------------------------------------

;; (pred-mnnn m) returns a predicate that checks if the opcode is of the form
;; mnnn where m is a constant

;; pred-mnnn: Nat -> (Nat -> Bool)
(define (pred-mnnn m)
  (λ (opcode)
    (equal? (quotient opcode #x1000) m)))

;;------------------------------------------------------------------------------

;; (pred-pnnq p q) returns a predicate that checks if the opcode is of the form
;; pnnq where p and q are constants

;; pred-pnnq: Nat Nat -> (Nat -> Bool)
(define (pred-pnnq p q)
  (λ (opcode)
    (and (equal? (quotient opcode #x1000) p)
         (equal? (bitwise-and opcode #x000f) q))))

;;------------------------------------------------------------------------------

;; (pred-sxtt) returns a predicate that checks if the opcode is of the form sxtt
;; where s and tt are constants

;; pred-sxtt: Nat Nat -> (Nat -> Bool)
(define (pred-sxtt s tt)
  (λ (opcode)
      (and (equal? (bitwise-and opcode #x00ff) tt)
           (equal? (quotient opcode #x1000) s))))

;;------------------------------------------------------------------------------