#lang racket

(require "disassemble.rkt")
(require "display.rkt")
(require "opcodes.rkt")
(require "ram.rkt")
(require "registers.rkt")
(require "util.rkt")

(provide (all-defined-out))

;;------------------------------------------------------------------------------

;; (cpu! ram reg disp input) simulates one clock cycle of the CHIP-8 cpu.
;; To-do: This is the worst thing I have ever seen or written in Racket

;; cpu!: Ram Registers Display Input -> Void
(define (cpu! ram reg disp input)
  (let* ([pc (registers-pc reg)]
         [opcode (+ (* 256 (ram-ref ram pc)) (ram-ref ram (add1 pc)))])
  (begin
    ;; (display (format "~a: ~a" (dec->hex opcode) (disassemble (ram-ref ram pc) (ram-ref ram (add1 pc)))))
    ;; (display #\newline)
    (if (zero? (registers-dt reg)) (void) (set-registers-dt! reg (sub1 (registers-dt reg))))
    (if (zero? (registers-st reg)) (void) (set-registers-st! reg (sub1 (registers-st reg))))
    (cond [(equal? #x00e0 opcode) (opcode-00e0 disp)] ; 00e0
          [(equal? #x00ee opcode) (opcode-00ee ram reg)] ; 00ee
          [((pred-mnnn #x0) opcode) (opcode-0nnn opcode)] ; 0nnn
          [((pred-mnnn #x1) opcode) (opcode-1nnn reg (modulo opcode #x1000))] ; 1nnn
          [((pred-mnnn #x2) opcode) (opcode-2nnn ram reg (modulo opcode #x1000))] ; 2nnn 
          [((pred-mnnn #x3) opcode) (opcode-3xnn reg (/ (bitwise-and opcode #x0f00) #x0100) (bitwise-and opcode #x00ff))] ; 3xnn
          [((pred-mnnn #x4) opcode) (opcode-4xnn reg (/ (bitwise-and opcode #x0f00) #x0100) (bitwise-and opcode #x00ff))] ; 4xnn
          [((pred-pnnq #x5 #x0) opcode) (opcode-5xy0 reg (/ (bitwise-and opcode #x0f00) #x0100) (/ (bitwise-and opcode #x00f0) #x0010))] ; 5xy0
          [((pred-mnnn #x6) opcode) (opcode-6xnn reg (/ (bitwise-and opcode #x0f00) #x0100) (bitwise-and opcode #x00ff))] ; 6xnn
          [((pred-mnnn #x7) opcode) (opcode-7xnn ram reg (/ (bitwise-and opcode #x0f00) #x0100) (bitwise-and opcode #x00ff))] ; 7xnn
          [((pred-pnnq #x8 #x0) opcode) (opcode-8xy0 reg (/ (bitwise-and opcode #x0f00) #x0100) (/ (bitwise-and opcode #x00f0) #x0010))] ; 8xy0
          [((pred-pnnq #x8 #x1) opcode) (opcode-8xy1 reg (/ (bitwise-and opcode #x0f00) #x0100) (/ (bitwise-and opcode #x00f0) #x0010))] ; 8xy1
          [((pred-pnnq #x8 #x2) opcode) (opcode-8xy2 reg (/ (bitwise-and opcode #x0f00) #x0100) (/ (bitwise-and opcode #x00f0) #x0010))] ; 8xy2
          [((pred-pnnq #x8 #x3) opcode) (opcode-8xy3 reg (/ (bitwise-and opcode #x0f00) #x0100) (/ (bitwise-and opcode #x00f0) #x0010))] ; 8xy3
          [((pred-pnnq #x8 #x4) opcode) (opcode-8xy4 reg (/ (bitwise-and opcode #x0f00) #x0100) (/ (bitwise-and opcode #x00f0) #x0010))] ; 8xy4
          [((pred-pnnq #x8 #x5) opcode) (opcode-8xy5 reg (/ (bitwise-and opcode #x0f00) #x0100) (/ (bitwise-and opcode #x00f0) #x0010))] ; 8xy5
          [((pred-pnnq #x8 #x6) opcode) (opcode-8xy6 reg (/ (bitwise-and opcode #x0f00) #x0100) (/ (bitwise-and opcode #x00f0) #x0010))] ; 8xy6
          [((pred-pnnq #x8 #x7) opcode) (opcode-8xy7 reg (/ (bitwise-and opcode #x0f00) #x0100) (/ (bitwise-and opcode #x00f0) #x0010))] ; 8xy7
          [((pred-pnnq #x8 #xe) opcode) (opcode-8xye reg (/ (bitwise-and opcode #x0f00) #x0100) (/ (bitwise-and opcode #x00f0) #x0010))] ; 8xye
          [((pred-pnnq #x9 #x0) opcode) (opcode-9xy0 reg (/ (bitwise-and opcode #x0f00) #x0100) (/ (bitwise-and opcode #x00f0) #x0010))] ; 9xy0
          [((pred-mnnn #xa) opcode) (opcode-annn reg (modulo opcode #xa000))] ; annn
          [((pred-mnnn #xb) opcode) (opcode-bnnn reg (modulo opcode #xb000))] ; bnnn
          [((pred-mnnn #xc) opcode) (opcode-cxnn reg (/ (bitwise-and opcode #x0f00) #x0100) (bitwise-and opcode #x00ff))] ; cxnn
          [((pred-mnnn #xd) opcode) (opcode-dxyn ram reg (/ (bitwise-and opcode #x0f00) #x0100) (/ (bitwise-and opcode #x00f0) #x0010) (bitwise-and opcode #x000f) disp)] ; dxyn
          [((pred-sxtt #xe #x9e) opcode) (opcode-ex9e ram reg (/ (bitwise-and opcode #x0f00) #x0100))] ; ex9e
          [((pred-sxtt #xe #xa1) opcode) (opcode-exa1 ram reg (/ (bitwise-and opcode #x0f00) #x0100))] ; exa1
          [((pred-sxtt #xf #x07) opcode) (opcode-fx07 reg (/ (bitwise-and opcode #x0f00) #x0100))] ; fx07
          [((pred-sxtt #xf #x0a) opcode) (opcode-fx0a ram reg (/ (bitwise-and opcode #x0f00) #x0100))] ; fx0a
          [((pred-sxtt #xf #x15) opcode) (opcode-fx15 reg (/ (bitwise-and opcode #x0f00) #x0100))] ; fx15
          [((pred-sxtt #xf #x18) opcode) (opcode-fx18 reg (/ (bitwise-and opcode #x0f00) #x0100))] ; fx18
          [((pred-sxtt #xf #x1e) opcode) (opcode-fx1e reg (/ (bitwise-and opcode #x0f00) #x0100))] ; fx1e
          [((pred-sxtt #xf #x29) opcode) (opcode-fx29 reg (/ (bitwise-and opcode #x0f00) #x0100))] ; fx29
          [((pred-sxtt #xf #x33) opcode) (opcode-fx33 ram reg (/ (bitwise-and opcode #x0f00) #x0100))] ; fx33
          [((pred-sxtt #xf #x55) opcode) (opcode-fx55 ram reg (/ (bitwise-and opcode #x0f00) #x0100))] ; fx55
          [((pred-sxtt #xf #x65) opcode) (opcode-fx65 ram reg (/ (bitwise-and opcode #x0f00) #x0100))]) ; fx65
    (set-registers-pc! reg (+ 2 (registers-pc reg))))))

;;------------------------------------------------------------------------------

;; (init-cpu! ram reg disp input) sets up the initial values for the components.

;; init-cpu!: Ram Registers Display Input -> Void
(define (init-cpu! ram reg disp input)
  (letrec ([init-font! (λ (index)
             (if (equal? index #x10) (void)
             (begin (ram-set-list! ram (* index 8) (vector-ref chars-new index))
                    (init-font! (add1 index)))))])
  (init-font! 0)))

;;------------------------------------------------------------------------------