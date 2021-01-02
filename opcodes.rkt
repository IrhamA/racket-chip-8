#lang racket

(require "display.rkt")
(require "registers.rkt")
(require "ram.rkt")
(require "util.rkt")

(provide (all-defined-out))

;;------------------------------------------------------------------------------

;; (opcode-0nnn ram registers nnn) calls machine code routine at address nnn.

;; opcode-0nnn: Ram Registers -> Void
(define (opcode-0nnn ram registers nnn)
  (begin (display "Jump to invalid address: ") (display nnn)))

;;------------------------------------------------------------------------------

;; (opcode-00e0 ram registers disp) clears the screen.

;; opcode-00e0: Ram Registers Display -> Void
(define (opcode-00e0 ram reg disp)
  (display-clear! disp))

;;------------------------------------------------------------------------------

;; (opcode-00ee ram registers) returns from a subroutine: Sets program counter
;; to the address at the top of the stack, then decrements from stack pointer.

;; opcode-00ee: Ram Registers -> Void
(define (opcode-00ee ram reg)
  (begin (set-registers-sp! reg (- (registers-sp reg) 2))
         (set-registers-pc! reg (+ (* (ram-ref ram (registers-sp reg)) #x100)
                                   (ram-ref ram (add1 (registers-sp reg)))))))

;;------------------------------------------------------------------------------

;; (opcode-1nnn ram registers nnn) jumps addess to nnn: Sets program counter to
;; nnn.

;; opcode-1nnn: Ram Registers Word -> Void
(define (opcode-1nnn ram reg nnn)
  (set-registers-pc! reg nnn))

;;------------------------------------------------------------------------------

;; (opcode-2nnn ram registers nnn) calls subroutine at nnn: Increments the stack
;; pointer, then pushes the current pc to the top of the stack. The pc is then
;; set to nnn.

;; opcode-2nnn: Ram Registers -> Void
(define (opcode-2nnn ram reg nnn)
  (begin (ram-set! ram (registers-sp reg)
           (quotient (bitwise-and (registers-pc reg) #xff00) #x100))
         (ram-set! ram (add1 (registers-sp reg))
           (bitwise-and (registers-pc reg) #xff))
         (set-registers-sp! reg (+ 2 (registers-sp reg)))
         (set-registers-pc! reg nnn)))

;;------------------------------------------------------------------------------

;; (opcode-3xnn ram registers x nn) skips the next instruction if Vx equals nn.
;; Usually the next instruction is a jump to skip a code block. The interpreter
;; compares register Vx to kk, and if they are equal, increments the program
;; counter by 2.

;; opcode-3xnn: Ram Registers Byte Byte -> Void
(define (opcode-3xnn ram reg x nn)
  (if (equal? (registers-vn reg x) nn)
      (set-registers-pc! reg (+ 2 (registers-pc reg)))
      (void)))

;;------------------------------------------------------------------------------

;; (opcode-4xnn ram registers x nn) skips the next instruction if Vx doesn't
;; equal nn.

;; opcode-4xnn: Ram Registers Byte Byte -> Void
(define (opcode-4xnn ram reg x nn)
  (if (equal? (registers-vn reg x) nn) (void)
      (set-registers-pc! reg (+ 2 (registers-pc reg)))))

;;------------------------------------------------------------------------------

;; (opcode-5xy0 ram registers x y) skips the next instruction if Vx equals Vy.
;; The interpreter compares register Vx to register Vy, and if they are equal,
;; increments the program counter by 2.

;; opcode-5xy0: Ram Registers Byte Byte -> Void
(define (opcode-5xy0 ram reg x y)
  (if (equal? (registers-vn reg x) (registers-vn reg y))
      (set-registers-pc! reg (+ 2 (registers-pc reg)))
      (void)))

;;------------------------------------------------------------------------------

;; (opcode-6xnn ram registers x nn) sets Vx to nn

;; opcode-6xnn: Ram Registers Byte Byte -> Void
(define (opcode-6xnn ram reg x nn)
  (set-registers-vn! reg x nn))

;;------------------------------------------------------------------------------

;; (opcode-7xnn ram registers x nn) adds nn to Vx. The Carry Flag (Vf) is not
;; changed. Adds the value kk to the value of register Vx, then stores the
;; result in Vx.

;; opcode-7xnn: Ram Registers Byte Byte -> Void
(define (opcode-7xnn ram reg x nn)
  (set-registers-vn! reg x (+ (registers-vn reg x) nn)))

;;------------------------------------------------------------------------------

;; (opcode-8xy0 ram registers x y) sets Vx to the value of Vy. The interpreter
;; stores the value of register Vy in register Vx.

;; opcode-8xy0: Ram Registers Byte Byte -> Void
(define (opcode-8xy0 ram reg x y)
  (set-registers-vn! reg x (registers-vn reg y)))

;;------------------------------------------------------------------------------

;; (opcode-8xy1 ram registers x y) sets Vx to Vx OR Vy. Performs a bitwise OR
;; using Vx and Vy, then stores the result in Vx. A bitwise OR compares the
;; corresponding bits from two numbers, and if either bit is 1, then the same
;; bit in the result is also 1. Otherwise, it is 0.

;; opcode-8xy1: Ram Registers Byte Byte -> Void
(define (opcode-8xy1 ram reg x y)
  (set-registers-vn! reg x (bitwise-ior (registers-vn reg x)
                                        (registers-vn reg y))))

;;------------------------------------------------------------------------------

;; (opcode-8xy2 ram registers x y) sets Vx to Vx AND Vy: Performs a bitwise AND
;; on Vx and Vy, using a similar method to bitwise OR but using AND instead

;; opcode-8xy2: Ram Registers Byte Byte -> Void
(define (opcode-8xy2 ram reg x y)
  (set-registers-vn! reg x (bitwise-and (registers-vn reg x)
                                        (registers-vn reg y))))

;;------------------------------------------------------------------------------

;; (opcode-8xy3 ram registers x y) sets Vx to Vx XOR Vy. Performs a bitwise XOR
;; on Vx and Vy. Again, this performs XOR on each bit in Vx with each bit in Vy.

;; opcode-8xy3: Ram Registers Byte Byte -> Void
(define (opcode-8xy3 ram reg x y)
  (set-registers-vn! reg x (bitwise-xor (registers-vn reg x)
                                        (registers-vn reg y))))

;;------------------------------------------------------------------------------

;; (opcode-8xy4 ram registers x y) adds Vy to Vx. Vf is set to 1 when any bits
;; must be carried, and to 0 otherwise.
;; To-do: Is the number supposed to be capped at 255?

;; opcode-8xy4: Ram Registers Byte Byte -> Void
(define (opcode-8xy4 ram reg x y)
  (let ([x+y (+ (registers-vn x) (registers-vn y))])
    (if (> x+y 255)
        (begin (set-registers-vn! reg x 255)
               (set-registers-vn! reg #xf 1))
        (begin (set-registers-vn! reg x x+y)
               (set-registers-vn! reg #xf 0)))))

;;------------------------------------------------------------------------------

;; (opcode-8xy5 ram registers x y) Setx Vx to Vx minus Vy. Vf is set to 0 when
;; there's a borrow, and 1 when there isn't.
;; To-do: Is the number supposed to be floored at 0?

;; opcode-8xy5: Ram Registers Byte Byte -> Void
(define (opcode-8xy5 ram reg x y)
  (let ([x-y (- (registers-vn x) (registers-vn y))])
    (if (> (registers-vn y) (registers-vn x))
       (begin (set-registers-vn! reg x 0)
              (set-registers-vn! reg #xf 1))
       (begin (set-registers-vn! reg x x-y)
              (set-registers-vn! reg #xf 0)))))

;;------------------------------------------------------------------------------

;; (opcode-8xy6 ram registers x y) stores the least significant bit of Vx in Vf
;; and then shifts Vx to the right by 1
;; To-do: y is unused

;; opcode-8xy6: Ram Registers Byte Byte -> Void
(define (opcode-8xy6 ram reg x y)
  (begin (if (odd? (registers-vn reg x))
             (set-registers-vn! reg #xf 1)
             (set-registers-vn! reg #xf 0))
         (set-registers-vn! reg x (/ (registers-vn reg x) 2))))
                   
;;------------------------------------------------------------------------------

;; (opcode-8xy7 ram registers x y) sets Vx to Vy minus Vx. Vf is set to 0 when
;; there's a borrow, and 1 when there isn't.

;; opcode-8xy7: Ram Registers Byte Byte -> Void
(define (opcode-8xy7 ram reg x y)
  (let ([y-x (- (registers-vn y) (registers-vn x))])
    (if (> (registers-vn x) (registers-vn y))
       (begin (set-registers-vn! reg x 0)
              (set-registers-vn! reg #xf 1))
       (begin (set-registers-vn! reg x y-x)
              (set-registers-vn! reg #xf 0)))))

;;------------------------------------------------------------------------------

;; (opcode-8xye ram registers x y) stores the most significant bit of Vx in Vf
;; and then shifts VX to the left by 1
;; To-do: y is unused

;; opcode-8xye: Ram Registers Byte Byte -> Void
(define (opcode-8xye ram reg x y)
  (begin (if (zero? (bitwise-and #b100000000 (registers-vn reg x)))
             (set-registers-vn! reg #xf 0)
             (set-registers-vn! reg #xf 1))
         (set-registers-vn! reg x (* (registers-vn reg x) 2))))

;;------------------------------------------------------------------------------

;; (opcode-9xy0 ram registers x y) skips the next instruction if Vx doesn't
;; equal Vy. Again, the next instruction is often a jump to skip a code block.

;; opcode-9xy0: Ram Registers Byte Byte -> Void
(define (opcode-9xy0 ram reg x y)
  (if (equal? (registers-vn reg y) (registers-vn reg x))
      (void)
      (set-registers-pc! reg (+ 2 (registers-pc reg)))))

;;------------------------------------------------------------------------------

;; (opcode-annn ram registers nnn) sets the index register i to the address nnn

;; opcode-annn: Ram Registers Word -> Void
(define (opcode-annn ram reg nnn)
  (set-registers-i! reg nnn))
   
;;------------------------------------------------------------------------------

;; (opcode-bnnn ram registers nnn) Jumps to the address nnn plus V0.

;; opcode-bnnn: Ram Registers -> Void
(define (opcode-bnnn ram reg nnn)
  (set-registers-pc! reg (+ (registers-vn reg #x0) nnn)))

;;------------------------------------------------------------------------------

;; (opcode-cxnn ram registers x nn) sets Vx to the result of an AND operation on
;; a random number (0 to 255 AND nn)

;; opcode-cxnn: Ram Registers Byte -> Void
(define (opcode-cxnn ram reg x nn)
  (set-registers-vn! reg x (bitwise-and (random 0 255) nn)))

;;------------------------------------------------------------------------------

;; (opcode-dxyn ram registers x y n disp) Draws a sprite at coordinate (Vx, Vy)
;; that has a width of 8 pixels and a height of n+1 pixels. Each row of 8 pixels
;; is read as bit-coded starting from memory location i; i's value doesn’t
;; change after the execution of this instruction. As described above, Vf is set
;; to 1 if any screen pixels are flipped from set to unset when the sprite is
;; drawn, and to 0 if that doesn’t happen.

;; opcode-dxyn: Ram Registers Byte Byte Byte Display -> Void
(define (opcode-dxyn ram reg x y n disp)
  (let* ([index (modulo (+ x (* y display-width)) 8)]
         [data (ram-ref ram (registers-i reg))]
         [byte1 (bitwise-shift-right data (modulo x 8))]
         [byte2 (bitwise-shift-left data (modulo x 8))])
  (cond [(= n 0) (void)]
        [else (begin (xor-display-byte! disp index byte1)
                     (xor-display-byte! disp (add1 index) byte2)
                     (opcode-dxyn ram reg x (add1 y) (sub1 n) disp))])))

;;------------------------------------------------------------------------------

;; (opcode-ex9e ram registers) skips the next instruction if the key stored in
;; Vx is pressed. Usually the next instruction is a jump to skip a code block.

;; opcode-ex9e: Ram Registers -> Void
(define (opcode-ex9e ram registers x)
  (void)) ;; Remove this void when you write the function body

;;------------------------------------------------------------------------------

;; (opcode-exa1 ram registers) skips the next instruction if the key stored in
;; Vx isn't pressed. Usually the next instruction is a jump to skip a code block.

;; opcode-exa1: Ram Registers -> Ram Registers
(define (opcode-exa1 ram reg x)
  (void)) ;; Remove this void when you write the function body

;;------------------------------------------------------------------------------

;; (opcode-fx07 ram registers x) sets Vx to the value of the delay timer.

;; opcode-fx07: Ram Registers Byte -> Void
(define (opcode-fx07 ram reg x)
  (set-registers-vn! reg x (registers-dt reg)))

;;------------------------------------------------------------------------------

;; (opcode-fx0a ram registers) a key press is awaited, and then stored in Vx.
;; (Blocking Operation. p next key event)

;; opcode-fx0a: Ram Registers Byte -> Ram Registers
(define (opcode-fx0a ram reg x)
  (void)) ;; Remove this void when you write the function body

;;------------------------------------------------------------------------------

;; (opcode-fx15 ram registers x) sets the delay timer register to Vx.

;; opcode-fx15: Ram Registers Byte -> Void
(define (opcode-fx15 ram reg x)
  (set-registers-dt! reg (registers-vn reg x)))

;;------------------------------------------------------------------------------

;; (opcode-fx18 ram registers) sets the sound timer register to Vx.

;; opcode-fx18: Ram Registers Byte -> Ram Registers
(define (opcode-fx18 ram reg x)
  (set-registers-st! reg (registers-vn reg x)))

;;------------------------------------------------------------------------------

;; (opcode-fx1e ram registers) adds Vx to i. Carry Flag (Vf) is not affected.
;; To-do: Should this number be clamped?

;; opcode-fx1e: Ram Registers Byte -> Ram Registers
(define (opcode-fx1e ram reg x)
  (set-registers-i! reg (+ (registers-vn reg x) (registers-i reg))))

;;------------------------------------------------------------------------------

;; (opcode-fx29 ram registers x) sets i to the location of the sprite for the
;; character in Vx. Characters 0-f (in hexadecimal) are represented by a 4x5
;; font.

;; opcode-fx29: Ram Registers -> Void
(define (opcode-fx29 ram reg x)
  (void)) ;; Remove this void when you write the function body

;;------------------------------------------------------------------------------

;; (opcode-fx33 ram registers x) stores the binary-coded decimal representation
;; of Vx, with the most significant of three digits at the address in i, the
;; middle digit at i plus 1, and the least significant digit at i plus 2.
;; (In other words, take the decimal representation of Vx, place the hundreds
;; digit in memory at location in i, the tens digit at location i+1, and the
;; ones digit at location i+2.)

;; opcode-fx33: Ram Registers -> Void
(define (opcode-fx33 ram reg x)
  (begin (ram-set! ram (registers-i reg) (quotient (registers-vn reg x) 100))
         (ram-set! ram (+ 1 (registers-i reg)) (quotient (modulo (registers-vn reg x) 100) 10))
         (ram-set! ram (+ 2 (registers-i reg)) (modulo (registers-vn reg x) 10))))

;;------------------------------------------------------------------------------

;; (opcode-fx55 ram registers x) stores V0 to Vx (including Vx) in memory
;; starting at address i. The offset from i is increased by 1 for each
;; value written, but i itself is left unmodified

;; opcode-fx55: Ram Registers -> Void
(define (opcode-fx55 ram reg x)
  (local [(define (redefine i)
            (cond [(= i x) (void)]
                  [else (begin
                          (ram-set! ram i (registers-vn reg
                                                        (+ (registers-i reg)
                                                               i)))
                          (redefine (add1 i)))]))]
    (redefine 0)))

;;------------------------------------------------------------------------------

;; (opcode-fx65 ram registers x) Fills V0 to Vx (including Vx) with list from
;; memory starting at address i. The offset from i is increased by 1 for each
;; value written, but i itself is left unmodified

;; opcode-fx65: Ram Registers -> Void
(define (opcode-fx65 ram reg x)
  (local [(define (redefine i)
            (cond [(= i x) (void)]
                  [else (begin
                          (set-registers-vn! reg i (ram-ref ram
                                                            (+ (registers-i reg)
                                                                   i)))
                          (redefine (add1 i)))]))]
    (redefine 0)))

;;------------------------------------------------------------------------------