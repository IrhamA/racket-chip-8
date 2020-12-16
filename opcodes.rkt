#lang racket

(require "registers.rkt")

(provide (all-defined-out))

;;------------------------------------------------------------------------------

;; (opcode-0nnn ram registers nnn) calls machine code routine at address NNN

;; opcode-0nnn: Ram Registers -> Void
(define (opcode-0nnn ram registers)
  (void))

;;------------------------------------------------------------------------------

;; (opcode-00e0 ram registers) clears the screen

;; opcode-00e0: Ram Registers -> Void
(define (opcode-00e0 ram registers)
  (void)) ;; Remove this void when you write the function body

;;------------------------------------------------------------------------------

;; (opcode-00ee ram registers) returns from a subroutine

;; opcode-00ee: Ram Registers -> Void
(define (opcode-00ee ram registers)
  (void)) ;; Remove this void when you write the function body

;;------------------------------------------------------------------------------

;; (opcode-1nnn ram registers) jumps addess to NNN

;; opcode-1nnn: Ram Registers Word -> Ram Registers
(define (opcode-1nnn ram reg nnn)
  (struct-copy registers reg [pc nnn]))

;;------------------------------------------------------------------------------

;; (opcode-2nnn ram registers) calls subroutine at NNN

;; opcode-2nnn: Ram Registers -> Void
(define (opcode-2nnn ram registers)
  (void)) ;; Remove this void when you write the function body

;;------------------------------------------------------------------------------

;; (opcode-3xnn ram registers) skips the next instruction if VX equals NN
;; (Usually the next instruction is a jump to skip a code block)

;; opcode-3xnn: Ram Registers -> Void
(define (opcode-3xnn ram registers)
  (void)) ;; Remove this void when you write the function body

;;------------------------------------------------------------------------------

;; (opcode-4xnn ram registers) skips the next instruction if VX doesn't equal NN
;; (Usually the next instruction is a jump to skip a code block)

;; opcode-4xnn: Ram Registers -> Void
(define (opcode-4xnn ram registers)
  (void)) ;; Remove this void when you write the function body

;;------------------------------------------------------------------------------

;; (opcode-5xy0 ram registers) skips the next instruction if VX equals VY.
;; (Usually the next instruction is a jump to skip a code block)

;; opcode-5xy0: Ram Registers -> Void
(define (opcode-5xy0 ram registers)
  (void)) ;; Remove this void when you write the function body

;;------------------------------------------------------------------------------

;; (opcode-6xnn ram registers) sets VX to NN

;; opcode-6xnn: Ram Registers -> Void
(define (opcode-6xnn ram registers)
  (void)) ;; Remove this void when you write the function body

;;------------------------------------------------------------------------------

;; (opcode-7xnn ram registers) adds NN to VX (Carry Flag is not changed)

;; opcode-7xnn: Ram Registers -> Void
(define (opcode-7xnn ram registers)
  (void)) ;; Remove this void when you write the function body

;;------------------------------------------------------------------------------

;; (opcode-8xy0 ram registers) sets VX to the value of VY

;; opcode-8xy0: Ram Registers -> Void
(define (opcode-8xy0 ram registers)
  (void)) ;; Remove this void when you write the function body

;;------------------------------------------------------------------------------

;; (opcode-8xy1 ram registers) sets VX to VX or VY

;; opcode-8xy1: Ram Registers -> Void
(define (opcode-8xy1 ram registers)
  (void)) ;; Remove this void when you write the function body

;;------------------------------------------------------------------------------

;; (opcode-8xy2 ram registers) sets VX to VX and VY

;; opcode-8xy2: Ram Registers -> Void
(define (opcode-8xy2 ram registers)
  (void)) ;; Remove this void when you write the function body

;;------------------------------------------------------------------------------

;; (opcode-8xy3 ram registers) sets VX to VX xor VY

;; opcode-8xy3: Ram Registers -> Void
(define (opcode-8xy3 ram registers)
  (void)) ;; Remove this void when you write the function body

;;------------------------------------------------------------------------------

;; (opcode-8xy4 ram registers) adds VY to VX. VF is set to 1 when there's a
;; carry, and to 0 when there isn't.

;; opcode-8xy4: Ram Registers -> Void
(define (opcode-8xy4 ram registers)
  (void)) ;; Remove this void when you write the function body

;;------------------------------------------------------------------------------

;; (opcode-8XY5 ram registers) VY is subtracted from VX. VF is set to 0 when
;; there's a borrow, and 1 when there isn't.

;; opcode-8XY5: Ram Registers -> Void
(define (opcode-8XY5 ram registers)
  (void)) ;; Remove this void when you write the function body

;;------------------------------------------------------------------------------

;; (opcode-8XY6 ram registers) stores the least significant bit of VX in VF
;; and then shifts VX to the right by 1

;; opcode-8XY6: Ram Registers -> Void
(define (opcode-8XY6 ram registers)
  (void)) ;; Remove this void when you write the function body

;;------------------------------------------------------------------------------

;; (opcode-8XY7 ram registers) sets VX to VY minus VX. VF is set to 0 when
;; there's a borrow, and 1 when there isn't

;; opcode-8XY7: Ram Registers -> Void
(define (opcode-8XY7 ram registers)
  (void)) ;; Remove this void when you write the function body

;;------------------------------------------------------------------------------

;; (opcode-8xye ram registers) stores the most significant bit of VX in VF
;; and then shifts VX to the left by 1

;; opcode-8xyE: Ram Registers -> Void
(define (opcode-8xyE ram registers)
  (void)) ;; Remove this void when you write the function body

;;------------------------------------------------------------------------------

;; (opcode-9xy0 ram registers) skips the next instruction if VX doesn't
;; equal VY. (Usually the next instruction is a jump to skip a code block)

;; opcode-9xy0: Ram Registers -> Void
(define (opcode-9xy0 ram registers)
  (void)) ;; Remove this void when you write the function body

;;------------------------------------------------------------------------------

;; (opcode-annn ram registers) sets I to the address NNN

;; opcode-annn: Ram Registers -> Void
(define (opcode-annn ram registers)
  (void)) ;; Remove this void when you write the function body

;;------------------------------------------------------------------------------

;; (opcode-bnnn ram registers) Jumps to the address NNN plus V0 sets VX
;; to the result of a bitwise and operation on a random number
;; (Typically: 0 to 255) and NN

;; opcode-bnnn: Ram Registers -> Void
(define (opcode-bnnn ram registers)
  (void)) ;; Remove this void when you write the function body

;;------------------------------------------------------------------------------

;; (opcode-cxnn ram registers) sets VX to the result of a bitwise and
;; operation on a random number (Typically: 0 to 255) and NN

;; opcode-cxnn: Ram Registers -> Void
(define (opcode-cxnn ram registers)
  (void)) ;; Remove this void when you write the function body

;;------------------------------------------------------------------------------

;; (opcode-dxyn ram registers) Draws a sprite at coordinate (VX, VY) that
;; has a width of 8 pixels and a height of N+1 pixels. Each row of 8 pixels
;; is read as bit-coded starting from memory location I; I value doesn’t
;; change after the execution of this instruction. As described above, VF
;; is set to 1 if any screen pixels are flipped from set to unset when the
;; sprite is drawn, and to 0 if that doesn’t happen

;; opcode-dxyn: Ram Registers -> Void
(define (opcode-dxyn ram registers)
  (void)) ;; Remove this void when you write the function body

;;------------------------------------------------------------------------------

;; (opcode-ex9e ram registers) skips the next instruction if the key stored in
;; VX is pressed. (Usually the next instruction is a jump to skip a code block)

;; opcode-ex9e: Ram Registers -> Void
(define (opcode-ex9e ram registers)
  (void)) ;; Remove this void when you write the function body

;;------------------------------------------------------------------------------

;; (opcode-exa1 ram registers) skips the next instruction if the key stored in
;; VX isn't pressed. (Usually the next instruction is a jump to skip a code block)

;; opcode-exa1: Ram Registers -> Void
(define (opcode-exa1 ram registers)
  (void)) ;; Remove this void when you write the function body

;;------------------------------------------------------------------------------

;; (opcode-fx07 ram registers) sets VX to the value of the delay timer.

;; opcode-fx07: Ram Registers -> Void
(define (opcode-fx07 ram registers)
  (void)) ;; Remove this void when you write the function body

;;------------------------------------------------------------------------------

;; (opcode-fx0a ram registers) a key press is awaited, and then stored in VX.
;; (Blocking Operation. All instruction halted until next key event)

;; opcode-fx0a: Ram Registers -> Void
(define (opcode-fx0a ram registers)
  (void)) ;; Remove this void when you write the function body

;;------------------------------------------------------------------------------

;; (opcode-fx15 ram registers) sets the delay timer to VX

;; opcode-fx15: Ram Registers -> Void
(define (opcode-fx15 ram registers)
  (void)) ;; Remove this void when you write the function body

;;------------------------------------------------------------------------------

;; (opcode-fx18 ram registers) sets the sound timer to VX

;; opcode-fx18: Ram Registers -> Void
(define (opcode-fx18 ram registers)
  (void)) ;; Remove this void when you write the function body

;;------------------------------------------------------------------------------

;; (opcode-fx1e ram registers) adds VX to I. VF is not affected

;; opcode-fx1e: Ram Registers -> Void
(define (opcode-fx1e ram registers)
  (void)) ;; Remove this void when you write the function body

;;------------------------------------------------------------------------------

;; (opcode-fx29 ram registers) sets I to the location of the sprite for the
;; character in VX. Characters 0-F (in hexadecimal) are represented by a 4x5
;; font.

;; opcode-fx29: Ram Registers -> Void
(define (opcode-fx29 ram registers)
  (void)) ;; Remove this void when you write the function body

;;------------------------------------------------------------------------------

;; (opcode-fx33 ram registers) stores the binary-coded decimal representation
;; of VX, with the most significant of three digits at the address in I, the
;; middle digit at I plus 1, and the least significant digit at I plus 2.
;; (In other words, take the decimal representation of VX, place the hundreds
;; digit in memory at location in I, the tens digit at location I+1, and the
;; ones digit at location I+2.)

;; opcode-fx33: Ram Registers -> Void
(define (opcode-fx33 ram registers)
  (void)) ;; Remove this void when you write the function body

;;------------------------------------------------------------------------------

;; (opcode-fx55 ram registers) stores V0 to VX (including VX) in memory
;; starting at address I. The offset from I is increased by 1 for each
;; value written, but I itself is left unmodified

;; opcode-fx55: Ram Registers -> Void
(define (opcode-fx55 ram registers)
  (void)) ;; Remove this void when you write the function body

;;------------------------------------------------------------------------------

;; (opcode-fx65 ram registers) Fills V0 to VX (including VX) with values from
;; memory starting at address I. The offset from I is increased by 1 for each
;; value written, but I itself is left unmodified

;; opcode-fx65: Ram Registers -> Void
(define (opcode-fx65 ram registers)
  (void)) ;; Remove this void when you write the function body

;;------------------------------------------------------------------------------