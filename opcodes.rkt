#lang racket

(require "registers.rkt")
(require "ram.rkt")

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

;; (opcode-00ee ram registers) returns from a subroutine: Sets program counter
;; to the address at the top of the stack, then sub1 from stack pointer

;; opcode-00ee: Ram Registers -> Ram Registers
(define (opcode-00ee ram reg)
  (values ram
          (struct-copy registers reg [pc (ram-ref ram (registers-sp reg))]
                                     [sp (sub1 (registers-sp reg))])))

;;------------------------------------------------------------------------------

;; (opcode-1nnn ram registers) jumps addess to NNN: Set program counter to NNN

;; opcode-1nnn: Ram Registers Word -> Ram Registers
(define (opcode-1nnn ram reg inc nnn)
  (values ram (struct-copy registers reg [pc nnn])))

;;------------------------------------------------------------------------------

;; (opcode-2nnn ram registers) calls subroutine at NNN: Increments the stack
;; pointer, then puts the current PC on the top of the stack. The PC is then
;; set to nnn.

;; opcode-2nnn: Ram Registers -> Ram Registers
(define (opcode-2nnn ram reg nnn)
  (values (ram-set ram (registers-sp reg) (registers-pc reg))
          (struct-copy registers reg [sp (add1 (registers-sp reg))]
                                     [pc nnn])))

;;------------------------------------------------------------------------------

;; (opcode-3xnn ram registers) skips the next instruction if VX equals NN
;; (Usually the next instruction is a jump to skip a code block)
;; Skip next instruction if Vx = kk. The interpreter compares register Vx to
;; kk, and if they are equal, increments the program counter by 2

;; opcode-3xnn: Ram Registers Byte Byte -> Void
(define (opcode-3xnn ram reg x nn)
  (if (equal? (vector-ref (registers-v reg) x) nn)
      (struct-copy registers reg [pc (+ 2 (registers-pc reg))])      
      (values ram reg)))

;;------------------------------------------------------------------------------

;; (opcode-4xnn ram registers) skips the next instruction if VX doesn't equal NN
;; (Usually the next instruction is a jump to skip a code block)

;; opcode-4xnn: Ram Registers Byte Byte -> Void
(define (opcode-4xnn ram reg x nn)
  (if (not (equal? (vector-ref (registers-v reg) x) nn))
      (struct-copy registers reg [pc (+ 2 (registers-pc reg))])     
      (values ram reg)))

;;------------------------------------------------------------------------------

;; (opcode-5xy0 ram registers) skips the next instruction if VX equals VY.
;; (Usually the next instruction is a jump to skip a code block)
;; Skip next instruction if Vx = Vy. The interpreter compares register Vx to
;; register Vy, and if they are equal, increments the program counter by 2.

;; opcode-5xy0: Ram Registers Byte Byte -> Ram Registers
(define (opcode-5xy0 ram reg x y)
  (if (equal? (vector-ref (registers-v reg) x) (vector-ref (registers-v reg) y))
      (struct-copy registers reg [pc (+ 2 (registers-pc reg))])      
      (values ram reg)))

;;------------------------------------------------------------------------------

;; (opcode-6xnn ram registers) sets VX to NN

;; opcode-6xnn: Ram Registers Byte Byte -> Ram Registers
(define (opcode-6xnn ram reg x nn)
  (values ram (registers-vn-set reg x nn)))

;;------------------------------------------------------------------------------

;; (opcode-7xnn ram registers) adds NN to VX (Carry Flag is not changed)
;; Set Vx = Vx + kk. Adds the value kk to the value of register Vx,
;; then stores the result in Vx.

;; opcode-7xnn: Ram Registers Byte Byte -> Ram Registers 
(define (opcode-7xnn ram reg x nn)
  (values ram (struct-copy registers reg
                           [v (registers-vn-set reg x (+ x nn))])))

;;------------------------------------------------------------------------------

;; (opcode-8xy0 ram registers) sets VX to the value of VY
;; Set Vx = Vy. Stores the value of register Vy in register Vx.

;; opcode-8xy0: Ram Registers Byte Byte -> Ram Registers
(define (opcode-8xy0 ram reg x y)
  (values ram (struct-copy registers reg
                           [v (registers-vn-set reg x (registers-vn reg y))])))

;;------------------------------------------------------------------------------

;; (opcode-8xy1 ram registers) sets VX to VX or VY
;; Set Vx = Vx OR Vy. Performs a bitwise OR on the values of Vx and Vy,
;; then stores the result in Vx. A bitwise OR compares the corresponding bits
;; from two values, and if either bit is 1, then the same bit in the result is
;; also 1. Otherwise, it is 0.

;; opcode-8xy1: Ram Registers Byte Byte -> Ram Registers
(define (opcode-8xy1 ram reg x y)
  (values ram
          (struct-copy registers reg
                       [v (registers-vn-set reg x
                               (bitwise-ior (registers-vn reg x)
                                            (registers-vn reg y)))])))

;;------------------------------------------------------------------------------

;; (opcode-8xy2 ram registers) sets VX to VX and VY: bitwise-and this

;; opcode-8xy2: Ram Registers Byte Byte -> Ram Registers
(define (opcode-8xy2 ram reg x y)
  (values ram
          (struct-copy registers reg
                       [v (registers-vn-set reg x
                               (bitwise-and (registers-vn reg x)
                                            (registers-vn reg y)))])))

;;------------------------------------------------------------------------------

;; (opcode-8xy3 ram registers) sets VX to VX xor VY

;; opcode-8xy3: Ram Registers Byte Byte -> Ram Registers
(define (opcode-8xy3 ram reg x y)
  (values ram
          (struct-copy registers reg
                       [v (registers-vn-set reg x
                               (bitwise-xor (registers-vn reg x)
                                            (registers-vn reg y)))])))

;;------------------------------------------------------------------------------

;; (opcode-8xy4 ram registers) adds VY to VX. VF is set to 1 when there's a
;; carry, and to 0 when there isn't.

;; opcode-8xy4: Ram Registers Byte Byte -> Ram Registers
(define (opcode-8xy4 ram reg x y f)
  (local [(define x+y (+ (registers-vn x) (registers-vn y)))]
      (values ram
          (struct-copy registers reg
                       [v (if (> x+y 255)
                              (registers-vn-set
                               (registers-vn-set reg x
                                                 (if (> x+y 255) 255 x+y))
                               #xf 1)
                              (registers-vn-set
                               (registers-vn-set reg x
                                                 (if (> x+y 255) 255 x+y))
                               #xf 0))]))))

;;------------------------------------------------------------------------------

;; (opcode-8XY5 ram registers) VY is subtracted from VX. VF is set to 0 when
;; there's a borrow, and 1 when there isn't.

;; opcode-8XY5: Ram Registers Byte Byte -> Ram Registers
(define (opcode-8xy5 ram reg x y)
  (local [(define x-y (- (registers-vn x) (registers-vn y)))]
      (values ram
          (struct-copy registers reg
                       [v (if (> (registers-vn x) (registers-vn y))
                                 (registers-vn-set
                                  (registers-vn-set reg x
                                                    (if (< x-y 0) 0 x-y))
                                  #xf 1)
                                (registers-vn-set
                                 (registers-vn-set reg x
                                                   (if (< x-y 0) 0 x-y))
                                 #xf 0))]))))

;;------------------------------------------------------------------------------

;; (opcode-8XY6 ram registers) stores the least significant bit of VX in VF
;; and then shifts VX to the right by 1

;; opcode-8XY6: Ram Registers Byte Byte -> Ram Registers
(define (opcode-8xy6 ram reg x y)
  (values ram
          (if (odd? (registers-vn reg x))
              (struct-copy registers reg
                           [v (registers-vn-set reg #xf 1)])
              (struct-copy registers reg
                           [v (registers-vn-set (registers-vn-set reg #xf 0)
                                                x
                                                (/ (registers-vn reg x) 2))]))))
                   
;;------------------------------------------------------------------------------

;; (opcode-8XY7 ram registers) sets VX to VY minus VX. VF is set to 0 when
;; there's a borrow, and 1 when there isn't

;; opcode-8XY7: Ram Registers Byte Byte -> Ram Registers
(define (opcode-8xy7 ram reg x y)
  (local [(define y-x (- (registers-vn y) (registers-vn x)))]
      (values ram
          (struct-copy registers reg
                       [v (if (> (registers-vn y) (registers-vn x))
                                 (registers-vn-set
                                  (registers-vn-set reg x
                                                    (if (< y-x 0) 0 y-x))
                                  #xf 1)
                                (registers-vn-set
                                 (registers-vn-set reg x
                                                   (if (< y-x 0) 0 y-x))
                                 #xf 0))]))))

;;------------------------------------------------------------------------------

;; (opcode-8xye ram registers) stores the most significant bit of VX in VF
;; and then shifts VX to the left by 1

;; opcode-8xyE: Ram Registers Byte Byte -> Ram Registers
(define (opcode-8xye ram reg x y)
  (values ram
          (if (zero? (bitwise-and 100000000 (registers-vn reg x)))
              (struct-copy registers reg
                           [v (registers-vn-set reg #xf 1)])
              (struct-copy registers reg
                           [v (registers-vn-set (registers-vn-set reg #xf 0)
                                                x
                                                (* (registers-vn reg x) 2))]))))

;;------------------------------------------------------------------------------

;; (opcode-9xy0 ram registers) skips the next instruction if VX doesn't
;; equal VY. (Usually the next instruction is a jump to skip a code block)

;; opcode-9xy0: Ram Registers Byte Byte -> Ram Registers
(define (opcode-9xy0 ram reg x y)
  (values ram
          (if (equal? (registers-vn reg y) (registers-vn reg x))
              reg
              (struct-copy registers reg [pc (+ 2 (registers-pc reg))]))))           

;;------------------------------------------------------------------------------

;; (opcode-annn ram registers) sets I to the address NNN

;; opcode-annn: Ram Registers Word -> Ram Registers
(define (opcode-annn ram reg nnn)
  (values ram (struct-copy registers reg [i nnn])))
   
;;------------------------------------------------------------------------------

;; (opcode-bnnn ram registers) Jumps to the address NNN plus V0 sets VX
;; to the result of a bitwise and operation on a random number
;; (Typically: 0 to 255) and NN

;; opcode-bnnn: Ram Registers -> Ram Registers
(define (opcode-bnnn ram reg nnn)
  (values ram (struct-copy registers reg [pc (+ (registers-vn reg #x0) nnn)])))

;;------------------------------------------------------------------------------

;; (opcode-cxnn ram registers) sets VX to the result of a bitwise and
;; operation on a random number (Typically: 0 to 255) and NN

;; opcode-cxnn: Ram Registers Byte -> Ram Registers
(define (opcode-cxnn ram reg x nn)
  (values ram (registers-vn-set reg x (bitwise-and (random 0 255) nn))))

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

;; opcode-exa1: Ram Registers -> Ram Registers
(define (opcode-exa1 ram registers)
  (void)) ;; Remove this void when you write the function body

;;------------------------------------------------------------------------------

;; (opcode-fx07 ram registers) sets VX to the value of the delay timer.

;; opcode-fx07: Ram Registers Byte -> Ram Registers
(define (opcode-fx07 ram reg x)
  (values ram (struct-copy registers reg
                           [v (registers-vn-set reg x (registers-dt reg))])))  

;;------------------------------------------------------------------------------

;; (opcode-fx0a ram registers) a key press is awaited, and then stored in VX.
;; (Blocking Operation. All instruction halted until next key event)

;; opcode-fx0a: Ram Registers Byte -> Ram Registers
(define (opcode-fx0a ram reg x)
  (void))  ;; Remove this void when you write the function body

;;------------------------------------------------------------------------------

;; (opcode-fx15 ram registers) sets the delay timer to VX

;; opcode-fx15: Ram Registers Byte -> Ram Registers
(define (opcode-fx15 ram reg x)
  (values ram (struct-copy registers reg
                           [dt (registers-vn reg x)])))

;;------------------------------------------------------------------------------

;; (opcode-fx18 ram registers) sets the sound timer to VX

;; opcode-fx18: Ram Registers -> Void
(define (opcode-fx18 ram registers)
  (void)) ;; Remove this void when you write the function body

;;------------------------------------------------------------------------------

;; (opcode-fx1e ram registers) adds VX to I. VF is not affected

;; opcode-fx1e: Ram Registers Byte -> Ram Registers
(define (opcode-fx1e ram reg x)
  (values ram (struct-copy registers reg
                           [i (+ (registers-vn reg x) (registers-i reg))])))

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