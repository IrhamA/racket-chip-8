# racket-chip-8
This is a virtual machine for the CHIP-8 interpreted programming language.
The [CHIP-8](https://en.wikipedia.org/wiki/CHIP-8) architecture is limited
to 4096 bytes of ram, a 64x32 monochrome display, 16-key input, and beeper
sound output.

![Pong](https://raw.githubusercontent.com/IrhamA/racket-chip-8/master/screenshots/pong.png)

## Features
- Partial emulated display support
- 90% binary compatibility (excluding input opcodes)
- RAM inspector (accessible through DrRacket)
- Disassembler (accessible through DrRacket)
