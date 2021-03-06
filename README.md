# Another Pong
A fully-commented Pong clone written in 6502 assembly for the Atari 2600.

<p align="center">
  <img alt="another-pong" src="./media/another-pong.png" width="75%" />
</p>

## Features
* Two-player support
* Sound effects

## Build and Run
**The included `Makefile` assumes that [DASM assembler](https://github.com/dasm-assembler/dasm) and the Atari 2600 emulator [Stella](https://github.com/stella-emu/stella) are installed and available from the command line.**
* Call `make` to assemble the `another-pong.bin` ROM.
* Call `make run` to run the ROM file via Stella.
* Call `make debug` to run the ROM in Stella's debug mode.
