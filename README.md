# Another Pong
A fully-commented Pong clone written in 6502 assembly for the Atari 2600.

## Demo

<div style="position:relative; padding-bottom:calc(75.95% + 44px)">
  <iframe src="https://gfycat.com/ifr/ExemplaryGrotesqueGroundhog" frameborder="0" scrolling="no" width="100%" height="100%" style="position:absolute;top:0;left:0;" allowfullscreen>
  </iframe>
</div>

## Features
* Two-player support
* Sound effects

## Build and Run
**The included `Makefile` assumes that [DASM assembler](https://github.com/dasm-assembler/dasm) and the Atari 2600 emulator [Stella](https://github.com/stella-emu/stella) are installed and available from the command line.**
* Call `make` to assemble the `another-pong.bin` ROM.
* Call `make run` to run the ROM file via Stella.
* Call `make debug` to run the ROM in Stella's debug mode.
