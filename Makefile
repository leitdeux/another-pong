all:
	dasm main.asm -f3 -v0 -oanother-pong.bin

run:
	stella pong.bin

debug:
	stella -debug pong.bin
