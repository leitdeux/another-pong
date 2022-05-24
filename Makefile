all:
	dasm main.asm -f3 -v0 -oanother-pong.bin

run:
	stella another-pong.bin

debug:
	stella -debug another-pong.bin
