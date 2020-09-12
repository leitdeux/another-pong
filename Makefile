all:
	dasm main.asm -f3 -v0 -ocart.bin

run:
	stella cart.bin

debug:
	stella -debug cart.bin