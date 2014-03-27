all: echo.hex echo.bin

echo.hex: echo.p
	p2hex -F Intel echo.p

echo.bin: echo.p
	p2bin echo.p

echo.p: echo.asm
	asl -L -cpu 8080 echo.asm

clean:
	$(RM) *.lst *.p *.bin *.hex
