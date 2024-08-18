all: rdt.abs

rdt.abs: rdt.bin
	../../../../Z80/HDOS/bin2abs.py rdt.bin rdt.abs

rdt.bin: rdt.p
	p2bin -r '$$-$$' rdt.p

rdt.p: rdt.asm dis.acm dist.acm
	asl -L -cpu 8080 -listradix 8 -splitbyte . rdt.asm

clean:
	$(RM) *.p

distclean:
	$(RM) *.lst *.p *.bin *.abs
