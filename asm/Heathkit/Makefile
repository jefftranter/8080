SUBDIRS = H8 H88 H89 H19 H90 H17

all:
	@for i in $(SUBDIRS); do \
	echo "make all in $$i..."; \
	(cd "$$i"; $(MAKE) $(MFLAGS) all); done

clean:
	@for i in $(SUBDIRS); do \
	echo "Cleaning in $$i..."; \
	(cd "$$i"; $(MAKE) $(MFLAGS) clean); done
