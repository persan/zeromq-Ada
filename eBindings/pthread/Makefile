PREFIX=$(dir $(shell which gnatls))..

compile:
#	gprbuild -p -P uuid.gpr

all:compile install

install:
	mkdir -p ${PREFIX}/lib/gnat
	cp libpthread.gpr ${PREFIX}/lib/gnat


clean:
	rm -rf lib .obj
