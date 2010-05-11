PREFIX=$(dir $(shell which gnatls))..

compile:
	gprbuild -p -P uuid.gpr

all:compile install

install:
	mkdir -p ${PREFIX}/include/uuid
	cp *.ad? ${PREFIX}/include/uuid
	chmod -w ${PREFIX}/include/uuid/*.ad?
	mkdir -p ${PREFIX}/lib/gnat
	cp libuuid.gpr ${PREFIX}/lib/gnat
	cp uuid.gpr.inst ${PREFIX}/lib/gnat/uuid.gpr
	chmod -w ${PREFIX}/lib/gnat/*uuid.gpr
	mkdir -p    ${PREFIX}/lib/uuid
	cp lib/*    ${PREFIX}/lib/uuid/

clean:
	rm -rf lib .obj
