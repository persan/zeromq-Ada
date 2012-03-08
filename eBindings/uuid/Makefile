

include Makefile.inc

compile:Makefile.inc
	gprbuild -p -P uuid.gpr  -XLIBRARY_TYPE=static
	gprbuild -p -P uuid.gpr  -XLIBRARY_TYPE=relocatable

Makefile.inc:
	echo PREFIX=$(dir $(shell which gnatls)).. >Makefile.inc

all:compile install

install:
	mkdir -p ${PREFIX}/lib/gnat
	mkdir -p ${PREFIX}/include/uuid
	mkdir -p ${PREFIX}/lib/uuid/relocatable
	mkdir -p ${PREFIX}/lib/uuid/static

	cp gnatcoll-uuid*.ad?        ${PREFIX}/include/uuid
	cp uuid.gpr.inst             ${PREFIX}/lib/gnat/uuid.gpr
	cp -r lib/relocatable/*.ali  ${PREFIX}/lib/uuid/relocatable
	cp -r lib/relocatable/*.so*  ${PREFIX}/lib/uuid/relocatable
	cp -r lib/static/*.ali       ${PREFIX}/lib/uuid/static
	cp -r lib/static/*.a         ${PREFIX}/lib/uuid/static

	chmod -w ${PREFIX}/lib/gnat/*uuid.gpr
	chmod -w ${PREFIX}/include/uuid/*.ad?
	chmod -w ${PREFIX}/lib/uuid/relocatable
	chmod -w ${PREFIX}/lib/uuid/static



uninstall:
	rm -rf ${PREFIX}/lib/uuid
	rm -rf ${PREFIX}/include/uuid/
	rm -rf ${PREFIX}/lib/gnat/uuid.gpr
clean:
	rm -rf lib .obj
	rm Makefile.inc


generate:
	rm -rf gen
	mkdir -p gen
	(cd gen ;gcc -c -fdump-ada-spec ../generate.c)
