PREFIX=$(dir $(shell which gnatls))..

compile:
	gprbuild -p -P uuid.gpr

all:compile install

install:
	mkdir -p ${PREFIX}/lib/gnat
	cp libuuid.gpr ${PREFIX}/lib/gnat

	#mkdir -p ${PREFIX}/include/uuid
	#cp *.ad? ${PREFIX}/include/uuid
	#chmod -w ${PREFIX}/include/uuid/*.ad?
	#cp uuid.gpr.inst ${PREFIX}/lib/gnat/uuid.gpr
	#chmod -w ${PREFIX}/lib/gnat/*uuid.gpr
	#mkdir -p    ${PREFIX}/lib/uuid
	#cp lib/*    ${PREFIX}/lib/uuid/

clean:
	rm -rf lib .obj
generate:
	rm -rf gen
	mkdir -p gen
	(cd gen ;gcc -c -fdump-ada-spec ../generate.c)
