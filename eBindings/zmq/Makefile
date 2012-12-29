PREFIX=$(dir $(shell which gnatls))..

all:install

install:
	mkdir -p ${PREFIX}/lib/gnat
	cp -f libzmq.gpr ${PREFIX}/lib/gnat
	chmod -w ${PREFIX}/lib/gnat/libzmq.gpr

clean:
	rm -rf lib .obj gen
generate:
	rm -rf gen
	mkdir -p gen
	(cd gen ;gcc -c -fdump-ada-spec -D__USE_GNU=1 ../generate.c)
