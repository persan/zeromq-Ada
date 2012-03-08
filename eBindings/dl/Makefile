
compile:
	gprbuild -p -P dl.gpr
	gprbuild -p -P dl.gpr

all:compile install

install:
	mkdir -p ${PREFIX}/include/dl
	cp -f dl.ad? ${PREFIX}/include/dl
	chmod -w ${PREFIX}/include/dl/*.ad?
	mkdir -p ${PREFIX}/lib/gnat

	cp -f libdl.gpr ${PREFIX}/lib/gnat
	cp -f dl.gpr.inst ${PREFIX}/lib/gnat/dl.gpr

	chmod -w ${PREFIX}/lib/gnat/*dl.gpr
	mkdir -p    ${PREFIX}/lib/dl

	cp -f lib/*    ${PREFIX}/lib/dl/

clean:
	rm -rf lib .obj gen

generate:
	rm -rf gen
	mkdir -p gen
	(cd gen ;gcc -c -fdump-ada-spec -D__USE_GNU=1 ../generate.c)
