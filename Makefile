
ifndef PREFIX
  PREFIX=$(dir $(shell dirname `which gnatls`))
endif

compile:
	gprbuild -p -P zmq.gpr -XLIBRARY_TYPE=static
	gprbuild -p -P zmq.gpr -XLIBRARY_TYPE=relocatable

uninstall:
	rm -rf ${PREFIX}/include/zmq ${PREFIX}/lib/zmq ${PREFIX}/lib/gnat/zmq.gpr

install: uninstall
	mkdir -p ${PREFIX}/include/zmq
	mkdir -p ${PREFIX}/lib/zmq
	mkdir -p ${PREFIX}/lib/gnat

	cp -r lib/* ${PREFIX}/lib/zmq

	cp -f src/zmq.ad* ${PREFIX}/include/zmq
	cp -f src/zmq-*.ad* ${PREFIX}/include/zmq
	chmod -w ${PREFIX}/include/zmq/*.ad?
#	(cd ${PREFIX}/lib; for i in `find -name lib*.so*`; do ln -s $$i ; done)s
	cp zmq.gpr.inst ${PREFIX}/lib/gnat/zmq.gpr

	mkdir -p ${PREFIX}/share/zmq/examples/Ada
	cp examples/zmq-examples-*.ad* ${PREFIX}/share/zmq/examples/Ada
	cp examples/zmq-examples.gpr.inst ${PREFIX}/share/zmq/examples/Ada/zmq-examples.gpr

all: compile install

examples:
	gprbuild -p -P examples/zmq-examples.gpr

generate:
	mkdir -p .temp
	echo "#include <zmq.h>">.temp/x.c
	(cd .temp;g++  -c -fdump-ada-spec x.c)
	cat .temp/zmq_h.ads | sed "s-/usr/local/include/--" >src/zmq_h.ads

setup:
	${MAKE} -C eBindings install

test:
	${MAKE} -C tests
