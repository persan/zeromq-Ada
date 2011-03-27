
ifndef PREFIX
  PREFIX=$(dir $(shell dirname `which gnatls`))
endif
DESTDIR ?= 
compile:
	gnatmake -p -P zmq.gpr -XLIBRARY_TYPE=static
	gnatmake -p -P zmq.gpr -XLIBRARY_TYPE=relocatable

uninstall:
	rm -rf ${DESTDIR}/${PREFIX}/include/zmq ${DESTDIR}/${PREFIX}/lib/zmq ${DESTDIR}/${PREFIX}/lib/gnat/zmq.gpr

install: compile uninstall
	mkdir -p ${DESTDIR}/${PREFIX}/include/zmq
	mkdir -p ${DESTDIR}/${PREFIX}/lib/zmq
	mkdir -p ${DESTDIR}/${PREFIX}/lib/gnat

	cp -r lib/* ${DESTDIR}/${PREFIX}/lib/zmq

	cp -f src/zmq.ad* ${DESTDIR}/${PREFIX}/include/zmq
	cp -f src/zmq-*.ad* ${DESTDIR}/${PREFIX}/include/zmq
	chmod -w ${DESTDIR}/${PREFIX}/include/zmq/*.ad?
#	(cd ${DESTDIR}/${PREFIX}/lib; for i in `find -name lib*.so*`; do ln -s $$i ; done)s
	cp zmq.gpr.inst ${DESTDIR}/${PREFIX}/lib/gnat/zmq.gpr

	mkdir -p ${DESTDIR}/${PREFIX}/share/zmq/examples/Ada
	cp examples/zmq-examples*.ad* ${DESTDIR}/${PREFIX}/share/zmq/examples/Ada
	cp examples/zmq-examples.gpr.inst ${DESTDIR}/${PREFIX}/share/zmq/examples/Ada/zmq-examples.gpr
all: compile install

examples:
	gnatmake -p -P examples/zmq-examples.gpr

generate:
	mkdir -p .temp
	echo "#include <zmq.h>">.temp/x.c
	(cd .temp;g++  -c -fdump-ada-spec x.c)
	cat .temp/zmq_h.ads | sed "s-/usr/local/include/--" >src/zmq_h.ads
clean:
	rm -rf .obj
	${MAKE} -C tests clean
	
setup:
	${MAKE} -C eBindings install

test:
	${MAKE} -C tests
