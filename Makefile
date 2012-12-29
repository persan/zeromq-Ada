
ifndef PREFIX
  PREFIX=$(dir $(shell dirname `which gnatls`))
endif
LIBDIR ?= ${PREFIX}/lib
DESTDIR ?= 
GNATFLAGS ?=
ADA_PROJECT_DIR ?= ${PREFIX}/lib/gnat
GNATMAKE = gnatmake ${GNATFLAGS} -p -f -R 
compile:
	${GNATMAKE} -P zmq.gpr -XLIBRARY_TYPE=static
	 ${GNATMAKE} -P zmq.gpr -XLIBRARY_TYPE=relocatable

uninstall:
	rm -rf ${DESTDIR}/${PREFIX}/include/zmq ${DESTDIR}/${LIBDIR}/zmq ${DESTDIR}/${ADA_PROJECT_DIR}/zmq.gpr

install: compile uninstall
	mkdir -p ${DESTDIR}/${PREFIX}/include/zmq
	mkdir -p ${DESTDIR}/${LIBDIR}/zmq
	mkdir -p ${DESTDIR}/${ADA_PROJECT_DIR}

	cp -r lib/* ${DESTDIR}/${LIBDIR}/zmq

	cp -f src/zmq.ad* ${DESTDIR}/${PREFIX}/include/zmq
	cp -f src/zmq-*.ad* ${DESTDIR}/${PREFIX}/include/zmq
	chmod -w ${DESTDIR}/${PREFIX}/include/zmq/*.ad?
#	(cd ${DESTDIR}/${PREFIX}/lib; for i in `find -name lib*.so*`; do ln -s $$i ; done)s
	cp zmq.gpr.inst ${DESTDIR}/${ADA_PROJECT_DIR}/zmq.gpr

	mkdir -p ${DESTDIR}/${PREFIX}/share/zmq/examples/Ada
	cp examples/zmq-examples*.ad* ${DESTDIR}/${PREFIX}/share/zmq/examples/Ada
	cp examples/zmq-examples.gpr.inst ${DESTDIR}/${PREFIX}/share/zmq/examples/Ada/zmq-examples.gpr
all: compile install

samples:
	${GNATMAKE} -P examples/zmq-examples.gpr

generate:
	mkdir -p .temp
	echo "#include <zmq.h>">.temp/x.cpp
	gprbuild -p -c -Pgenerate.gpr x.cpp

	python renames.py .temp/zmq_h.ads
	gnatchop -w -gnat05 .temp/zmq_h.ads  src
	gnatpp  -rnb -M127 src/zmq-low_level.ads -cargs -gnat05

        
clean:
	rm -rf .obj
	${MAKE} -C tests clean
	
setup:
	${MAKE} -C eBindings install

test:
	${MAKE} -C tests
