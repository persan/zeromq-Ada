

-include Makefile.config

Makefile.config:configure
	./configure

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
	rm -rf src/gen/*
	mkdir -p .temp src/gen
	echo "#include <zmq.h>">.temp/x.c
	(cd .temp;g++  -c -fdump-ada-spec x.c)
	python rename.py .temp/zmq_h.ads
	gnatchop -w -gnat12 .temp/zmq_h.ads src/gen
	gnat pretty -P zmq.gpr -rf   -M128  src/gen/*.ads

clean:
	rm -rf .obj
	${MAKE} -C tests clean

test:
	${MAKE} -C tests
dist:
	rm -rf .dist
	gprbuild -p -P helpers/zmq-helpers.gpr -XLIBRARY_TYPE=static
	echo "|$(shell helpers/getinfo --binding-version)|"
	git clone . .dist/zeromq-ada-$(shell helpers/getinfo --binding-version)
	rm -rf .dist/zeromq-ada-$(shell helpers/getinfo --binding-version)/.git
	cd .dist; tar -czf ../zeromq-ada-$(shell helpers/getinfo --binding-version).tgz *
	rm -rf .dist

	
	
	
							
	
																									
