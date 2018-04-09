-include Makefile.config
export GPR_PROJECT_PATH:=${CURDIR}
all: compile

Makefile.config: configure
	./configure

GNATMAKE = gprbuild ${GNATFLAGS} -p -f -R

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

samples:
	${GNATMAKE} -P examples/zmq-examples.gpr

generate:
	rm -rf src/gen/*
	mkdir -p .temp src/gen
	echo "#include <zmq.h>">.temp/x.c
	(cd .temp;g++  -c -fdump-ada-spec -C x.c)
	python rename.py .temp/zmq_h.ads
	cp .temp/zmq_h.ads src/gen/zmq-low_level.ads
	gnatpp -rf  -M128  --comments-special src/gen/*.ads
	sed "s-Zmq.Low_Level-ZMQ.Low_Level-" -i src/gen/zmq-low_level.ads
clean:
	git clean -fXd
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

gps:
	gps -P tests/zmq-tests.gpr
