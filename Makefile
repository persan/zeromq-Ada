
ifndef PREFIX
  PREFIX=$(dir $(which gnatls))..
endif



compile:
	gprbuild -p -P zmq.gpr -XLIBRARY_TYPE=static
	gprbuild -p -P zmq.gpr -XLIBRARY_TYPE=relocatable
install:
	mkdir -p ${PREFIX}/include/zmq
	mkdir -p ${PREFIX}/lib/zmq
	mkdir -p ${PREFIX}/lib/gnat

	cp -r lib/* ${PREFIX}/lib/zmq

	cp -f src*/*.ad* ${PREFIX}/include/zmq
	chmod -w ${PREFIX}/include/zmq/*.ad?
#	(cd ${PREFIX}/lib; for i in `find -name lib*.so*`; do ln -s $$i ; done)s
	cp zmq.gpr.inst ${PREFIX}/lib/gnat/zmq.gpr

	mkdir -p ${PREFIX}/share/zmq/examples/Ada
	cp examples/*.ad* ${PREFIX}/share/zmq/examples/Ada
	cp examples/zmq-examples.gpr.inst ${PREFIX}/share/zmq/examples/Ada/zmq-examples.gpr

all: compile install

examples:
	gprbuild -p -P examples/zmq-examples.gpr

generate:
	mkdir -p .temp
	echo "#include <zmq.h>">.temp/x.c
	(cd .temp;gcc  -c -fdump-ada-spec x.c)
	cat .temp/zmq_h.ads | sed "s-/usr/local/include/--" >src/zmq_h.ads


