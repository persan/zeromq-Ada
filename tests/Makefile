
GNATFLAGS ?= 

all: compile test

compile:
	gprbuild -p -P zmq-tests.gpr ${GNATFLAGS}

test:
	bin/test_all


