#!/bin/sh
set -eo

pushd .
cd tests
alr build --validation --profiles=zeromq_ada=validation && ./bin/test_all
popd

