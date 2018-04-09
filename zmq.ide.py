import GPS

from os.path import *


def do_load():
    for i in [".", "..", "../.."]:
        path = join(i, "zmq-case_exceptions.xml")
        if exists(path):
            with open(path) as inf:
                buffer = inf.read()
                print buffer
                GPS.parse_xml(buffer)
                print "OK"

try:
    if GPS.zmq_config_is_loaded:
        print "OK"
except:
    GPS.zmq_config_is_loaded = True
    print "\nLOAD\n"
    do_load()
