import GPS

from os.path import *


def do_load():
    for i in [".", "..", "../.."]:
        path = join(i, "zmq-case_exceptions.xml")
        if exists(path):
            with open(path) as inf:
                buffer = inf.read()
                GPS.parse_xml(buffer)

try:
    if GPS.zmq_config_is_loaded:
        pass
except:
    GPS.zmq_config_is_loaded = True
    do_load()
