#!/usr/bin/env python


import sys
import re
header="""-------------------------------------------------------------------------------
--                                                                           --
--                             0MQ Ada-binding                               --
--                                                                           --
--                         Z M Q . L O W _ L E V E L                         --
--                                                                           --
--                                  S p e c                                  --
--                                                                           --
--            Copyright (C) 2010-2011, per.sandberg@bredband.net             --
--                                                                           --
--  Permission is hereby granted, free of charge, to any person obtaining a  --
--  copy of this software and associated documentation files                 --
--  (the "Software"), to deal in the Software without restriction, including --
--  without limitation the rights to use, copy, modify, merge, publish,      --
--  distribute, sublicense, and / or sell copies of the Software, and to     --
--  permit persons to whom the Software is furnished to do so, subject to    --
--  the following conditions :                                               --
--                                                                           --
--  The above copyright notice and this permission notice shall be included  --
--  in all copies or substantial portions of the Software.                   --
--                                                                           --
--  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS  --
--  OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF               --
--  MERCHANTABILITY,                                                         --
--  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL  --
--  THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR     --
--  OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,    --
--  ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR    --
--  OTHER DEALINGS IN THE SOFTWARE.                                          --
-------------------------------------------------------------------------------


--  The contents of this file is derived from zmq.h using the
--   -fdump-ada-spec switch for gcc.

"""

renames=[["stddef_h.size_t","size_t"],
["with stddef_h;",""],
["with stdint_h;","with Interfaces.C.Extensions;"],
["stdint_h.uint16_t","Extensions.Unsigned_16"],
["stdint_h.int32_t","Extensions.Signed_32"],
["stdint_h.uint8_t","Extensions.Unsigned_8"],
["package zmq_h is","""package ZMQ.Low_Level is

   pragma Preelaborate;
   pragma Warnings (Off);

   package Defs is
   --  This package is here to give a namespace to constants, since
   --  identifiers in Ada are caseinsensetive.

"""],
["procedure zmq_version","""   end Defs;

   procedure zmq_version"""],
["zmq_h","ZMQ.Low_Level"]]


obslolete_functions=["zmq_ctx_shutdown","zmq_ctx_term","zmq_term","zmq_sendmsg","zmq_recvmsg"]

def main(path):
    dumped = False
    with file(path) as f:
        buffer=f.read()

    for i in renames:
        buffer=buffer.replace(i[0],i[1])

    buffer=buffer.split("\n")
    include_matcher=re.compile("(.+-- +)(/.+/include/)(.*)")
    #      --  unsupported macro: EFSM (ZMQ_HAUSNUMERO + 51)
    e_matcher=re.compile(r""".+unsupported macro: (\w+) \((\w+) \+ (\w+)\).*""")

    #--  unsupported macro: ZMQ_XREQ ZMQ_DEALER
    r_matcher=re.compile(r""".+unsupported macro: (\w+) (\w+)$""")
    for i in range(0,len(buffer)):
       m=include_matcher.match(buffer[i])
       if m:
          buffer[i]=m.group(1) + m.group(3)
       m=e_matcher.match(buffer[i])
       if m:
          buffer[i]="%s : constant := %s + %s;" % (m.group(1) , m.group(2), m.group(3))

       m=r_matcher.match(buffer[i])
       if m:
          buffer[i]="%s : constant := %s;" % (m.group(1) , m.group(2))

       for obslolete in obslolete_functions:
          pattern=r'^.*pragma Import *\(C, *(%s), *"%s"\);' % (obslolete, obslolete)
          if not dumped:
             print pattern
          matcher=re.compile(pattern)
          if matcher.match(buffer[i]):
             buffer[i] = "   pragma Obsolescent;\n%s" % buffer[i]
       dumped = True


    buffer="\n".join(buffer)
    with file(path,"w") as f:
        f.write(header)
        f.write(buffer)


if __name__ == "__main__":
   main(sys.argv[1])
