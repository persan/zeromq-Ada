#!/usr/bin/env python


renames=[["with stddef_h;\n",""],
         ["stddef_h.size_t","size_t"],
         ["zmq_h","ZMQ.Low_Level"]]


import sys
import os
import re


m=re.compile("(^.*-- *)(/.*?/)(zmq\\.h.*$)")
def fix(line):
  ma=m.match(line)
  if ma:
     line=ma.group(1)+ma.group(3)
  ma=re.match("^\\s+--\\s+unsupported\\s+macro:\\s+(\\w+)\\s+.ZMQ_HAUSNUMERO\\s+\\+\\s+(\\w+).*",line)
  if ma:
     line="   %s : constant := ZMQ_HAUSNUMERO + %s;" % (ma.group(1), ma.group(2))
  return line

def rename(p):
   f=file(p)
   buffer=f.read()
   f.close();

   for i in renames:
      buffer=buffer.replace(i[0],i[1])

   buffer=buffer.split("\n")
   ret=[]
   ret.append('--------------------------------------------------------------------')
   ret.append('--                                                                --')
   ret.append('--  Do not edit, this file is automaticly generated from "zmq.h"  --')
   ret.append('--                                                                --')
   ret.append('--------------------------------------------------------------------')
   ret.append('')
   for line in buffer:
      ret.append(fix(line))
      if re.match("^package.*is",line):
         ret.append("   pragma Preelaborate;")

   f=file(p,"w")
   f.write("\n".join(ret))
   f.close()

if __name__ == "__main__":
  rename(sys.argv[1])
