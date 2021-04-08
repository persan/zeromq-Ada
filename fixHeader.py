#!/usr/bin/env python
#  ---------------------------------------------------------------------------
#            Copyright (C) 2020-2030, per.s.sandberg@bahnhof.se
#
#  Permission is hereby granted, free of charge, to any person obtaining a
#  copy of this software and associated documentation files
#  (the "Software"), to deal in the Software without restriction, including
#  without limitation the rights to use, copy, modify, merge, publish,
#  distribute, sublicense, and / or sell copies of the Software, and to
#  permit persons to whom the Software is furnished to do so, subject to
#  the following conditions :
#
#  The above copyright notice and this permission notice shall be included
#  in all copies or substantial portions of the Software.
#
#  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
#  OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
#  MERCHANTABILITY,
#  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
#  THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR
#  OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
#  ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
#  OTHER DEALINGS IN THE SOFTWARE.
#  ---------------------------------------------------------------------------

import GPS
from os.path import *
import string

HEADER = \
"""##############################################################################
##                              MIT License                                 ##
##                             0MQ Ada-binding                              ##
%(name)s
##                                                                          ##
%(ext)s
##                                                                          ##
##             Copyright (c) 2021 per.s.sandberg@bahnhof.se                 ##
##                                                                          ##
## Permission is hereby granted, free of charge, to any person obtaining a  ##
## copy of this software and associated documentation files                 ##
## (the "Software"), to deal in the Software without restriction, including ##
## without limitation the rights to use, copy, modify, merge, publish,      ##
## distribute, sublicense, and/or sell copies of the Software, and to       ##
## permit persons to whom the Software is furnished to do so, subject to    ##
## the following conditions:                                                ##
##                                                                          ##
## The above copyright notice and this permission notice shall be included  ##
## in all copies or substantial portions of the Software.                   ##
##                                                                          ##
## THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS  ##
## OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF               ##
## MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.   ##
## IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY     ##
## CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,     ##
## TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE        ##
## SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.                   ##
##############################################################################
"""


def to80Comment(s):
    if len(s) > 76:
        s = s.replace(" ", "")
    if len(s) > 76:
        s = s[:-76]
    n = 76 - len(s)
    n = n/2
    ret = "##" + (" " * n) + s
    ret = ret + (" " * (76 - len(ret))) + "##"
    return ret


def getHeader(f):
    name, ext = splitext(basename(f.name()))
    name = name.upper().replace("-", ".")
    name = to80Comment(string.join(name, " "))
    if ext == ".ads":
        ext = to80Comment("S p e c")
        return(HEADER % {"name": name, "ext": ext}).replace("#", "-")
    elif ext == ".adb":
        ext = to80Comment("B o d y")
        return (HEADER % {"name": name, "ext": ext}).replace("#", "-")
    elif ext == ".gpr":
        ext = to80Comment("P r o j e c t")
        return (HEADER % {"name": name, "ext": ext}).replace("#", "-")
    else:
        ext = to80Comment("")
        return HEADER % {"name": name, "ext": ext}


def fixFile(f, of):
    name, ext = splitext(basename(f.name()))
    ed = GPS.EditorBuffer.get(f)
    begin = ed.beginning_of_buffer()
    if (ext not in [".gpr", ".adb", ".ads",
                    ".c", ".cpp", ".h", ".hh", ".idl"]):
        begin = begin.forward_line(1)
    last = ed.beginning_of_buffer().search(r"\n\n", regexp=True)

    of.write("%s\n" % ( 80 * "/"))
    of.write("%s\n" % (f.name()))
    of.write("%s\n" % (30 * "<"))
    of.write("%s\n" % (ed.get_chars(begin, last[0])))
    of.write("%s\n" % (30 * "<"))
    of.write("%s\n" % (getHeader(f)))
    of.write("%s\n" % (80 * "/"))

with file("temp.out", "w") as of:
    for i in GPS.Project("zmq").sources():
        fixFile(i, of)



