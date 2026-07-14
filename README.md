# zeromq-Ada
 provides an Ada-binding the 0MQ library
 http://wiki.zeromq.org
## Prerequisits:
zeromq 4.x installed (Works with zeromq 3.x as well but will warn)
a modern GCC (4.3 or better) with Ada enabled

## Install:

### Installing with Alire
Inside your Alire based project, add the dependency:
```
  alr with zeromq_ada
```

### Installing with locally installed toolchain
```
 $ ./configure
 $ make
 $ sudo make install
```
## Tested on
Fedora 27 /native
GNATPro 19.x

openSUSE Tumbleweed
FSF GNAT 15.2.1
