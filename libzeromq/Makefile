


.DEFAULT:all
all:zeromq

PREFIX?=$(dir $(shell dirname `which gnatls`))


########################################################################
##
##  zeromq
##
########################################################################

zeromqVersion?=4.1.4
zeromqBASE?=http://download.zeromq.org

zeromq-${zeromqVersion}.tar.gz:
	wget ${zeromqBASE}/${@}
	
zeromq-${zeromqVersion}:zeromq-${zeromqVersion}.tar.gz
	tar -xzf ${<}

zeromq:zeromq-${zeromqVersion} libsodium
	cd ${<};./configure --prefix=${PREFIX}
	${MAKE} -C ${<} all
	sudo ${MAKE} -C ${<} install
	touch ${@}


########################################################################
##
##  libsodium
##
########################################################################

libsodiumVersion?=1.0.8
libsodiumBASE?=https://github.com/jedisct1/libsodium/releases/download
libsodium-${libsodiumVersion}.tar.gz:
	wget ${libsodiumBASE}/${libsodiumVersion}/${@}
	
libsodium-${libsodiumVersion}:libsodium-${libsodiumVersion}.tar.gz
	tar -xzf ${<}
libsodium:libsodium-${libsodiumVersion}
	cd ${<};./configure --prefix=${PREFIX}
	${MAKE} -C ${<} all
	sudo ${MAKE} -C ${<} install
	touch ${@}

clean:
	rm -rf `cat .gitignore| grep -v "#" `
