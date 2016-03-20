
zeromqVersion=4.1.4
all:compile
	

zeromq-${zeromqVersion}:zeromq-${zeromqVersion}.tar.gz
	tar -xzf ${<}


zeromq:zeromq-${zeromqVersion} libsodium
	cd ${<};./configure --prefix=/usr/gnat
	${MAKE} -C ${<} all -j 8
	sudo ${MAKE} -C ${<} install
	touch ${@}




libsodiumVersion=1.0.8
libsodium-${libsodiumVersion}.tar.gz:
	wget https://github.com/jedisct1/libsodium/releases/download/${libsodiumVersion}/${@}
	
libsodium-${libsodiumVersion}:libsodium-${libsodiumVersion}.tar.gz
	tar -xzf ${<}
libsodium:libsodium-${libsodiumVersion}
	cd ${<};./configure --prefix=/usr/gnat
	${MAKE} -C ${<} all -j 8
	sudo ${MAKE} -C ${<} install
	touch ${@}
