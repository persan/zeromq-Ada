Name:		zeromq-ada
Version:	4.1.5
Release:	1%{?dist}
Summary:	Ada binding for zeromq

Group:          System Environment/Libraries

License:	MIT
URL:		zeromq.org
Source:	%{name}-%{version}.tar.bz
Patch0: 0001-change-library-path.patch
BuildRequires:	gcc-gnat zeromq-devel >= %{version}
Requires:	zeromq-devel >= %{version}
BuildArchitectures: x86_64

%description
Ada bindings for zeromq

%prep
%setup -n zeromq-ada
%patch0 -p1
%build
make %{?_smp_mflags}

%package devel
Summary:        Devel package for Ada binding for zeromq
Group:          System Environment/Libraries
License:        MIT
Requires:       %name = %version

%description devel
Devel package for Ada binding for zeromq


%install
rm -rf %{buildroot}
make install DESTDIR=%{buildroot}
#correct whate makefile makes wrong
%{__mkdir} -p %{buildroot}%{_prefix}/gnat/share/gpr
%{__mv} %{buildroot}/zmq.gpr %{buildroot}%{_prefix}/gnat/share/gpr/
%{__install} libzmq.gpr -m 644  %{buildroot}%{_prefix}/gnat/share/gpr/libzmq.gpr
rm -f %{buildroot}/usr/gnat/lib/zmq/static/libzmqAda.a

%files
%defattr(-,root,root,-)
/usr/gnat/lib/zmq/relocatable/libzmqAda.so.%{version}


%files devel
%defattr(-,root,root,-)
%{_prefix}/gnat/lib/zmq/relocatable/libzmqAda.so
%{_prefix}/gnat/include/zmq/zmq-contexts.adb
%{_prefix}/gnat/include/zmq/zmq-contexts.ads
%{_prefix}/gnat/include/zmq/zmq-messages.adb
%{_prefix}/gnat/include/zmq/zmq-messages.ads
%{_prefix}/gnat/include/zmq/zmq-sockets.adb
%{_prefix}/gnat/include/zmq/zmq-sockets.ads
%{_prefix}/gnat/include/zmq/zmq-utilities-memory_streams.adb
%{_prefix}/gnat/include/zmq/zmq-utilities-memory_streams.ads
%{_prefix}/gnat/include/zmq/zmq-utilities.ads
%{_prefix}/gnat/include/zmq/zmq.adb
%{_prefix}/gnat/include/zmq/zmq.ads
%{_prefix}/gnat/include/zmq/zmq-pollsets.adb
%{_prefix}/gnat/include/zmq/zmq-pollsets.ads
%{_prefix}/gnat/include/zmq/zmq-proxys.adb
%{_prefix}/gnat/include/zmq/zmq-proxys.ads
%{_prefix}/gnat/include/zmq/zmq-sockets-indefinite_typed_generic.adb
%{_prefix}/gnat/include/zmq/zmq-sockets-indefinite_typed_generic.ads
%{_prefix}/gnat/include/zmq/zmq-sockets-typed_generic.adb
%{_prefix}/gnat/include/zmq/zmq-sockets-typed_generic.ads
%{_prefix}/gnat/include/zmq/zmq-utilities-stream_element_array_image.adb
%{_prefix}/gnat/include/zmq/zmq-utilities-stream_element_array_image.ads
%{_prefix}/gnat/include/zmq/zmq-low_level.ads

%{_prefix}/gnat/share/gpr/zmq.gpr
%{_prefix}/gnat/share/gpr/libzmq.gpr
%{_prefix}/gnat/lib/zmq/relocatable/zmq-contexts.ali
%{_prefix}/gnat/lib/zmq/relocatable/zmq-low_level.ali
%{_prefix}/gnat/lib/zmq/relocatable/zmq-messages.ali
%{_prefix}/gnat/lib/zmq/relocatable/zmq-sockets.ali
%{_prefix}/gnat/lib/zmq/relocatable/zmq-utilities-memory_streams.ali
%{_prefix}/gnat/lib/zmq/relocatable/zmq-utilities.ali
%{_prefix}/gnat/lib/zmq/relocatable/zmq.ali
%{_prefix}/gnat/lib/zmq/relocatable/zmq-proxys.ali
%{_prefix}/gnat/lib/zmq/static/zmq-contexts.ali
%{_prefix}/gnat/lib/zmq/static/zmq-low_level.ali
%{_prefix}/gnat/lib/zmq/static/zmq-messages.ali
%{_prefix}/gnat/lib/zmq/static/zmq-sockets.ali
%{_prefix}/gnat/lib/zmq/static/zmq-utilities-memory_streams.ali
%{_prefix}/gnat/lib/zmq/static/zmq-utilities.ali
%{_prefix}/gnat/lib/zmq/static/zmq.ali
%{_prefix}/gnat/lib/zmq/static/zmq-pollsets.ali
%{_prefix}/gnat/lib/zmq/static/zmq-proxys.ali
%{_prefix}/gnat/lib/zmq/static/zmq-sockets-indefinite_typed_generic.ali
%{_prefix}/gnat/lib/zmq/static/zmq-sockets-typed_generic.ali
%{_prefix}/gnat/lib/zmq/static/zmq-utilities-stream_element_array_image.ali
%{_prefix}/gnat/share/zmq/examples/Ada/* 

%changelog
* Fri Nov 14 2025 Lutz Beger <lutz.berger@airbus.com> 4.1.5-1
- update spec file and provide patch
* Wed Feb 2 2011 Pavel Zhukov <pavel@zhukoff.net> - 2.0.10-1
- Initial package
