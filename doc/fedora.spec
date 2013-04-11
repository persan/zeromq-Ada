Name:		zeromq-ada
Version:	3.2.0
Release:	1%{?dist}
Summary:	Ada binding for zeromq

Group:          System Environment/Libraries

License:	GPLv2
URL:		zeromq.org
Source0:	%{name}-%{version}.tar.gz
Patch0: 	%{name}-gnat.patch
BuildRequires:	libgnat-static gcc-gnat zeromq >= %{version}
Requires:	zeromq >= %{version}

%description
Ada bindings for zeromq

%prep
%setup -q -n zeromq-ada
%patch0 -p1

%build
make %{?_smp_mflags}

%package devel
Summary:        Devel package for Ada binding for zeromq
Group:          System Environment/Libraries
License:        GPLv2

%description devel
Devel package for Ada binding for zeromq


%install
rm -rf %{buildroot}
make install DESTDIR=%{buildroot}
rm -f %{buildroot}/usr/lib/zmq/static/libzmqAda.a

%files
%defattr(-,root,root,-)
%doc README
/usr/lib/zmq/relocatable/libzmqAda.so.%{version}


%files devel
%defattr(-,root,root,-)
/usr/lib/zmq/relocatable/libzmqAda.so
%{_includedir}/zmq/zmq-contexts.adb
%{_includedir}/zmq/zmq-contexts.ads
%{_includedir}/zmq/zmq-devices.adb
%{_includedir}/zmq/zmq-devices.ads
%{_includedir}/zmq/zmq-low_level.ads
%{_includedir}/zmq/zmq-messages.adb
%{_includedir}/zmq/zmq-messages.ads
%{_includedir}/zmq/zmq-sockets.adb
%{_includedir}/zmq/zmq-sockets.ads
%{_includedir}/zmq/zmq-utilities-memory_streams.adb
%{_includedir}/zmq/zmq-utilities-memory_streams.ads
%{_includedir}/zmq/zmq-utilities.ads
%{_includedir}/zmq/zmq.adb
%{_includedir}/zmq/zmq.ads
/usr/lib/gnat/zmq.gpr
/usr/lib/zmq/relocatable/zmq-contexts.ali
/usr/lib/zmq/relocatable/zmq-devices.ali
/usr/lib/zmq/relocatable/zmq-low_level.ali
/usr/lib/zmq/relocatable/zmq-messages.ali
/usr/lib/zmq/relocatable/zmq-sockets.ali
/usr/lib/zmq/relocatable/zmq-utilities-memory_streams.ali
/usr/lib/zmq/relocatable/zmq-utilities.ali
/usr/lib/zmq/relocatable/zmq.ali
/usr/lib/zmq/static/zmq-contexts.ali
/usr/lib/zmq/static/zmq-devices.ali
/usr/lib/zmq/static/zmq-low_level.ali
/usr/lib/zmq/static/zmq-messages.ali
/usr/lib/zmq/static/zmq-sockets.ali
/usr/lib/zmq/static/zmq-utilities-memory_streams.ali
/usr/lib/zmq/static/zmq-utilities.ali
/usr/lib/zmq/static/zmq.ali
%{_datadir}/zmq/examples/Ada/* 

%changelog
* Wed Feb 2 2011 Pavel Zhukov <pavel@zhukoff.net> - 2.0.10-1
- Initial package
