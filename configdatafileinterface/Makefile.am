# -*- makefile -*-

noinst_LTLIBRARIES = libconfigdatafileinterface.la

EXTRA_DIST = filenames.ini

install-data-local:
	$(INSTALL_DATA) $(srcdir)/filenames.ini \
	   $(DESTDIR)$(pkgdatadir)/bin/filenames.ini

libconfigdatafileinterface_la_LIBADD = \
	$(top_builddir)/logos_libs/utility/libutility.la

libconfigdatafileinterface_la_SOURCES = configdataerror.cpp configdataerror.h\
  configdatafile.cpp configdatafile.h \
  configdatainterface.cpp configdatainterface.h \
  configdatainterfacemain.cpp configdatainterfacemain.h \
  enginedata.cpp enginedata.h \
  resdata.cpp resdata.h \
  sourcedata.cpp sourcedata.h \
  trandata.cpp trandata.h \
  mswindowsinifile.cpp mswindowsinifile.h \
  writeprivateprofilestring.cpp

#  testmain.cpp    # This file seems to be outdated

