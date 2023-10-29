# -*- makefile -*-

noinst_LTLIBRARIES = liblgssgml.la

EXTRA_DIST = lgssgml_tag.tbl

install-data-local:
	test -z "$(DESTDIR)$(pkgdatadir)/bin" || $(mkdir_p) "$(DESTDIR)$(pkgdatadir)/bin"
	$(INSTALL_DATA) "$(srcdir)/lgssgml_tag.tbl" "$(DESTDIR)$(pkgdatadir)/bin/lgssgml_tag.tbl" 

liblgssgml_la_SOURCES = lgsalign.cpp \
  lgsaligntarget.cpp \
  lgsbuffer.cpp \
  lgsentity.cpp \
  lgsfmtpipe.cpp \
  lgslex.cpp \
  lgsmerger.cpp \
  lgssent.cpp \
  lgssentence.cpp \
  lgssplitter.cpp \
  lgstag.cpp \
  lgstarget.cpp \
  lgstranio.cpp \
  lgsutil.cpp \
  lgsword.cpp \
  litscan.cpp \
  lgsalign.h \
  lgsaligntarget.h \
  lgsbuffer.h \
  lgsentity.h \
  lgsfmtpipe.h \
  lgslex.h \
  lgsmerger.h \
  lgssent.h \
  lgssentence.h \
  lgssplitter.h \
  lgstag.h \
  lgstarget.h \
  lgstranio.h \
  lgsutil.h \
  lgsword.h \
  litscan.h
