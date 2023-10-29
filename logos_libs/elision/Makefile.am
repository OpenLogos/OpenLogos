# -*- makefile -*-

SUBDIRS=english french german spanish portuguese italian

noinst_LTLIBRARIES = libelision.la

libelision_la_LIBADD = \
	$(top_builddir)/logos_libs/elision/french/libfrench.la \
	$(top_builddir)/logos_libs/elision/spanish/libspanish.la \
	$(top_builddir)/logos_libs/elision/portuguese/libportuguese.la \
	$(top_builddir)/logos_libs/elision/italian/libitalian.la

libelision_la_SOURCES = replaceconsequent.cpp replaceconsequent.h \
  searchantecedent.cpp searchantecedent.h variable.cpp variable.h \
  document.h engine.h factory.h initcapconsequent.h lowercaseconsequent.h \
  primitive.h 