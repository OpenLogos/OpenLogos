# -*- makefile -*-

vpath %cpp $(top_srcdir)/logos_libs/halfnoun

AM_CPPFLAGS = @JAVAINCLUDE@

lib_LTLIBRARIES = libhalfnoun.la

libhalfnoun_la_LIBADD= $(top_builddir)/logos_libs/utility/libutility.la
#libhalfnoun_la_LIBADD= \
#	$(top_builddir)/configdatafileinterface/libconfigdatafileinterface.la\
#	$(top_builddir)/logos_libs/regex/libregex.la\
#	$(top_builddir)/logos_libs/newdebug/libnewdebug.la

libhalfnoun_la_SOURCES = \
	GermanHalfNounJNI.cpp \
	germancache.cpp \
	germancache.h

nodist_libhalfnoun_la_SOURCES = \
        germanconnectorinfo.cpp \
        germandictionary1.cpp \
        germandictionary2.cpp \
        germandictionaryentry.cpp \
        germanexclusioninfo.cpp \
        germanflag.cpp \
        germanpatsteminfo.cpp \
        germansearch.cpp \
        germansuffixinfo.cpp \
        germantoken.cpp \
        germanutil.cpp \
        logfile.cpp \
        timer.cpp

