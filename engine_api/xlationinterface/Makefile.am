# -*- makefile -*-

AM_CPPFLAGS = -DXLATIONINTERFACECONSTANTS_NO_IMPORT @JAVAINCLUDE@

lib_LTLIBRARIES = libxlationinterface.la

libxlationinterface_la_LIBADD = $(top_builddir)/engine_api/proxylayer/libproxylayer.la

libxlationinterface_la_SOURCES =  property.h propertylist.cpp propertylist.h \
 xlationinterfacedefs.cpp xlationinterfacedefs.h \
 xlationsession.cpp xlationsession.h \
 XlationSessionJNI.cpp \
 xlationsessionmanager.cpp xlationsessionmanager.h
