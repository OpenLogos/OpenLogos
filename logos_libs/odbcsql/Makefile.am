# -*- makefile -*-

noinst_LTLIBRARIES = libodbcsql.la

libodbcsql_la_SOURCES = globalsqlconnection.cpp\
	odbccolumn.cpp \
	odbcconnection.cpp \
	odbcsequencegenerator.cpp \
	odbcstatement.cpp \
	globalsqlconnection.h\
	odbccolumn.h \
	odbcconnection.h \
	odbcsequencegenerator.h \
	odbcstatement.h \
	logosenv.h \
	logosodbc.h \
	odbcstuff.h

libodbcsql_la_LIBADD = $(top_builddir)/logos_libs/sql/libsql.la
