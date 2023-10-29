# -*- makefile -*-

noinst_LTLIBRARIES = libregex.la

EXTRA_DIST = 1_readme.txt

libregex_la_SOURCES = charutil.cpp matchinfo.cpp \
	regex.cpp regularexpression.cpp charutil.h matchinfo.h \
	regex.h regularexpression.h

if TESTPROGS
  bin_PROGRAMS = testchar

  testchar_SOURCES = main.cpp

  testchar_LDADD = libregex.la
else
  EXTRA_DIST += main.cpp
endif