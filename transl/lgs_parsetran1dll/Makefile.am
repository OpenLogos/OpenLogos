# -*- makefile -*-

lib_LTLIBRARIES = liblgs_parsetran1dll.la

liblgs_parsetran1dll_la_LDFLAGS = -Wl,--version-script="$(top_srcdir)/transl/lgs_parsetran1dll/version.ld"

EXTRA_DIST = version.ld

liblgs_parsetran1dll_la_LIBADD = \
 $(top_builddir)/logos_libs/lgs_parsetran1/liblgs_parsetran1.la \
 $(top_builddir)/logos_libs/lgs_tran_rule_io/liblgs_tran_rule_io.la \
 $(top_builddir)/logos_libs/parsetrans/libparsetrans.la \
 $(top_builddir)/logos_libs/res_parsetran_utils/libres_parsetran_utils.la\
 $(top_builddir)/TermMiner/libTermMiner.la

liblgs_parsetran1dll_la_SOURCES =  t1mainentry.cpp
