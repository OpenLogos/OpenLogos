# -*- makefile -*-

lib_LTLIBRARIES = liblgs_parsetran3dll.la

liblgs_parsetran3dll_la_LDFLAGS = -Wl,--version-script="$(top_srcdir)/transl/lgs_parsetran3dll/version.ld"

EXTRA_DIST = version.ld

liblgs_parsetran3dll_la_LIBADD = \
 $(top_builddir)/logos_libs/lgs_parsetran3/liblgs_parsetran3.la \
 $(top_builddir)/logos_libs/lgs_tran_rule_io/liblgs_tran_rule_io.la \
 $(top_builddir)/logos_libs/parsetrans/libparsetrans.la \
 $(top_builddir)/logos_libs/res_parsetran_utils/libres_parsetran_utils.la

liblgs_parsetran3dll_la_SOURCES =  t3mainentry.cpp
