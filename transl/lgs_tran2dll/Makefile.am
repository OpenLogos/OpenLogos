# -*- makefile -*-

lib_LTLIBRARIES = liblgs_tran2dll.la

liblgs_tran2dll_la_LDFLAGS = -Wl,--version-script="$(top_srcdir)/transl/lgs_parsetran2dll/version.ld"

#EXTRA_DIST = version.ld

liblgs_tran2dll_la_LIBADD = \
 $(top_builddir)/logos_libs/lgs_parsetran2/liblgs_parsetran2.la \
 $(top_builddir)/logos_libs/lgs_tran_rule_io/liblgs_tran_rule_io.la \
 $(top_builddir)/logos_libs/parsetrans/libparsetrans.la \
 $(top_builddir)/logos_libs/res_parsetran_utils/libres_parsetran_utils.la

liblgs_tran2dll_la_SOURCES =  tran2entry.cpp
