# -*- makefile -*-

lib_LTLIBRARIES = liblgs_res.la

liblgs_res_la_LDFLAGS = -Wl,--version-script="$(top_srcdir)/transl/lgs_resdll/version.ld"

EXTRA_DIST = version.ld

liblgs_res_la_LIBADD = \
 $(top_builddir)/logos_libs/lgs_tran_rule_io/liblgs_tran_rule_io.la \
 $(top_builddir)/logos_libs/res_parsetran_utils/libres_parsetran_utils.la \
 $(top_builddir)/logos_libs/utility/libutility.la \
 $(top_builddir)/logos_libs/translutility/libtranslutility.la


liblgs_res_la_SOURCES =  res1and2.c\
			 res2x.c\
			 res6000.c\
			 res_io.cpp\
			 rescsase.c\
			 resformo.c\
			 resmain.c\
			 resmainentry.cpp\
			 resmatch.c\
			 ressem.c\
			 ressws.c\
			 resvserc.c\
			 resvtr65.c\
			 resxdiag.c\
			 rsrulch.c\
			 resadapter.cpp \
			 project.h \
			 projexts.h \
			 resadapter.h \
			 rescommon.h

