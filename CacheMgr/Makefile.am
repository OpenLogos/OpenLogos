# -*- makefile -*-

if TESTPROGS

  bin_PROGRAMS = CacheMgr

  CacheMgr_SOURCES = CacheManager.cpp CacheManager.h main.cpp

  CacheMgr_LDADD = $(top_builddir)/lgs_db_io/liblgs_db_io.la \
    $(top_builddir)/logos_libs/lgs_tran_rule_io/liblgs_tran_rule_io.la

else

  EXTRA_DIST = CacheManager.cpp CacheManager.h main.cpp

endif
