# -*- makefile -*-

if TESTPROGS

  # translationserver must be changed to use another interface than .COM first
  bin_PROGRAMS = translationserver

  AM_CPPFLAGS = -DXLATIONINTERFACE_EXPORTS

  translationserver_SOURCES = translationserver.cpp xlationconsts.cpp \
	xlationconsts.h XlationEngine.cpp XlationEngine.h comparameters.h \
	resource.h

  translationserver_LDADD = \
    $(top_builddir)/logos_libs/SubjectMatterCodes/libSubjectMatterCodes.la \
    $(top_builddir)/logos_libs/utility/libutility.la \
    $(top_builddir)/lgs_db_io/liblgs_db_io.la

else

  EXTRA_DIST = translationserver.cpp xlationconsts.cpp XlationEngine.cpp \
	comparameters.h resource.h xlationconsts.h

endif