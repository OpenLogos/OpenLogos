# -*- makefile -*-

vpath %.cpp $(top_srcdir)/engine_api/translationserver

bin_PROGRAMS = APITest

AM_CPPFLAGS = -DXLATIONINTERFACE_EXPORTS

APITest_SOURCES = APIParameter.cpp APIParameter.h APIParameterList.cpp \
	APIParameterList.h APITest.cpp \
	Lgsout.cpp Lgsout.h

# from ../translationserver
nodist_APITest_SOURCES = xlationconsts.cpp XlationEngine.cpp

APITest_LDADD = \
  $(top_builddir)/engine_api/xlationinterface/libxlationinterface.la

APITest_LDADD += \
  $(top_builddir)/lgs_db_io/liblgs_db_io.la \
  $(top_builddir)/logos_libs/SubjectMatterCodes/libSubjectMatterCodes.la \
  $(top_builddir)/logos_libs/utility/libutility.la
