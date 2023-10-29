
vpath %.cpp $(top_srcdir)/engine_api/translationserver\
	$(top_srcdir)/logos_libs/utility

bin_PROGRAMS = logos_batch

# -Dunix should be configurable 
# used for setting path separator in PathUtil; default is Windows separator
AM_CPPFLAGS = -DXLATIONINTERFACE_EXPORTS -Dunix -I$(top_srcdir)/engine_api/xlationinterface

logos_batch_SOURCES = Ls8Jobs.cpp Ls8Jobs.h logos_batch.cpp

nodist_logos_batch_SOURCES = lgsout.cpp \
	xlationconsts.cpp XlationEngine.cpp

logos_batch_LDADD = \
	$(top_builddir)/engine_api/xlationinterface/libxlationinterface.la\
	$(top_builddir)/lgs_db_io/liblgs_db_io.la \
	$(top_builddir)/logos_libs/SubjectMatterCodes/libSubjectMatterCodes.la \
	$(top_builddir)/logos_libs/utility/libutility.la

