SUBDIRS      = lgs_resdll \
	lgs_parsetran1dll lgs_parsetran2dll \
	lgs_parsetran3dll lgs_parsetran4dll \
	lgs_tran1dll lgs_tran2dll lgs_tran3dll lgs_tran4dll

vpath %.cpp $(top_srcdir)/logos_libs/linguistic $(top_srcdir)/logos_libs/gerdem

bindir = $(pkgdatadir)/bin

bin_PROGRAMS = transl

# transl_SOURCES = AlignOutput.cpp \
#   AsciiAlignTool.cpp \
#   DiagCombine.cpp \
#   Filter.cpp \
#   PostTransl.cpp \
#   PreTransl.cpp \
#   transl.cpp \
#   translthrman.cpp \
#   Utils.cpp \
#   AlignOutput.h \
#   AsciiAlignTool.h \
#   DiagCombine.h \
#   Filter.h \
#   translthrman.h \
#   Utils.h \
#   interface.h lgstraninterface.h

transl_SOURCES = transl.cpp \
  Utils.h \
  AlignOutput.h \
  AsciiAlignTool.h \
  DiagCombine.h \
  Filter.h \
  translthrman.h \
  interface.h lgstraninterface.h

nodist_transl_SOURCES = lsentence.cpp \
  targetunitconcretebuilder.cpp


#noinst_LTLIBRARIES = libtransl.la
lib_LTLIBRARIES = libtransl.la
libtransl_la_SOURCES = AlignOutput.cpp \
  AsciiAlignTool.cpp \
  DiagCombine.cpp \
  Filter.cpp \
  PostTransl.cpp \
  PreTransl.cpp \
  translthrman.cpp \
  Utils.cpp

libtransl_la_LIBADD = \
   $(top_builddir)/lgs_db_io/liblgs_db_io.la \
   $(top_builddir)/transl/lgs_parsetran1dll/liblgs_parsetran1dll.la \
   $(top_builddir)/transl/lgs_parsetran2dll/liblgs_parsetran2dll.la \
   $(top_builddir)/transl/lgs_parsetran3dll/liblgs_parsetran3dll.la \
   $(top_builddir)/transl/lgs_parsetran4dll/liblgs_parsetran4dll.la \
   $(top_builddir)/transl/lgs_resdll/liblgs_res.la \
   $(top_builddir)/logos_libs/translutility/libtranslutility.la \
   $(top_builddir)/logos_libs/gerdem/libgerdem.la \
   $(top_builddir)/logos_libs/elision/libelision.la \
   $(top_builddir)/logos_libs/linguistic/liblinguistic.la \
   $(top_builddir)/logos_libs/patternmatcher/libpatternmatcher.la \
   $(top_builddir)/logos_libs/regex/libregex.la \
   $(top_builddir)/logos_libs/utility/libutility.la \
   $(top_builddir)/logos_libs/startrules/libstartrules.la \
   $(top_builddir)/logos_libs/ruleengine/libruleengine.la \
   $(top_builddir)/lgs_stemgen/liblgs_stemgen.la \
   $(top_builddir)/logos_libs/multithreadlib/libmultithreadlib.la \
   $(top_builddir)/transl/lgs_tran1dll/liblgs_tran1dll.la \
   $(top_builddir)/transl/lgs_tran2dll/liblgs_tran2dll.la \
   $(top_builddir)/transl/lgs_tran3dll/liblgs_tran3dll.la \
   $(top_builddir)/transl/lgs_tran4dll/liblgs_tran4dll.la \
   $(top_builddir)/logos_libs/halfnoundll/libhalfnoun.la \
   $(top_builddir)/logos_libs/PatternRecognition/libPatternRecognition.la \
   $(top_builddir)/logos_libs/SubjectMatterCodes/libSubjectMatterCodes.la \
   $(top_builddir)/TermMiner/libTermMiner.la \
   $(top_builddir)/lgs_base/liblgs_base.la \
   $(top_builddir)/logos_libs/lgssgml/liblgssgml.la \
   $(top_builddir)/logos_libs/newdebug/libnewdebug.la

# omit transl.cpp (no main needed)

AM_CPPFLAGS = -DTRANSLAPP

transl_LDADD = \
   $(top_builddir)/transl/AlignOutput.$(OBJEXT) \
   $(top_builddir)/transl/AsciiAlignTool.$(OBJEXT) \
   $(top_builddir)/transl/DiagCombine.$(OBJEXT) \
   $(top_builddir)/transl/Filter.$(OBJEXT) \
   $(top_builddir)/transl/PostTransl.$(OBJEXT) \
   $(top_builddir)/transl/PreTransl.$(OBJEXT) \
   $(top_builddir)/transl/translthrman.$(OBJEXT) \
   $(top_builddir)/transl/Utils.$(OBJEXT) \
   $(top_builddir)/lgs_db_io/liblgs_db_io.la \
   $(top_builddir)/transl/lgs_parsetran1dll/liblgs_parsetran1dll.la \
   $(top_builddir)/transl/lgs_parsetran2dll/liblgs_parsetran2dll.la \
   $(top_builddir)/transl/lgs_parsetran3dll/liblgs_parsetran3dll.la \
   $(top_builddir)/transl/lgs_parsetran4dll/liblgs_parsetran4dll.la \
   $(top_builddir)/transl/lgs_resdll/liblgs_res.la \
   $(top_builddir)/logos_libs/translutility/libtranslutility.la \
   $(top_builddir)/logos_libs/gerdem/libgerdem.la \
   $(top_builddir)/logos_libs/elision/libelision.la \
   $(top_builddir)/logos_libs/linguistic/liblinguistic.la \
   $(top_builddir)/logos_libs/patternmatcher/libpatternmatcher.la \
   $(top_builddir)/logos_libs/regex/libregex.la \
   $(top_builddir)/logos_libs/utility/libutility.la \
   $(top_builddir)/logos_libs/startrules/libstartrules.la \
   $(top_builddir)/logos_libs/ruleengine/libruleengine.la \
   $(top_builddir)/lgs_stemgen/liblgs_stemgen.la \
   $(top_builddir)/logos_libs/multithreadlib/libmultithreadlib.la \
   $(top_builddir)/transl/lgs_tran1dll/liblgs_tran1dll.la \
   $(top_builddir)/transl/lgs_tran2dll/liblgs_tran2dll.la \
   $(top_builddir)/transl/lgs_tran3dll/liblgs_tran3dll.la \
   $(top_builddir)/transl/lgs_tran4dll/liblgs_tran4dll.la \
   $(top_builddir)/logos_libs/halfnoundll/libhalfnoun.la \
   $(top_builddir)/logos_libs/PatternRecognition/libPatternRecognition.la \
   $(top_builddir)/logos_libs/SubjectMatterCodes/libSubjectMatterCodes.la \
   $(top_builddir)/TermMiner/libTermMiner.la \
   $(top_builddir)/lgs_base/liblgs_base.la \
   $(top_builddir)/logos_libs/lgssgml/liblgssgml.la \
   $(top_builddir)/logos_libs/newdebug/libnewdebug.la



#DIST_SUBDIRS = $(SUBDIRS) common borland
#EXTRA_DIST   = BUGS CHANGELOG LICENSE README TODO 
#	       doxyconfig.cheap doxyconfig.flop

# For ebrowse (Emacs)
BROWSE: 
	echo `find $(topsrc_dir) -name '*.h' -print ` \
	     `find $(topsrc_dir) \( -name '*.c' -o -name '*.cpp' \) -print` | \
	tr ' ' '\n'| ebrowse

sign: dist
	gpg --detach-sign --armor $(distdir).tar.gz
	md5sum $(distdir).tar.gz | gpg --clearsign > $(distdir).tar.gz.md5sum

# Dependencies: all source files of the distribution
# Should be done when the distribution is made
#doc: flopdoc cheapdoc

#flopdoc:
#	doxygen doxyconfig.flop

#cheapdoc:
#	doxygen doxyconfig.cheap

