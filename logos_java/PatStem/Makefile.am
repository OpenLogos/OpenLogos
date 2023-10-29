# -*- makefile -*-

AM_CPPFLAGS = @JAVAINCLUDE@

vpath %cpp $(top_srcdir)/logos_libs/linguistic $(top_srcdir)/logos_libs/gerdem
# WK: lib not usable

# lib_LTLIBRARIES = libjni_stemgen.la

# libjni_stemgen_la_SOURCES = \
# CompoundStatement.cpp \
# ConnectionManager.cpp \
# DBConnection.cpp \
# GenericStatement.cpp \
# ObjectStore.cpp \
# PatFinder.cpp \
# PatGeneratorImpl.cpp \
# PatgenQuery.cpp \
# PatStemGenerator.cpp \
# statement.cpp \
# StemGeneratorImpl.cpp \
# StemgenQuery.cpp \
# lsentence.cpp \
# targetunitconcretebuilder.cpp

# fake_main.cpp

#libjni_stemgen_la_LIBADD = \
#   $(top_builddir)/lgs_db_io/liblgs_db_io.la \
#   $(top_builddir)/logos_libs/linguistic/liblinguistic.la \
#   $(top_builddir)/logos_libs/translutility/libtranslutility.la \
#   $(top_builddir)/logos_libs/gerdem/libgerdem.la \
#   $(top_builddir)/logos_libs/entity/libentity.la \
#   $(top_builddir)/logos_libs/elision/libelision.la \
#   $(top_builddir)/logos_libs/patternmatcher/libpatternmatcher.la \
#   $(top_builddir)/logos_libs/PatternRecognition/libPatternRecognition.la \
#   $(top_builddir)/logos_libs/ruleengine/libruleengine.la \
#   $(top_builddir)/logos_libs/regex/libregex.la \
#   $(top_builddir)/logos_libs/newdebug/libnewdebug.la
#   $(top_builddir)/logos_libs/startrules/libstartrules.la \
#   $(top_builddir)/lgs_stemgen/liblgs_stemgen.la \
#   $(top_builddir)/logos_libs/halfnoundll/libhalfnoun.la \
#   $(top_builddir)/TermMiner/libTermMiner.la \
#   $(top_builddir)/lgs_base/liblgs_base.la \
#   $(top_builddir)/configdatafileinterface/libconfigdatafileinterface.la \
#   $(top_builddir)/logos_libs/utility/libutility.la

#  	$(top_srcdir)/transl/libtransl.la

bin_PROGRAMS = patstemgenerator

patstemgenerator_SOURCES = \
 CompoundStatement.cpp \
 ConnectionManager.cpp \
 DBConnection.cpp \
 GenericStatement.cpp \
 ObjectStore.cpp \
 PatFinder.cpp \
 PatGeneratorImpl.cpp \
 PatgenQuery.cpp \
 StemgenMain.cpp \
 statement.cpp \
 StemGeneratorImpl.cpp \
 StemgenQuery.cpp \
 fake_main.cpp\
 CompoundStatement.h \
 ConnectionManager.h \
 DBConnection.h \
 GenericStatement.h \
 ObjectStore.h \
 PatFinder.h \
 PatGeneratorImpl.h \
 PatgenQuery.h \
 statement.h \
 StemGeneratorImpl.h \
 StemgenQuery.h

nodist_patstemgenerator_SOURCES = lsentence.cpp \
  targetunitconcretebuilder.cpp

patstemgenerator_LDADD = \
	$(top_builddir)/lgs_db_io/liblgs_db_io.la \
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
	$(top_builddir)/logos_libs/newdebug/libnewdebug.la\
	$(top_builddir)/transl/libtransl.la
