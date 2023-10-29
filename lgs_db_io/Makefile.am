# -*- makefile -*-

vpath %.cpp $(top_srcdir)/logos_libs/dbcache \
	$(top_srcdir)/logos_libs/SubjectMatterCodes \
	$(top_srcdir)/logos_libs/dbms2restran \
	$(top_srcdir)/logos_libs/sql \
	$(top_srcdir)/logos_libs/semtabrule \
	$(top_srcdir)/logos_libs/gerdem \
	$(top_srcdir)/logos_libs/odbcsql \
	$(top_srcdir)/logos_libs/linguistic \
	$(top_srcdir)/logos_libs/regex

lib_LTLIBRARIES = liblgs_db_io.la

liblgs_db_io_la_LIBADD = \
 $(top_builddir)/logos_libs/entity/libentity.la \
 $(top_builddir)/configdatafileinterface/libconfigdatafileinterface.la \
 $(top_builddir)/logos_libs/odbcsql/libodbcsql.la

liblgs_db_io_la_SOURCES = \
	dbinstances.cpp dbinstances.h \
	interactivetranslstatus.cpp interactivetranslstatus.h \
	jobcntrlarginterface.cpp jobcntrlarginterface.h \
	jobcontrolarguments.cpp jobcontrolarguments.h \
	lgsdbcommonobjects.cpp lgsdbcommonobjects.h \
	pattablequery.cpp pattablequery.h \
	querytodictionary.cpp querytodictionary.h \
	serverproperties.cpp serverproperties.h \
	stemgenrulequery.cpp stemgenrulequery.h \
	memory_mapper.cpp \
	dllmain.cpp

nodist_liblgs_db_io_la_SOURCES = \
	SubjectMatterCode.cpp \
	CacheCompanyData.cpp \
	CacheCompanyQuery.cpp \
	CacheConstantCodeData.cpp \
	CacheConstantCodeQuery.cpp \
	CacheConstantPointerData.cpp \
	CacheConstantPointerQuery.cpp \
	CacheData.cpp \
	CacheDerivedFormData.cpp \
	CacheDerivedFormQuery.cpp \
	CacheEndingLengthData.cpp \
	CacheEndingLengthQuery.cpp \
	CacheGphraseData.cpp \
	CacheGphraseQuery.cpp \
	CacheGssuData.cpp \
	CacheGssuQuery.cpp \
	CacheGwordQuery.cpp \
	CacheHashTab.cpp \
	CacheInflectionData.cpp \
	CacheInflectionQuery.cpp \
	CacheIrregularStemData.cpp \
	CacheIrregularStemQuery.cpp \
	CacheKey.cpp \
	CachePatData.cpp \
	CachePatData_pn_deg.cpp \
	CachePatData_pn_gc_nc.cpp \
	CachePatData_pn_gc_nc_cc.cpp \
	CachePatData_pn_gc_nc_dc_cc_deg.cpp \
	CachePatData_pn_gc_nc_deg.cpp \
	CachePatData_pn_gc_nc_pc_cc.cpp \
	CachePatData_pn_gc_nc_pc_tc.cpp \
	CachePatData_pn_nc.cpp \
	CachePatData_pn_nc_deg.cpp \
	CachePatData_pn_nc_pc_tc.cpp \
	CachePatQuery.cpp \
	CachePatQuery_pn_deg.cpp \
	CachePatQuery_pn_gc_nc.cpp \
	CachePatQuery_pn_gc_nc_cc.cpp \
	CachePatQuery_pn_gc_nc_dc_cc_deg.cpp \
	CachePatQuery_pn_gc_nc_deg.cpp \
	CachePatQuery_pn_gc_nc_pc_cc.cpp \
	CachePatQuery_pn_gc_nc_pc_tc.cpp \
	CachePatQuery_pn_nc.cpp \
	CachePatQuery_pn_nc_deg.cpp \
	CachePatQuery_pn_nc_pc_tc.cpp \
	CachePatternRulesData.cpp \
	CachePatternRulesQuery.cpp \
	CacheSemtabRuleData.cpp \
	CacheSemtabRuleQuery.cpp \
	CacheSMCData.cpp \
	CacheSMCQuery.cpp \
	CacheStemgenRuleData.cpp \
	CacheStemgenRuleQuery.cpp \
	CacheTargetData.cpp \
	CacheTargetQuery.cpp \
	CacheTargetWordData.cpp \
	CacheTargetWordQuery.cpp \
	CacheTransferCodeData.cpp \
	CacheTransferCodeQuery.cpp \
	CacheTransferCountData.cpp \
	CacheTransferCountQuery.cpp \
	CacheTransferData.cpp \
	CacheTransferQuery.cpp \
	CacheWordInPhraseData.cpp \
	CacheWordInPhraseQuery.cpp \
	constantcodebuilder.cpp \
	constantpointerretriever.cpp \
	genderretriever.cpp \
	gettargetcodes.cpp \
	gettargetwordphrase.cpp \
	targetretriever.cpp \
	transfercode.cpp \
	transfercodebuilder.cpp \
	transferretriever.cpp \
	constantpointerquery.cpp \
	gphrasequery.cpp \
	gssuquery.cpp \
	gwordcapquery.cpp \
	gwordquery.cpp \
	irregularstemquery.cpp \
	phraseidquery.cpp \
	targetquery.cpp \
	transfercountquery.cpp \
	transferquery.cpp \
	wordinphrasequery.cpp \
	wordphrasequery.cpp \
	semtabrule.cpp \
	semtabrulebuilder.cpp \
	SMRCache.cpp \
	spio.cpp \
	transfer.cpp \
	charutil.cpp