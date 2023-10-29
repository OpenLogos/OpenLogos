/*
This file is part of OpenLogos/LogOSMaTrans.  Copyright (C) 2005 Globalware AG

OpenLogos/LogOSMaTrans has two licensing options:

The Commercial License, which allows you to provide commercial software
licenses to your customers or distribute Logos MT based applications or to use
LogOSMaTran for commercial purposes. This is for organizations who do not want
to comply with the GNU General Public License (GPL) in releasing the source
code for their applications as open source / free software.

The Open Source License allows you to offer your software under an open source
/ free software license to all who wish to use, modify, and distribute it
freely. The Open Source License allows you to use the software at no charge
under the condition that if you use OpenLogos/LogOSMaTran in an application you
redistribute, the complete source code for your application must be available
and freely redistributable under reasonable conditions. GlobalWare AG bases its
interpretation of the GPL on the Free Software Foundation's Frequently Asked
Questions.

OpenLogos is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE.  See the GNU General Public License for more details.

You should have received a copy of the License conditions along with this
program. If not, write to Globalware AG, Hospitalstraﬂe 6, D-99817 Eisenach.

Linux port modifications and additions by Bernd Kiefer, Walter Kasper,
Deutsches Forschungszentrum fuer kuenstliche Intelligenz (DFKI)
Stuhlsatzenhausweg 3, D-66123 Saarbruecken
*/

#ifndef _CacheManagerClass
#define _CacheManagerClass

#include <logos_include/logoscommon.h>

class CacheGssuData;
class CacheGphraseData;
class CacheTransferCodeData;
class CacheTargetData;
class CacheTransferCountData;
class CacheSemtabRuleData;
class CachePatternRulesData;
class CacheConstantCodeData;
class CacheTransferData;
class CacheStemgenRuleData;
class CacheTargetWordData;
class CacheConstantPointerData;
class CachePatData;
class CachePatData_pn_gc_nc;
class CachePatData_pn_nc_pc_tc;
class CachePatData_pn_gc_nc_deg;
class CachePatData_pn_gc_nc_pc_cc;
class CachePatData_pn_gc_nc_pc_tc;
class CachePatData_pn_gc_nc_cc;
class CachePatData_pn_gc_nc_dc_cc_deg;
class CachePatData_pn_deg;
class CachePatData_pn_nc_deg;
class CachePatData_pn_nc;
class CacheWordInPhraseData;
class CacheCompanyData;
class CacheSMCData;
class CacheInflectionData;
class CacheDerivedFormData;
class CacheIrregularStemData;
class CacheEndingLengthData;

#define NDATASET	30

#define GSSU_INDEX 0
#define GPHRASE_INDEX 1
#define TRANSFER_CODE_INDEX 2
#define TARGET_INDEX 3
#define TRANSFER_COUNT_INDEX 4
#define SEMTAB_RULE_INDEX 5
#define CONSTANT_CODE_INDEX 6
#define TRANSFER_INDEX 7
#define STEMGEN_RULE_INDEX 8
#define TARGET_WORD_INDEX 9
#define CONSTANT_POINTER_INDEX 10
#define PAT_INDEX 11
#define PAT_PN_GC_NC_INDEX 12
#define PAT_PN_NC_PC_TC_INDEX 13
#define PAT_PN_GC_NC_DEG_INDEX 14
#define PAT_PN_GC_NC_PC_CC_INDEX 15
#define PAT_PN_GC_NC_PC_TC_INDEX 16
#define PAT_PN_GC_NC_CC_INDEX 17
#define PAT_PN_GC_NC_DC_CC_DEG_INDEX 18
#define WORD_IN_PHRASE_INDEX 19
#define COMPANY_INDEX 20
#define SMC_INDEX 21
#define INFLECTION_INDEX 22
#define DERIVED_FORM_INDEX 23
#define PATTERN_RULES_INDEX 24
#define PAT_PN_DEG_INDEX 25
#define PAT_PN_NC_DEG_INDEX 26
#define IRREGULAR_STEM_INDEX 27
#define ENDING_LENGTH_INDEX 28
#define PAT_PN_NC_INDEX 29

#define NLANG 7
#define GERMAN_LANGUAGE_CODE 1
#define ENGLISH_LANGUAGE_CODE 2
#define FRENCH_LANGUAGE_CODE 3
#define SPANISH_LANGUAGE_CODE 4
#define ITALIAN_LANGUAGE_CODE 5
#define PORTUGUESE_LANGUAGE_CODE 6

#define LOAD_FROM_DISK_ONLY 0
#define LOAD_FROM_DB_ONLY 1
#define LOAD_TRY_BEST 2

struct stmts_combination {
	int index;
	double cost;	// memory
	double gain;	// summary time percentage
};

class SqlConnection;
class CacheData;

struct dataset_desc {
	char name[64];
	int state;
	CacheData *data;	// valid for server (CacheMgr) side only
};

static char *stmt_names[NDATASET] = {
	"GSSU", 
	"GPHRASE", 
	"TRANSFER_CODE",
	"TARGET", 
	"TRANSFER_COUNT",
	"SEMTAB_RULE",
	"CONSTANT_CODE",
	"TRANSFER",
	"STEMGEN_RULE",
	"TARGET_WORD",
	"CONSTANT_POINTER",
	"PAT",
	"PAT_PN_GC_NC",
	"PAT_PN_NC_PC_TC",
	"PAT_PN_GC_NC_DEG",
	"PAT_PN_GC_NC_PC_CC",
	"PAT_PN_GC_NC_PC_TC",
	"PAT_PN_GC_NC_CC",
	"PAT_PN_GC_NC_DC_CC_DEG",
	"WORD_IN_PHRASE",
	"COMPANY",
	"SMC",
	"INFLECTION",
	"DERIVED_FORM",
	"PATTERN_RULES",
	"PAT_PN_DEG",
	"PAT_PN_NC_DEG",
	"IRREGULAR_STEM",
	"ENDING_LENGTH"
	"PAT_PN_NC",
};

class CacheManager {

public:

	static CacheManager *singleton(LgsString name, LgsString uid,
		LgsString p, bool conn);

	static void setDebug(bool);
	static int calculateBestDatasetCombination(int srclang, int trglang,
		int nmegabytes, int *result);

	long getMemoryUsage();
	long getNRecords();
	long getLoadTime();

	bool loadPair(int src, int dst);
	bool loadPair(int src, int dst, int strategy);

	bool loadNeutrals();
	bool loadNeutrals(int strategy);
	bool loadSource(int lang);
	bool loadSource(int lang, int strategy);
	bool loadTarget(int lang);
	bool loadTarget(int lang, int strategy);
	bool loadSourceTarget(int src, int dst);
	bool loadSourceTarget(int src, int dst, int strategy);

// source
	bool loadGssuData(int lang, int strategy);
	bool loadGphraseData(int lang, int strategy);
	bool loadDerivedFormData(int lang, int strategy);
// target
	bool loadTransferCodeData(int lang, int strategy);
	bool loadTargetData(int lang, int strategy);
	bool loadTransferCountData(int lang, int strategy);
	bool loadConstantCodeData(int lang, int strategy);
	bool loadTransferData(int lang, int strategy);
	bool loadStemgenRuleData(int lang, int strategy);
	bool loadTargetWordData(int lang, int strategy);
	bool loadConstantPointerData(int lang, int strategy);
	bool loadInflectionData(int lang, int strategy);
// all langs
	bool loadPatData(int lang, int strategy);
	bool loadPat_pn_degData(int lang, int strategy);
	bool loadPat_pn_ncData(int lang, int strategy);
	bool loadPat_pn_nc_degData(int lang, int strategy);
// French target
	bool loadPat_pn_gc_ncData(int lang, int strategy);
	bool loadPat_pn_gc_nc_pc_tcData(int lang, int strategy);
// FR and DE
	bool loadPat_pn_nc_pc_tcData(int lang, int strategy);
	bool loadPat_pn_gc_nc_degData(int lang, int strategy);
	bool loadPat_pn_gc_nc_pc_ccData(int lang, int strategy);
	bool loadPat_pn_gc_nc_ccData(int lang, int strategy);
// German target specific
	bool loadPat_pn_gc_nc_dc_cc_degData(int lang, int strategy);
// lang-independent
	bool loadWordInPhraseData(int strategy);
	bool loadCompanyData(int strategy);
	bool loadSMCData(int strategy);
	bool loadIrregularStemData(int strategy);
	bool loadEndingLengthData(int strategy);

// now unloading
	void unloadAll();
	void unloadSource(int lang);
	void unloadTarget(int lang);
	void unloadSourceTarget(int src, int dst);
	void unloadDataset(int lang, int index);

// source
	void unloadGssuData(int lang);
	void unloadGphraseData(int lang);
// target
	void unloadTransferCodeData(int lang);
	void unloadTargetData(int lang);
	void unloadTransferCountData(int lang);
	void unloadSemtabRuleData(int lang);
	void unloadPatternRulesData(int lang);
	void unloadConstantCodeData(int lang);
	void unloadTransferData(int lang);
	void unloadStemgenRuleData(int lang);
	void unloadTargetWordData(int lang);
	void unloadConstantPointerData(int lang);
	void unloadInflectionData(int lang);
// all langs
	void unloadPatData(int lang);
	void unloadPat_pn_degData(int lang);
	void unloadPat_pn_nc_degData(int lang);
// French target
	void unloadPat_pn_gc_ncData(int lang);
	void unloadPat_pn_gc_nc_pc_tcData(int lang);
// FR and DE
	void unloadPat_pn_nc_pc_tcData(int lang);
	void unloadPat_pn_gc_nc_degData(int lang);
	void unloadPat_pn_gc_nc_pc_ccData(int lang);
	void unloadPat_pn_gc_nc_ccData(int lang);
// German target specific
	void unloadPat_pn_gc_nc_dc_cc_degData(int lang);
// lang-independent
	void unloadWordInPhraseData();
	void unloadCompanyData();
	void unloadSMCData();
	void unloadDerivedFormData(int lang);
	void unloadIrregularStemData();
	void unloadEndingLengthData();

// helpers
	CacheData *constructDataset(int index,int lang,bool disk,bool save);
	bool loadDataset(int index, int lang, int strategy);
// both source and target
	bool loadSemtabRuleData(int src, int dst);
	bool loadSemtabRuleData(int src, int dst, int strategy);
	bool loadPatternRulesData(int src, int dst);
	bool loadPatternRulesData(int src, int dst, int strategy);

private:
	CacheManager(LgsString sname, LgsString uid, LgsString pswd, bool conn);
	~CacheManager();
	static bool debug;
	static CacheManager *self;
	static struct dataset_desc dataGrid[NLANG][NDATASET];
	static struct dataset_desc semtabGrid[NLANG][NLANG];
	static struct dataset_desc patternRulesGrid[NLANG][NLANG];

	static SqlConnection *dbConnection;

	LgsString serviceName;
	LgsString username;
	LgsString password;

};

#endif // _CacheManagerClass
