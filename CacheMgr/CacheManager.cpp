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

#include "CacheManager.h"

//#include <logos_libs/orasql/OracleConnection.h>
#include <logos_libs/odbcsql/odbcconnection.h>
#include <logos_libs/sql/sqlconnection.h>

#include <logos_libs/dbcache/CacheData.h>

#include <logos_libs/dbcache/CacheGssuData.h>
#include <logos_libs/dbcache/CacheGphraseData.h>
#include <logos_libs/dbcache/CacheTransferCodeData.h>
#include <logos_libs/dbcache/CacheTargetData.h>
#include <logos_libs/dbcache/CacheTransferCountData.h>
#include <logos_libs/dbcache/CacheSemtabRuleData.h>
#include <logos_libs/dbcache/CachePatternRulesData.h>
#include <logos_libs/dbcache/CacheConstantCodeData.h>
#include <logos_libs/dbcache/CacheTransferData.h>
#include <logos_libs/dbcache/CacheStemgenRuleData.h>
#include <logos_libs/dbcache/CacheTargetWordData.h>
#include <logos_libs/dbcache/CacheConstantPointerData.h>
#include <logos_libs/dbcache/CachePatData.h>
#include <logos_libs/dbcache/CachePatData_pn_gc_nc.h>
#include <logos_libs/dbcache/CachePatData_pn_nc_pc_tc.h>
#include <logos_libs/dbcache/CachePatData_pn_gc_nc_deg.h>
#include <logos_libs/dbcache/CachePatData_pn_gc_nc_pc_cc.h>
#include <logos_libs/dbcache/CachePatData_pn_gc_nc_pc_tc.h>
#include <logos_libs/dbcache/CachePatData_pn_gc_nc_cc.h>
#include <logos_libs/dbcache/CachePatData_pn_gc_nc_dc_cc_deg.h>
#include <logos_libs/dbcache/CacheWordInPhraseData.h>
#include <logos_libs/dbcache/CacheCompanyData.h>
#include <logos_libs/dbcache/CacheSMCData.h>
#include <logos_libs/dbcache/CacheInflectionData.h>
#include <logos_libs/dbcache/CacheDerivedFormData.h>
#include <logos_libs/dbcache/CachePatData_pn_deg.h>
#include <logos_libs/dbcache/CachePatData_pn_nc_deg.h>
#include <logos_libs/dbcache/CacheIrregularStemData.h>
#include <logos_libs/dbcache/CacheEndingLengthData.h>
#include <logos_libs/dbcache/CachePatData_pn_nc.h>

#include <stdio.h>
#ifdef _MSC_VER
#include <windows.h>
#else
#include "logos_include/windows.h"
#endif

bool CacheManager::debug = false;
CacheManager *CacheManager::self = NULL;
SqlConnection * CacheManager::dbConnection = NULL;
struct dataset_desc CacheManager::dataGrid[NLANG][NDATASET];
struct dataset_desc CacheManager::semtabGrid[NLANG][NLANG];
struct dataset_desc CacheManager::patternRulesGrid[NLANG][NLANG];

static unsigned char hda[256];	// oracle host data area, according to
								// oracle docs has to be static to allow
								// multi-threading

CacheManager::CacheManager(LgsString sname, LgsString uid, 
						   LgsString pwd, bool connectNeeded) {
	if(self) {
		printf("CM already running\n");
		fflush(stdout);
		return;
	}

	serviceName = sname;
	username = uid;
	password = pwd;
	if(connectNeeded) {
		if(debug) {
			printf("Connecting to service \"%s\" as \"%s/%s\"...",
				serviceName.c_str(), username.c_str(),
				password.c_str());
		}

		dbConnection = new ODBCConnection(); //new OracleConnection(hda);

		dbConnection->Open(serviceName, username, password);
		if(debug) { printf("done\n"); fflush(stdout); }
	} else
		dbConnection = NULL;

	self = this;

	memset(dataGrid, 0, sizeof(dataGrid));
	memset(semtabGrid, 0, sizeof(semtabGrid));
	memset(patternRulesGrid, 0, sizeof(semtabGrid));

}
	
CacheManager * CacheManager::singleton(LgsString a, LgsString b, LgsString c,
									   bool conn) {
	if(self)
		return self;
	return new CacheManager(a, b, c, conn);
}

CacheManager::~CacheManager() {
	unloadAll();
	if(dbConnection) {
		dbConnection->Close();
		delete dbConnection;
	}
}

void
CacheManager::setDebug(bool n) {
	debug = n;
}

bool
CacheManager::loadPair(int src, int dst) {
	return loadPair(src, dst, LOAD_TRY_BEST);
}
bool
CacheManager::loadPair(int src, int dst, int strategy) {
	bool ret = true;
	ret &= loadSource(src, strategy);
	ret &= loadTarget(dst, strategy);
	ret &= loadSourceTarget(src, dst, strategy);
	return ret;
}

bool
CacheManager::loadDataset(int index, int lang, int strategy) {

	dataGrid[lang][index].state = DATASET_STATE_LOADING;

	CacheData *d;
	bool disk = true; bool save = false;

	if(strategy==LOAD_FROM_DB_ONLY) {
		disk = false; save = true;
		d = constructDataset(index, lang, disk, save);
	} else if(strategy==LOAD_FROM_DISK_ONLY) {
		disk = true; save = false;
		d = constructDataset(index, lang, disk, save);
	} else {
		disk = true; save = false;
		d = constructDataset(index, lang, disk, save);
		if(!d || (d && !d->isValid())) {
			disk = false; save = true;
			d = constructDataset(index, lang, disk, save);
		}

	}
	if(!d || (d && !d->isValid())) {
		dataGrid[lang][index].state = DATASET_STATE_EMPTY;
		if(d)
			delete d;
		return false;
	}
	dataGrid[lang][index].state = DATASET_STATE_READY;
	dataGrid[lang][index].data = d;

	return true;
}

// semtab loading is different because it depends both on source and target
bool CacheManager::loadSemtabRuleData(int src, int dst) {
	return loadSemtabRuleData(src, dst, LOAD_TRY_BEST);
}
bool CacheManager::loadSemtabRuleData(int src, int dst, int strategy) {
	semtabGrid[src][dst].state = DATASET_STATE_LOADING;

	CacheSemtabRuleData *d;
	bool disk, save;

	if(strategy==LOAD_FROM_DB_ONLY) {
		disk = false; save = true;
		d = new CacheSemtabRuleData(dbConnection, src, dst, 1, 
				disk, save);
	} else if(strategy==LOAD_FROM_DISK_ONLY) {
		disk = true; save = false;
		d = new CacheSemtabRuleData(dbConnection, src, dst, 1, 
				disk, save);
	} else {
		disk = true; save = false;
		d = new CacheSemtabRuleData(dbConnection, src, dst, 1, 
				disk, save);
		if(!d || (d && !d->isValid())) {
			disk = false; save = true;
			d = new CacheSemtabRuleData(dbConnection, src, dst, 1, 
				disk, save);
		}
	}
	if(!d || (d && !d->isValid())) {
		semtabGrid[dst][dst].state = DATASET_STATE_EMPTY;
		if(d)
			delete d;
		return false;
	}
	semtabGrid[src][dst].state = DATASET_STATE_READY;
	semtabGrid[src][dst].data = d;

	return true;
}

// patternRules loading is different because it depends both on source and target
bool CacheManager::loadPatternRulesData(int src, int dst) {
	return loadPatternRulesData(src, dst, LOAD_TRY_BEST);
}
bool CacheManager::loadPatternRulesData(int src, int dst, int strategy) {
	patternRulesGrid[src][dst].state = DATASET_STATE_LOADING;

	CachePatternRulesData *d;
	bool disk, save;

	if(strategy==LOAD_FROM_DB_ONLY) {
		disk = false; save = true;
		d = new CachePatternRulesData(dbConnection, src, dst, 1, 
				disk, save);
	} else if(strategy==LOAD_FROM_DISK_ONLY) {
		disk = true; save = false;
		d = new CachePatternRulesData(dbConnection, src, dst, 1, 
				disk, save);
	} else {
		disk = true; save = false;
		d = new CachePatternRulesData(dbConnection, src, dst, 1, 
				disk, save);
		if(!d || (d && !d->isValid())) {
			disk = false; save = true;
			d = new CachePatternRulesData(dbConnection, src, dst, 1, 
				disk, save);
		}
	}
	if(!d || (d && !d->isValid())) {
		patternRulesGrid[dst][dst].state = DATASET_STATE_EMPTY;
		if(d)
			delete d;
		return false;
	}
	patternRulesGrid[src][dst].state = DATASET_STATE_READY;
	patternRulesGrid[src][dst].data = d;

	return true;
}

CacheData *
CacheManager::constructDataset(int index, int lang, bool disk, bool save) {
	CacheData *d;
	switch(index) {

		case GSSU_INDEX:
			d = new CacheGssuData(dbConnection, lang, 1, disk, save);
			break;
		case GPHRASE_INDEX:
			d = new CacheGphraseData(dbConnection, lang, 1, disk, save);
			break;
		case INFLECTION_INDEX:
			d = new CacheInflectionData(dbConnection, lang, 
				1, disk, save);
			break;

		case TRANSFER_CODE_INDEX:
			d = new CacheTransferCodeData(dbConnection, lang, 1, disk, save);
			break;
		case TARGET_INDEX:
			d = new CacheTargetData(dbConnection, lang, 1, disk, save);
			break;
		case TRANSFER_COUNT_INDEX:
			d = new CacheTransferCountData(dbConnection, lang, 1,disk,save);
			break;
		case CONSTANT_CODE_INDEX:
			d = new CacheConstantCodeData(dbConnection, lang,1,disk,save);
			break;
		case TRANSFER_INDEX:
			d = new CacheTransferData(dbConnection, lang, 1, disk, save);
			break;
		case STEMGEN_RULE_INDEX:
			d = new CacheStemgenRuleData(dbConnection, lang, 1, disk, save);
			break;
		case TARGET_WORD_INDEX:
			d = new CacheTargetWordData(dbConnection, lang, 1, disk, save);
			break;
		case CONSTANT_POINTER_INDEX:
			d = new CacheConstantPointerData(dbConnection,lang,1,disk,save);
			break;
		case PAT_INDEX:
			d = new CachePatData(dbConnection, lang, 1, disk, save);
			break;
		case PAT_PN_GC_NC_INDEX:
			d = new CachePatData_pn_gc_nc(dbConnection, lang, 1, disk,save);
			break;
		case PAT_PN_GC_NC_PC_TC_INDEX:
			d = new CachePatData_pn_gc_nc_pc_tc(dbConnection, lang, 1, 
				disk, save);
			break;
		case PAT_PN_NC_PC_TC_INDEX:
			d = new CachePatData_pn_nc_pc_tc(dbConnection, lang, 1, 
				disk, save);
			break;
		case PAT_PN_GC_NC_DEG_INDEX:
			d = new CachePatData_pn_gc_nc_deg(dbConnection, lang, 1, 
				disk, save);
			break;
		case PAT_PN_GC_NC_PC_CC_INDEX:
			d = new CachePatData_pn_gc_nc_pc_cc(dbConnection, lang, 1, 
				disk, save);
			break;
		case PAT_PN_GC_NC_CC_INDEX:
			d = new CachePatData_pn_gc_nc_cc(dbConnection, lang, 1, 
				disk, save);
			break;
		case PAT_PN_GC_NC_DC_CC_DEG_INDEX:
			d = new CachePatData_pn_gc_nc_dc_cc_deg(dbConnection, lang, 
				1, disk, save);
			break;
		case WORD_IN_PHRASE_INDEX:
			d = new CacheWordInPhraseData(dbConnection, 0, 1,disk, save);
			break;
		case COMPANY_INDEX:
			d = new CacheCompanyData(dbConnection, 0, 1,disk, save);
			break;
		case SMC_INDEX:
			d = new CacheSMCData(dbConnection, 0, 1,disk, save);
			break;
		case DERIVED_FORM_INDEX:
			d = new CacheDerivedFormData(dbConnection, lang, 1,disk, save);
			break;
		case PAT_PN_DEG_INDEX:
			d = new CachePatData_pn_deg(dbConnection, lang, 1, 
				disk, save);
			break;
		case PAT_PN_NC_DEG_INDEX:
			d = new CachePatData_pn_nc_deg(dbConnection, lang, 1, 
				disk, save);
			break;
		case IRREGULAR_STEM_INDEX:
			d = new CacheIrregularStemData(dbConnection, 0, 1,disk, save);
			break;
		case ENDING_LENGTH_INDEX:
			d = new CacheEndingLengthData(dbConnection, 0, 1,disk, save);
			break;
		case PAT_PN_NC_INDEX:
			d = new CachePatData_pn_nc(dbConnection, lang, 1, disk, save);
			break;
		default:
			d = NULL; break;
	}

	return d;
}

bool CacheManager::loadGssuData(int lang, int strategy) {
	return loadDataset(GSSU_INDEX, lang, strategy);
}

bool CacheManager::loadGphraseData(int lang, int strategy) {
	return loadDataset(GPHRASE_INDEX, lang, strategy);
}

bool CacheManager::loadTransferCodeData(int lang, int strategy) {
	return loadDataset(TRANSFER_CODE_INDEX, lang, strategy);
}

bool CacheManager::loadTargetData(int lang, int strategy) {
	return loadDataset(TARGET_INDEX, lang, strategy);
}

bool CacheManager::loadTransferCountData(int lang, int strategy) {
	return loadDataset(TRANSFER_COUNT_INDEX, lang, strategy);
}

bool CacheManager::loadConstantCodeData(int lang, int strategy) {
	return loadDataset(CONSTANT_CODE_INDEX, lang, strategy);
}

bool CacheManager::loadTransferData(int lang, int strategy) {
	return loadDataset(TRANSFER_INDEX, lang, strategy);
}

bool CacheManager::loadStemgenRuleData(int lang, int strategy) {
	return loadDataset(STEMGEN_RULE_INDEX, lang, strategy);
}

bool CacheManager::loadTargetWordData(int lang, int strategy) {
	return loadDataset(TARGET_WORD_INDEX, lang, strategy);
}

bool CacheManager::loadConstantPointerData(int lang, int strategy) {
	return loadDataset(CONSTANT_POINTER_INDEX, lang, strategy);
}

bool CacheManager::loadPatData(int lang, int strategy) {
	return loadDataset(PAT_INDEX, lang, strategy);
}

bool CacheManager::loadPat_pn_gc_ncData(int lang, int strategy) {
	return loadDataset(PAT_PN_GC_NC_INDEX, lang, strategy);
}
bool CacheManager::loadPat_pn_gc_nc_pc_tcData(int lang, int strategy) {
	return loadDataset(PAT_PN_GC_NC_PC_TC_INDEX, lang, strategy);
}
bool CacheManager::loadPat_pn_nc_pc_tcData(int lang, int strategy) {
	return loadDataset(PAT_PN_NC_PC_TC_INDEX, lang, strategy);
}
bool CacheManager::loadPat_pn_gc_nc_degData(int lang, int strategy) {
	return loadDataset(PAT_PN_GC_NC_DEG_INDEX, lang, strategy);
}
bool CacheManager::loadPat_pn_gc_nc_pc_ccData(int lang, int strategy) {
	return loadDataset(PAT_PN_GC_NC_PC_CC_INDEX, lang, strategy);
}
bool CacheManager::loadPat_pn_gc_nc_ccData(int lang, int strategy) {
	return loadDataset(PAT_PN_GC_NC_CC_INDEX, lang, strategy);
}
bool CacheManager::loadPat_pn_gc_nc_dc_cc_degData(int lang, int strategy) {
	return loadDataset(PAT_PN_GC_NC_DC_CC_DEG_INDEX, lang, strategy);
}
bool CacheManager::loadPat_pn_degData(int lang, int strategy) {
	return loadDataset(PAT_PN_DEG_INDEX, lang, strategy);
}
bool CacheManager::loadPat_pn_nc_degData(int lang, int strategy) {
	return loadDataset(PAT_PN_NC_DEG_INDEX, lang, strategy);
}
bool CacheManager::loadPat_pn_ncData(int lang, int strategy) {
	return loadDataset(PAT_PN_NC_INDEX, lang, strategy);
}

bool CacheManager::loadInflectionData(int lang, int strategy) {
	return loadDataset(INFLECTION_INDEX, lang, strategy);
}

bool CacheManager::loadWordInPhraseData(int strategy) {
// "lang" parameter is 0 because WordInPhrase query does not
// depend on language.
	return loadDataset(WORD_IN_PHRASE_INDEX, 0, strategy);
}

bool CacheManager::loadCompanyData(int strategy) {
// "lang" parameter is 0 because Company query does not
// depend on language.
	return loadDataset(COMPANY_INDEX, 0, strategy);
}

bool CacheManager::loadSMCData(int strategy) {
// "lang" parameter is 0 because SMC query does not
// depend on language.
	return loadDataset(SMC_INDEX, 0, strategy);
}

bool CacheManager::loadDerivedFormData(int lang, int strategy) {
// "lang" parameter is 0 because SMC query does not
// depend on language.
	return loadDataset(DERIVED_FORM_INDEX, lang, strategy);
}

bool CacheManager::loadIrregularStemData(int strategy) {
// "lang" parameter is 0 because IrregularStem query does not
// depend on language.
	return loadDataset(IRREGULAR_STEM_INDEX, 0, strategy);
}

bool CacheManager::loadEndingLengthData(int strategy) {
// "lang" parameter is 0 because IrregularStem query does not
// depend on language.
	return loadDataset(ENDING_LENGTH_INDEX, 0, strategy);
}

void CacheManager::unloadAll() {
	CacheData *d;
	int i, j;
	for(i=0;i<NLANG;i++) {
		for(j=0;j<NDATASET;j++) {
			d = (CacheData *)dataGrid[i][j].data;
			if(d)
				delete d;
			dataGrid[i][j].data = NULL;
		}
	}
	for(i=1;i<NLANG;i++) {
		for(j=1;j<NLANG;j++) {
			d = (CacheData *)semtabGrid[i][j].data;
			if(d)
				delete d;
			semtabGrid[i][j].data = NULL;
		}
	}
	for(i=1;i<NLANG;i++) {
		for(j=1;j<NLANG;j++) {
			d = (CacheData *)patternRulesGrid[i][j].data;
			if(d)
				delete d;
			patternRulesGrid[i][j].data = NULL;
		}
	}
}

void
CacheManager::unloadDataset(int lang, int index) {
	CacheData *d = (CacheData *)dataGrid[lang][index].data;
	if(d)
		delete d;
	dataGrid[lang][index].data = NULL;
}

void CacheManager::unloadGssuData(int lang) { unloadDataset(lang, GSSU_INDEX); }
void CacheManager::unloadGphraseData(int lang) { 
	unloadDataset(lang, GPHRASE_INDEX); }
void CacheManager::unloadTransferCodeData(int lang) { 
	unloadDataset(lang, TRANSFER_CODE_INDEX); }
void CacheManager::unloadTargetData(int lang) { 
	unloadDataset(lang, TARGET_INDEX); }
void CacheManager::unloadTransferCountData(int lang) { 
	unloadDataset(lang, TRANSFER_COUNT_INDEX); }
void CacheManager::unloadSemtabRuleData(int lang) { 
	unloadDataset(lang, SEMTAB_RULE_INDEX); }
void CacheManager::unloadPatternRulesData(int lang) { 
	unloadDataset(lang, PATTERN_RULES_INDEX); }
void CacheManager::unloadConstantCodeData(int lang) { 
	unloadDataset(lang, CONSTANT_CODE_INDEX); }
void CacheManager::unloadTransferData(int lang) { 
	unloadDataset(lang, TRANSFER_INDEX); }
void CacheManager::unloadStemgenRuleData(int lang) { 
	unloadDataset(lang, STEMGEN_RULE_INDEX); }
void CacheManager::unloadTargetWordData(int lang) { 
	unloadDataset(lang, TARGET_WORD_INDEX); }
void CacheManager::unloadConstantPointerData(int lang) { 
	unloadDataset(lang, CONSTANT_POINTER_INDEX); }
void CacheManager::unloadPatData(int lang) { unloadDataset(lang, PAT_INDEX); }
void CacheManager::unloadPat_pn_gc_ncData(int lang) { 
	unloadDataset(lang, PAT_PN_GC_NC_INDEX); }
void CacheManager::unloadPat_pn_gc_nc_pc_tcData(int lang) { 
	unloadDataset(lang, PAT_PN_GC_NC_PC_TC_INDEX); }
void CacheManager::unloadPat_pn_nc_pc_tcData(int lang) { 
	unloadDataset(lang, PAT_PN_NC_PC_TC_INDEX); }
void CacheManager::unloadPat_pn_gc_nc_degData(int lang) { 
	unloadDataset(lang, PAT_PN_GC_NC_DEG_INDEX); }
void CacheManager::unloadPat_pn_gc_nc_pc_ccData(int lang) { 
	unloadDataset(lang, PAT_PN_GC_NC_PC_CC_INDEX); }
void CacheManager::unloadPat_pn_gc_nc_ccData(int lang) { 
	unloadDataset(lang, PAT_PN_GC_NC_CC_INDEX); }
void CacheManager::unloadPat_pn_gc_nc_dc_cc_degData(int lang) { 
	unloadDataset(lang, PAT_PN_GC_NC_DC_CC_DEG_INDEX); }
void CacheManager::unloadPat_pn_degData(int lang) { 
	unloadDataset(lang, PAT_PN_DEG_INDEX); }
void CacheManager::unloadPat_pn_nc_degData(int lang) { 
	unloadDataset(lang, PAT_PN_NC_DEG_INDEX); }
void CacheManager::unloadInflectionData(int lang) { 
	unloadDataset(lang, INFLECTION_INDEX); }
void CacheManager::unloadWordInPhraseData() { 
	unloadDataset(0, WORD_IN_PHRASE_INDEX);}
void CacheManager::unloadCompanyData() { 
	unloadDataset(0, COMPANY_INDEX);
}
void CacheManager::unloadSMCData() { 
	unloadDataset(0, SMC_INDEX);
}
void CacheManager::unloadDerivedFormData(int lang) { 
	unloadDataset(lang, DERIVED_FORM_INDEX);
}
void CacheManager::unloadIrregularStemData() { 
	unloadDataset(0, IRREGULAR_STEM_INDEX);
}
void CacheManager::unloadEndingLengthData() { 
	unloadDataset(0, ENDING_LENGTH_INDEX);
}

// --------------------------------------------------------
bool CacheManager::loadNeutrals() {
	return loadNeutrals(LOAD_TRY_BEST);
}

bool CacheManager::loadNeutrals(int strategy) {
	printf("\nLoading Language-Neutral Datasets:\n");
	bool ret;
	ret = loadWordInPhraseData(strategy);

	ret &= loadCompanyData(strategy);

	ret &= loadSMCData(strategy);

	ret &= loadIrregularStemData(strategy);
	ret &= loadEndingLengthData(strategy);

	printf("\n");
	return ret;
}

bool CacheManager::loadSourceTarget(int src, int dst) {
	return loadSourceTarget(src, dst, LOAD_TRY_BEST);
}
bool CacheManager::loadSourceTarget(int src, int dst, int strategy) {
	bool ret;
	printf("Loading Semtab(%d,%d): ", src, dst);
	ret = loadSemtabRuleData(src, dst, strategy);
	printf("\n");
	printf("Loading PatternRules(%d,%d): ", src, dst);
	ret = loadPatternRulesData(src, dst, strategy);
	printf("\n");
	return ret;
}

bool CacheManager::loadSource(int lang) {
	return loadSource(lang, LOAD_TRY_BEST);
}

bool CacheManager::loadSource(int lang, int strategy) {
	printf("\nLoading Source Language (%d):\n", lang);
	bool ret = true;
	ret &= loadGssuData(lang, strategy); // reason is selection order
	ret &= loadGphraseData(lang, strategy);
	ret &= loadInflectionData(lang, strategy);
	ret &= loadDerivedFormData(lang, strategy);
	printf("\n");
	return ret;
}

bool CacheManager::loadTarget(int lang) {
	return loadTarget(lang, LOAD_TRY_BEST);
}

bool CacheManager::loadTarget(int lang, int strategy) {
	printf("\nLoading Target Language (%d):\n", lang);

	bool ret = true;

	ret = ret && loadTransferCodeData(lang, strategy);
	ret = ret && loadTargetData(lang, strategy);
	ret = ret && loadTransferCountData(lang, strategy);
	ret = ret && loadConstantCodeData(lang, strategy);
	ret = ret && loadTransferData(lang, strategy);
	ret = ret && loadStemgenRuleData(lang, strategy);
	ret = ret && loadTargetWordData(lang, strategy);
	ret = ret && loadConstantPointerData(lang, strategy);
	ret = ret && loadPatData(lang, strategy);
	ret = ret && loadPat_pn_gc_ncData(lang, strategy);
	ret = ret && loadPat_pn_gc_nc_pc_tcData(lang, strategy);
	ret = ret && loadPat_pn_nc_pc_tcData(lang, strategy);	
	ret = ret && loadPat_pn_gc_nc_degData(lang, strategy);
	ret = ret && loadPat_pn_gc_nc_pc_ccData(lang, strategy);
	ret = ret && loadPat_pn_gc_nc_ccData(lang, strategy);
	ret = ret && loadPat_pn_gc_nc_dc_cc_degData(lang, strategy);
	ret = ret && loadPat_pn_degData(lang, strategy);
	ret = ret && loadPat_pn_nc_degData(lang, strategy);
	ret = ret && loadPat_pn_ncData(lang, strategy);
	printf("\n");

	return ret;
}

// ---------------------------------------------
long CacheManager::getMemoryUsage() {
	int i, j;
	long memusage=0;
	for(i=0;i<NLANG;i++) {
		for(j=0;j<NDATASET;j++) {
			CacheData *d = dataGrid[i][j].data;
			if(d==NULL) continue;
			memusage += d->getMemoryUsage();
		}
	}
	for(i=1;i<NLANG;i++) {
		for(j=1;j<NLANG;j++) {
			CacheSemtabRuleData *st = (CacheSemtabRuleData *)
				semtabGrid[i][j].data;
			if(st!=NULL)
				memusage += st->getMemoryUsage();
		}
	}
	for(i=1;i<NLANG;i++) {
		for(j=1;j<NLANG;j++) {
			CachePatternRulesData *pr = (CachePatternRulesData *)
				patternRulesGrid[i][j].data;
			if(pr!=NULL)
				memusage += pr->getMemoryUsage();
		}
	}
	return memusage;
}

long CacheManager::getLoadTime() {
	int i, j;
	long loadtime=0;
	for(i=0;i<NLANG;i++) {
		for(j=0;j<NDATASET;j++) {
			CacheData *d = dataGrid[i][j].data;
			if(d==NULL) continue;
			loadtime += d->getLoadTime();
		}
	}
	for(i=1;i<NLANG;i++) {
		for(j=1;j<NLANG;j++) {
			CacheSemtabRuleData *st = (CacheSemtabRuleData *)
				semtabGrid[i][j].data;
			if(st==NULL) continue;
			loadtime += st->getLoadTime();
		}
	}
	for(i=1;i<NLANG;i++) {
		for(j=1;j<NLANG;j++) {
			CachePatternRulesData *pr = (CachePatternRulesData *)
				patternRulesGrid[i][j].data;
			if(pr==NULL) continue;
			loadtime += pr->getLoadTime();
		}
	}
	return loadtime;
}

long CacheManager::getNRecords() {
	int i, j;
	long n=0;
	for(i=0;i<NLANG;i++) {
		for(j=0;j<NDATASET;j++) {
			CacheData *d = dataGrid[i][j].data;
			if(d==NULL) continue;
			n += d->getNRows();
		}
	}
	for(i=1;i<NLANG;i++) {
		for(j=1;j<NLANG;j++) {
			CacheSemtabRuleData *st = (CacheSemtabRuleData *)
				semtabGrid[i][j].data;
			if(st==NULL) continue;
			n += st->getNRows();
		}
	}
	for(i=1;i<NLANG;i++) {
		for(j=1;j<NLANG;j++) {
			CachePatternRulesData *pr = (CachePatternRulesData *)
				patternRulesGrid[i][j].data;
			if(pr==NULL) continue;
			n += pr->getNRows();
		}
	}
	return n;
}

int CacheManager::calculateBestDatasetCombination(int src, int trg,
										int nmegabytes, int *opt) {
	double costs[NLANG][NDATASET];
	double gains[NLANG][NDATASET];

	memset(costs, 0, sizeof(double)*NLANG*NDATASET);
	memset(gains, 0, sizeof(double)*NLANG*NDATASET);

	costs[ENGLISH_LANGUAGE_CODE][GSSU_INDEX] = 8.3;
	costs[ENGLISH_LANGUAGE_CODE][GPHRASE_INDEX] = 18.9;
	costs[FRENCH_LANGUAGE_CODE][TRANSFER_CODE_INDEX] = 23.1;
	costs[FRENCH_LANGUAGE_CODE][TARGET_INDEX] = 16.4;
	costs[FRENCH_LANGUAGE_CODE][TRANSFER_COUNT_INDEX] = 12.9;
	costs[FRENCH_LANGUAGE_CODE][SEMTAB_RULE_INDEX] = 4.4;
	costs[FRENCH_LANGUAGE_CODE][PATTERN_RULES_INDEX] = 0.01;
	costs[FRENCH_LANGUAGE_CODE][CONSTANT_CODE_INDEX] = 0.7;
	costs[FRENCH_LANGUAGE_CODE][TRANSFER_INDEX] = 15.8;
	costs[FRENCH_LANGUAGE_CODE][STEMGEN_RULE_INDEX] = 0.1;
	costs[FRENCH_LANGUAGE_CODE][TARGET_WORD_INDEX] = 25.3;
	costs[FRENCH_LANGUAGE_CODE][CONSTANT_POINTER_INDEX] = 0.4;
	costs[FRENCH_LANGUAGE_CODE][PAT_INDEX] = 0.36;
	costs[FRENCH_LANGUAGE_CODE][PAT_PN_GC_NC_INDEX] = 0.36;
	costs[FRENCH_LANGUAGE_CODE][PAT_PN_NC_PC_TC_INDEX] = 0.36;
	costs[FRENCH_LANGUAGE_CODE][PAT_PN_GC_NC_DEG_INDEX] = 0.36;
	costs[FRENCH_LANGUAGE_CODE][PAT_PN_GC_NC_PC_CC_INDEX] = 0.36;
	costs[FRENCH_LANGUAGE_CODE][PAT_PN_GC_NC_PC_TC_INDEX] = 0.36;
	costs[FRENCH_LANGUAGE_CODE][PAT_PN_GC_NC_CC_INDEX] = 0.36;
	costs[FRENCH_LANGUAGE_CODE][PAT_PN_GC_NC_DC_CC_DEG_INDEX] = 0.36;
	costs[FRENCH_LANGUAGE_CODE][WORD_IN_PHRASE_INDEX] = 0.8;

	gains[ENGLISH_LANGUAGE_CODE][GSSU_INDEX] = 6;
	gains[ENGLISH_LANGUAGE_CODE][GPHRASE_INDEX] = 24;
	gains[FRENCH_LANGUAGE_CODE][TRANSFER_CODE_INDEX] = 12;
	gains[FRENCH_LANGUAGE_CODE][TARGET_INDEX] = 12;
	gains[FRENCH_LANGUAGE_CODE][TRANSFER_COUNT_INDEX] = 3;
	gains[FRENCH_LANGUAGE_CODE][SEMTAB_RULE_INDEX] = 3;
	gains[FRENCH_LANGUAGE_CODE][PATTERN_RULES_INDEX] = 0;
	gains[FRENCH_LANGUAGE_CODE][CONSTANT_CODE_INDEX] = 1;
	gains[FRENCH_LANGUAGE_CODE][TRANSFER_INDEX] = 9;
	gains[FRENCH_LANGUAGE_CODE][STEMGEN_RULE_INDEX] = 8;
	gains[FRENCH_LANGUAGE_CODE][TARGET_WORD_INDEX] = 6;
	gains[FRENCH_LANGUAGE_CODE][CONSTANT_POINTER_INDEX] = 2;
	gains[FRENCH_LANGUAGE_CODE][PAT_INDEX] = 1;
	gains[FRENCH_LANGUAGE_CODE][PAT_PN_GC_NC_INDEX] = 1;
	gains[FRENCH_LANGUAGE_CODE][PAT_PN_NC_PC_TC_INDEX] = 1;
	gains[FRENCH_LANGUAGE_CODE][PAT_PN_GC_NC_DEG_INDEX] = 1;
	gains[FRENCH_LANGUAGE_CODE][PAT_PN_GC_NC_PC_CC_INDEX] = 1;
	gains[FRENCH_LANGUAGE_CODE][PAT_PN_GC_NC_PC_TC_INDEX] = 1;
	gains[FRENCH_LANGUAGE_CODE][PAT_PN_GC_NC_CC_INDEX] = 1;
	gains[FRENCH_LANGUAGE_CODE][PAT_PN_GC_NC_DC_CC_DEG_INDEX] = 1;
	gains[FRENCH_LANGUAGE_CODE][WORD_IN_PHRASE_INDEX] = 1;

	int limit = 2 << NDATASET;
	struct stmts_combination best = {0,0,0};
	for(int i=0;i<limit;i++) {
		double cost = 0.;
		double gain = 0.;
		for(int j=0;j<NDATASET;j++) {
			int idx = i & (1<<j);
			if(idx)
				if(j<TRANSFER_CODE_INDEX) {
					cost += costs[ENGLISH_LANGUAGE_CODE][j];
					gain += gains[ENGLISH_LANGUAGE_CODE][j];
				} else {
					cost += costs[FRENCH_LANGUAGE_CODE][j];
					gain += gains[FRENCH_LANGUAGE_CODE][j];
				}
		}
		if(cost > nmegabytes)
			continue;
		if(gain > best.gain) {
			best.gain = gain;
			best.cost = cost;
			best.index = i;
		}
	}
	printf("Memory limit=%dMb, cost = %4.2fMb, gain = %4.2f%%\n",
		nmegabytes, best.cost, best.gain);
	printf("Datasets recommended:\n");
	for(int j=0;j<NDATASET;j++) {
		int idx = best.index & (1<<j);
		if(idx) {
			opt[j] = 1;
			printf("\t%s\n", stmt_names[j]);
		} else
			opt[j] = 0;
	}
	return best.index;
}
