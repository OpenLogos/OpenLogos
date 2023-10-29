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
#ifndef _CacheGssuDataClass
#define _CacheGssuDataClass

#define GSSU_KEYLEN 20	// max(strlen(company_code)) + 1 
					// + sizeof(word_id)
					// + 3 * sizeof(int) 

#ifdef _MSC_VER
#include <windows.h>
#else
#include "logos_include/windows.h"
#endif
#include <logos_libs/dbcache/CacheData.h>

struct a_row {
	char company_code[4];
	int word_id;
	int word_class_code;
	int gender_code;
	int pat_number;
	int source_stem_number;
	int usage_id;
	int aux_code;
	int meaning_id;
	int set_id;
	int subset_id;
	int superset_id;
	int priority_order;
	int form_code;
	int atomic_code;
	int generic_code;
	char smc[7];
	char transitivity[2];
};

class DLLEXPORT CacheGssuData : public CacheData {

public:

	CacheGssuData(SqlConnection *con, int langcode, bool loader, bool ff, bool sav);

	void makeKey(CacheKey *, char *cc, int wordid, int val);
	virtual bool 	populateFromDatabase();	
	virtual void getSizeFromDB();

// Statement A-specific
// select (whole bunch) from morphology mr, meaning mn
// where
//	mr.word_id=:1 and company_code = :2
//	and mr.language_code = :3
//	...

	inline struct a_row * getRow(int i) {
		struct a_row *arp = 
		(struct a_row *)((char *)data_array + i*sizeof(struct a_row));
		return arp;
	}
	inline char * getCompanyCode(int i) { return getRow(i)->company_code; }
	inline int getWordId(int i) { return getRow(i)->word_id; }
	inline int getWordClassCode(int i) { return getRow(i)->word_class_code; }
	inline int getGenderCode(int i) { return getRow(i)->gender_code;}
	inline int getPatNumber(int i) { return getRow(i)->pat_number; }
	inline int getSourceStemNumber(int i) {return getRow(i)->source_stem_number; }
	inline int getUsageId(int i) { return getRow(i)->usage_id; }
	inline int getAuxCode(int i) { return getRow(i)->aux_code; }
	inline int getMeaningId(int i) { return getRow(i)->meaning_id; }
	inline int getSetId(int i) { return getRow(i)->set_id; }
	inline int getSubsetId(int i) { return getRow(i)->subset_id; }
	inline int getSupersetId(int i) { return getRow(i)->superset_id;}
	inline int getPriorityOrder(int i) {return getRow(i)->priority_order;}
	inline int getFormCode(int i) { return getRow(i)->form_code;}
	inline char * getSMC(int i) { return getRow(i)->smc;}
	inline int getAtomicCode(int i) { return getRow(i)->atomic_code;}
	inline int getGenericCode(int i) { return getRow(i)->generic_code;}
	inline char * getTransitivity(int i) { return getRow(i)->transitivity;}
};

#endif // _CacheGssuDataClass
