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
#ifndef _CacheIrregularStemDataClass
#define _CacheIrregularStemDataClass

#define IRREGULAR_STEM_KEYLEN 20	// max(strlen(company_code)) + 1 
			// + sizeof(usage_id)
			// + 3 * sizeof(int) 

#ifdef _MSC_VER
#include <windows.h>
#else
#include "logos_include/windows.h"
#endif
#include <logos_libs/dbcache/CacheData.h>

struct is_row {
	int usage_id;
	char company_code[4];
	char stem_word[32];
};

class DLLEXPORT CacheIrregularStemData : public CacheData {

public:

	CacheIrregularStemData(SqlConnection *con, int langcode, bool loader, bool ff, bool sav);

	void makeKey(CacheKey *, char *cc, int usid, int val);
	virtual bool 	populateFromDatabase();	
	virtual void getSizeFromDB();

/*
select stem_word
from irregular_stem 
where 
	company_code=:cc
	and t.usage_id = :usid
*/
	inline struct is_row * getRow(int i) {
		struct is_row *arp = 
		(struct is_row *)((char *)data_array + i*sizeof(struct is_row));
		return arp;
	}
	inline char * getCompanyCode(int i) { return getRow(i)->company_code; }
	inline char * getStemWord(int i) { return getRow(i)->stem_word; }
	inline int getUsageId(int i) { return getRow(i)->usage_id; }
};

#endif // _CacheIrregularStemDataClass
