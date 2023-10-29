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
#ifndef _CacheTranfsferCountDataClass
#define _CacheTranfsferCountDataClass

#define TRANSFER_COUNT_KEYLEN 20	// max(length(company_code)) + 1 
			// + sizeof(meaning_id)
			// + 3 * sizeof(int) 

#ifdef _MSC_VER
#include <windows.h>
#else
#include "logos_include/windows.h"
#endif
#include <logos_libs/dbcache/CacheData.h>

struct e_row {
	int meaning_id;
	char company_code[4];
	int count;
};

class DLLEXPORT CacheTransferCountData : public CacheData {

public:

	CacheTransferCountData(SqlConnection *con, int langcode, bool loader, 
		bool ff, bool sav);

	void makeKey(CacheKey *, char *cc, int mid, int val);
	virtual bool 	populateFromDatabase();	
	virtual void getSizeFromDB();

/*
select count(*) 
from transfer
where
	meaning_id = :mid
	and company_code = :cc
	and target_language_code = :lc
*/
	inline struct e_row * getRow(int i) {
		struct e_row *arp = 
		(struct e_row *)((char *)data_array + i*sizeof(struct e_row));
		return arp;
	}
	inline char * getCompanyCode(int i) { return getRow(i)->company_code; }
	inline int getMeaningId(int i) { return getRow(i)->meaning_id; }
	inline int getCount(int i) { return getRow(i)->count; }
};

#endif // _CacheTranfsferCountDataClass
