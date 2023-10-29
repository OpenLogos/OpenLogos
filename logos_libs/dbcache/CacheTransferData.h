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
#ifndef _CacheTransferDataClass
#define _CacheTransferDataClass

#define TRANSFER_KEYLEN 20	// sizeof(meaning_id) + sizeof(company_code)
			// + 3 * sizeof(int)

#ifdef _MSC_VER
#include <windows.h>
#else
#include "logos_include/windows.h"
#endif
#include <logos_libs/dbcache/CacheData.h>

struct k_row {
	int target_usage_id;
	int combining_form_code;
	int alternate_sequence;
	int meaning_id;
	char company_code[4];
};

class DLLEXPORT CacheTransferData : public CacheData {

public:

	CacheTransferData(SqlConnection *con, int langcode, bool loader, bool ff, bool sav);
	void makeKey(CacheKey *, char *cc, int mid, int val);
	virtual bool 	populateFromDatabase();
	virtual void getSizeFromDB();
	inline struct k_row * getRow(int i) {
		struct k_row *lrp = 
		(struct k_row *)((char *)data_array + i*sizeof(struct k_row));
		return lrp;
	}

/*
Statement "K", 5% of DBtime (after ABCDG have been done)

select target_usage_id, combining_form_code, alternate_sequence 
from transfer 
where
	meaning_id = :mid and company_code = :cc
	and deactivation_switch = 'N'
group by target_language_code

select target_language_code, count(*) from
transfer where deactivation_switch = 'N'
group by target_language_code
*/

	inline int getMeaningId(int i) {
		return getRow(i)->meaning_id; }
	inline char * getCompanyCode(int i) {
		return getRow(i)->company_code; }
	inline int getTargetUsageId(int i) {
		return getRow(i)->target_usage_id; }
	inline int getCombiningFormCode(int i) {
		return getRow(i)->combining_form_code; }
	inline int getAlternateSequence(int i) {
		return getRow(i)->alternate_sequence; }
};

#endif // _CacheTransferDataClass
