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
#ifndef _CacheConstantPointerDataClass
#define _CacheConstantPointerDataClass

#define CONSTANT_POINTER_KEYLEN 20	
	// sizeof(constant_id)
	// +sizeof(constant_type) + 3 * sizeof(int)

#ifdef _MSC_VER
#include <windows.h>
#else
#include "logos_include/windows.h"
#endif
#include <logos_libs/dbcache/CacheData.h>

struct n_row {
	int constant_id;
	char constant_type[1];
	int primary_usage_id;
	int alternate_usage_id;
	int combining_form_code;
	char company_code[4];
};

class DLLEXPORT CacheConstantPointerData : public CacheData {

public:

	CacheConstantPointerData(SqlConnection *con, int langcode, bool loader, bool ff, bool sav);
	void makeKey(CacheKey *, int cid, char *type, int val);
	virtual bool 	populateFromDatabase();
	virtual void getSizeFromDB();
	inline struct n_row * getRow(int i) {
		struct n_row *lrp = 
		(struct n_row *)((char *)data_array + i*sizeof(struct n_row));
		return lrp;
	}

/*
Statement "N", 5% of DBtime (after ABCDG have been done)

select primary_usage_id, alternate_usage_id, combining_form_code
from constant_pointer
	where company_code=:cc and constant_id=:cid
	and constant_type=:ctp and language_code=:lc
*/

	inline int getConstantId(int i) { return getRow(i)->constant_id; }
	inline char * getConstantType(int i){return getRow(i)->constant_type;}
	inline char * getCompanyCode(int i){return getRow(i)->company_code;}
	inline int getPrimaryUsageId(int i) {
		return getRow(i)->primary_usage_id;}
	inline int getAlternateUsageId(int i) {
		return getRow(i)->alternate_usage_id;}
	inline int getCombiningFormCode(int i) {
		return getRow(i)->combining_form_code;}
};

#endif // _CacheConstantPointerDataClass
