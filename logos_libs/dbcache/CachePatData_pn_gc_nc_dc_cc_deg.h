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
#ifndef _CachePatData_pn_gc_nc_dc_cc_degClass
#define _CachePatData_pn_gc_nc_dc_cc_degClass

#define O_NEN 5017
#define O_NDE 40232
#define O_NFR 9516

#define O_PN_GC_NC_DC_CC_DEG_KEYLEN 36	// sizeof(pat_number) + sizeof(gender_code)
			// sizeof(number_code) + sizeof(declention_code)
			// + sizeof(case_code)+sizeof(degree) + 3 * sizeof(int)

#ifdef _MSC_VER
#include <windows.h>
#else
#include "logos_include/windows.h"
#endif
#include <logos_libs/dbcache/CacheData.h>

struct o_pn_gc_nc_dc_cc_deg_row {
	int pat_number;
	int gender_code;
	int number_code;
	int declention_code;
	int case_code;
	int degree;
	int stem_number;
	char ending[9];
};

class DLLEXPORT CachePatData_pn_gc_nc_dc_cc_deg : public CacheData {

public:

	CachePatData_pn_gc_nc_dc_cc_deg(SqlConnection *con, int langcode, bool loader,
		bool ff, bool sav);
	void makeKey(CacheKey *, int pn, int gc, int nc, int pc, int cc,
		int deg, int val);
	virtual bool 	populateFromDatabase();
	virtual void getSizeFromDB();
	inline struct o_pn_gc_nc_dc_cc_deg_row * getRow(int i) {
		struct o_pn_gc_nc_dc_cc_deg_row *lrp = 
		(struct o_pn_gc_nc_dc_cc_deg_row *)((char *)data_array + i*sizeof(struct o_pn_gc_nc_dc_cc_deg_row));
		return lrp;
	}

/*
Statement "O"
select stem_number, ending
from pat_table 
where
	language_code=:lc and
	pat_number=:pn and gender_code=:gc and number_code=:nc
	and declention_code=:pc and case_code=:cc and degree=:degree
*/

	inline int getPatNumber(int i) { return getRow(i)->pat_number; }
	inline int getGenderCode(int i) { return getRow(i)->gender_code; }
	inline int getNumberCode(int i) { return getRow(i)->number_code; }
	inline int getDeclentionCode(int i){return getRow(i)->declention_code;}
	inline int getCaseCode(int i) { return getRow(i)->case_code; }
	inline int getDegree(int i) { return getRow(i)->degree; }
	inline int getStemNumber(int i) { return getRow(i)->stem_number; }
	inline char * getEnding(int i) { return getRow(i)->ending; }
};

#endif // _CachePatData_pn_gc_nc_dc_cc_degClass
