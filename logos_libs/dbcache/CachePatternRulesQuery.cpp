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

#ifdef _MSC_VER
#include <windows.h>
#else
#include "logos_include/windows.h"
#endif
#include <cstring>

#include <logos_libs/dbcache/CachePatternRulesQuery.h>
#include <logos_libs/dbcache/CachePatternRulesData.h>

CachePatternRulesQuery::CachePatternRulesQuery(CachePatternRulesData *bd,
											   char cc_[MAX_CC][4], int ncc_) {
	fdata = bd;
	ncc = ncc_;
	nindex = 0;
	char *cp;
	int i=0;
	for(cp=cc_[0];i<ncc;cp++,i++) {
		if(i==MAX_CC)
			break;
		strncpy(company_codes[i], cp, 3);
	}

	nindex = 0;

	curr_index = 0;
}
	
CachePatternRulesQuery::~CachePatternRulesQuery(){}

// Algorithm is VERY, VERY ineffective.
// In fact, the whole hash scheme is not suited for sql clauses like
// " where company_code in ('cc1', 'cc2' ...) "
// And rules should be ordered by sequence_number regardless of company_code.
// Thus, we're ignoring all hashing and just looking through flat array
// of rules, which is ordered by sequence_number,
// praying that number of rules is small (like 1 or 2:) because
// algorithm has linear complexity ( performing as O(n)).
// ----------------------------------
int
CachePatternRulesQuery::query(char cc_[MAX_CC][4], int ncc_) {
	ncc=ncc_;

	nindex = 0;

	int i = 0;
	char *cp;
	for(cp=cc_[0];i<ncc;cp++,i++) {
		if(i==MAX_CC)
			break;
		strncpy(company_codes[i], cp, 3);
	}

	int nrows = fdata->getSize() - 1;

	curr_index = 0;

	for(i=0;i<nrows;i++) {
//printf("CachePatternRulesQuery start_index %d\n", start_index);
		for(int j=0;j<ncc;j++) {
			if((strncmp(company_codes[j],
					fdata->getCompanyCode(i),3)==0)) {
				indexes[nindex] = i;
				nindex++;
				if(nindex==(MAX_RULES-1)) {
					return nindex;
				}
				break;
			}
		}
	}

	return nindex;
}

bool
CachePatternRulesQuery::fetch(int *sn, char *rtc, char *se,
		char *re, char *cl, char *cc) {

	if(curr_index==-1 || curr_index >= nindex)
		return false;

	int i = indexes[curr_index];

	*sn = fdata->getSequenceNumber(i);
	strncpy(rtc, fdata->getRuleTypeCode(i), 1);
	strncpy(se, fdata->getSearchExpression(i), 500);
	strncpy(re, fdata->getReplaceExpression(i), 500);
	strncpy(cl, fdata->getCommentLine(i), 500);
	strncpy(cc, fdata->getCompanyCode(i), 3);

	curr_index++;
	return (true);
}
