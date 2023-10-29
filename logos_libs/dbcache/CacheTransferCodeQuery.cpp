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

#include <logos_libs/dbcache/CacheTransferCodeData.h>
#include <logos_libs/dbcache/CacheTransferCodeQuery.h>
#include <logos_libs/dbcache/CacheKey.h>

CacheTransferCodeQuery::CacheTransferCodeQuery(CacheTransferCodeData *bd, 
											   char *c, int w) {
	cdata = bd;
	if(c)
		memcpy(cc, c, 4);
	mid = w;
	start_index = end_index = curr_index = -1;
}
	
CacheTransferCodeQuery::~CacheTransferCodeQuery(){}

int
CacheTransferCodeQuery::query(char *s, int m) {

	if(s==NULL)
		return 0;

	memcpy(cc, s, 4);
	mid = m;
	char keybuf[256];
	CacheKey *k = (CacheKey *)keybuf;

	start_index = end_index = curr_index = -1;
	
	cdata->makeKey(k, s, m, 0);

	start_index = cdata->getRowIndex(k);
	int len = strlen(s);
	int ret = 0;
	if(start_index!=-1) {
		end_index = start_index + 1;
		while(m==cdata->getMeaningId(end_index)) {
			end_index++;
		}
		curr_index = start_index;
		ret = end_index-start_index;
	}

	return ret;
}

bool
CacheTransferCodeQuery::fetch(char *ccc, int *as, int *o2a, int *o2b, int *o3a,
		int *o3b, int *wcc, int *pn, int *gc) {
	if(curr_index==-1 || curr_index >= (cdata->getSize()-1))
		return false;
// data ordered by meaning_id,company code thus we should
// skip extra rows w/ non-relevant CCs.
        int i;
	for(i=curr_index;i<end_index;i++) {
		if(memcmp(cc, cdata->getCompanyCode(curr_index), 4)==0)
			break;
	}
	if(i==end_index)
		return false;

// gotcha, CCs match
	memcpy(ccc, cdata->getCompanyCode(curr_index), 4);
	*as = cdata->getAlternateSequence(curr_index);
	*o2a = cdata->getOverflow2a(curr_index);
	*o2b = cdata->getOverflow2b(curr_index);
	*o3a = cdata->getOverflow3a(curr_index);
	*o3b = cdata->getOverflow3b(curr_index);
	*wcc = cdata->getWordClassCode(curr_index);
	*pn = cdata->getPatNumber(curr_index);
	*gc = cdata->getGenderCode(curr_index);

	curr_index++;
	return (curr_index <= end_index);
}
