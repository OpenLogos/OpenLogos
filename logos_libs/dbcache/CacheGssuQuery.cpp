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

#include <logos_libs/dbcache/CacheGssuData.h>
#include <logos_libs/dbcache/CacheGssuQuery.h>
#include <logos_libs/dbcache/CacheKey.h>

CacheGssuQuery::CacheGssuQuery(CacheGssuData *bd, char *c, int w) {
	adata = bd;
	if(c)
		memcpy(cc, c, 4);
	wid = w;
	start_index = end_index = curr_index = -1;
}
	
CacheGssuQuery::~CacheGssuQuery(){}

int
CacheGssuQuery::query(char *s, int w) {
	char keybuf[256];

	if(s)
		memcpy(cc, s, 4);
	wid = w;
	CacheKey *k = (CacheKey *)keybuf;

	start_index = end_index = curr_index = -1;
	
	adata->makeKey(k, s, w, 0);

	start_index = adata->getRowIndex(k);
	int len = strlen(s);
	int ret = 0;
	if(start_index!=-1) {
		end_index = start_index + 1;
		while(w==adata->getWordId(end_index))
			end_index++;
		curr_index = start_index;
		ret = end_index-start_index;
	}

	return ret;
}

bool
CacheGssuQuery::fetch(int *wcc, int *gc, int *pn, int *ssn, int *usid,
		int *ac, int *mid, int *setid, int *subsetid, int *supersetid,
		int *po, int *fc, char *smc, int *acode, int *gcode, char *tr) {
	if(curr_index==-1 || curr_index >= (adata->getSize()-1))
		return false;
// key is (word_id,company code) thus we should
// skip extra rows w/ non-relevant CCs.
        int i;
	for(i=curr_index;i<end_index;i++) {
		if(memcmp(cc, adata->getCompanyCode(curr_index), 4)==0)
			break;
	}
	if(i==end_index)
		return false;

// gotcha, CCs match

	*wcc = adata->getWordClassCode(curr_index);
	*gc = adata->getGenderCode(curr_index);
	*pn = adata->getPatNumber(curr_index);
	*ssn = adata->getSourceStemNumber(curr_index);
	*usid = adata->getUsageId(curr_index);
	*ac = adata->getAuxCode(curr_index);
	*mid = adata->getMeaningId(curr_index);
	*setid = adata->getSetId(curr_index);
	*subsetid = adata->getSubsetId(curr_index);
	*supersetid = adata->getSupersetId(curr_index);
	*po = adata->getPriorityOrder(curr_index);
	*fc = adata->getFormCode(curr_index);
	memcpy(smc, adata->getSMC(curr_index), 7);
	*acode = adata->getAtomicCode(curr_index);
	*gcode = adata->getGenericCode(curr_index);
	memcpy(tr, adata->getTransitivity(curr_index), 2);
	curr_index++;
	return (curr_index <= end_index);
}
