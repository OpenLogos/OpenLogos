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

#include <logos_libs/dbcache/CacheConstantCodeQuery.h>
#include <logos_libs/dbcache/CacheConstantCodeData.h>
#include <logos_libs/dbcache/CacheKey.h>

CacheConstantCodeQuery::CacheConstantCodeQuery(CacheConstantCodeData *bd, char *ct, int i) {
	hdata = bd;
	if(ct)
		strncpy(ctp, ct, 1);
	cid = i;
	start_index = end_index = curr_index = -1;
}
	
CacheConstantCodeQuery::~CacheConstantCodeQuery(){}

int
CacheConstantCodeQuery::query(char *c, int id) {
	char keybuf[256];

//printf("KQUERY: \"%s\" %d\n", c, m);
//fflush(stdout);
	if(c)
		strncpy(ctp, c, 2);
	cid = id;
	CacheKey *k = (CacheKey *)keybuf;

	start_index = end_index = curr_index = -1;
	
	hdata->makeKey(k, ctp, cid, 0);

	start_index = hdata->getRowIndex(k);
	int ret = 0;
	if(start_index!=-1) {
		end_index = start_index + 1;
		while((cid==hdata->getConstantId(end_index))
			&& (strcmp(ctp,hdata->getConstantType(end_index))==0))
			end_index++;
		curr_index = start_index;
		ret = end_index-start_index;
	}

	return ret;
}

bool
CacheConstantCodeQuery::fetch(char *cc, int *auid, char *wtc, int *pn1, int *gc1,
		char *nc, int *pn2, int *gc2) {
	if(curr_index==-1 || curr_index >= (hdata->getSize()-1))
		return false;

	strcpy(cc, hdata->getCompanyCode(curr_index));
	*auid = hdata->getAlternateUsageId(curr_index);
	strcpy(wtc, hdata->getWordClassCode(curr_index));
	*pn1 = hdata->getPatNumber1(curr_index);
	*pn2 = hdata->getPatNumber2(curr_index);
	strcpy(nc, hdata->getNumericConstraint(curr_index));
	*gc1 = hdata->getGenderCode1(curr_index);
	*gc2 = hdata->getGenderCode2(curr_index);

	curr_index++;
	return (curr_index <= end_index);
}
