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

#include <logos_libs/dbcache/CachePatQuery.h>
#include <logos_libs/dbcache/CachePatData.h>
#include <logos_libs/dbcache/CacheKey.h>

CachePatQuery::CachePatQuery(CachePatData *bd, int p, int n, int c) {
	odata = bd;
	pn = p;
	nc = n;
	cc = c;
	start_index = end_index = curr_index = -1;
}
	
CachePatQuery::~CachePatQuery(){}

int
CachePatQuery::query(int p, int n, int c) {
	char keybuf[256];

	pn = p;
	nc = n;
	cc = c;
	CacheKey *k = (CacheKey *)keybuf;

	start_index = end_index = curr_index = -1;
	
	odata->makeKey(k, pn, nc, cc, 0);

	start_index = odata->getRowIndex(k);
	int ret = 0;
	if(start_index!=-1) {
		end_index = start_index + 1;
		while((pn==odata->getPatNumber(end_index))
			&& (nc==odata->getNumberCode(end_index))
			&& (cc==odata->getCaseCode(end_index)))
			end_index++;
		curr_index = start_index;
		ret = end_index-start_index;
	}

//printf("OQUERY: %d %d %d ret=%d\n", p, n, c, ret);
//fflush(stdout);

	return ret;
}

bool
CachePatQuery::fetch(int *sn, char *ending) {
	if(curr_index==-1 || curr_index >= (odata->getSize()-1))
		return false;

	*sn = odata->getStemNumber(curr_index);
	strcpy(ending, odata->getEnding(curr_index));

	curr_index++;
	return (curr_index <= end_index);
}
