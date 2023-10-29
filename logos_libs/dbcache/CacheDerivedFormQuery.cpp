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
#include <cstdio>
#include <cstring>

#include <logos_libs/dbcache/CacheDerivedFormQuery.h>
#include <logos_libs/dbcache/CacheDerivedFormData.h>

CacheDerivedFormQuery::CacheDerivedFormQuery(CacheDerivedFormData *bd) {
	idata = bd;
	start_index = end_index = curr_index = -1;
}
	
CacheDerivedFormQuery::~CacheDerivedFormQuery(){}

int
CacheDerivedFormQuery::query() {

	start_index = 0;
	end_index = idata->getSize() - 1;
	curr_index = start_index;
	int ret = end_index-start_index;

	return ret;
}

bool
CacheDerivedFormQuery::fetch(char *dfi, int *pn, int *sn, char *endng,
		int *wcc, int *fc) {
	if(curr_index==-1 || curr_index >= (idata->getSize()-1))
		return false;

	strncpy(dfi, idata->getDerivedFormId(curr_index), 60);
	*pn = idata->getPatNumber(curr_index);
	*sn = idata->getStemNumber(curr_index);
	strncpy(endng, idata->getEnding(curr_index), 31);
	*wcc = idata->getWordClassCode(curr_index);
	*fc = idata->getFormCode(curr_index);

	curr_index++;
	return (curr_index <= end_index);
}
