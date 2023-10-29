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

#include <logos_libs/dbcache/CacheEndingLengthQuery.h>
#include <logos_libs/dbcache/CacheEndingLengthData.h>
#include <logos_libs/dbcache/CacheKey.h>

CacheEndingLengthQuery::CacheEndingLengthQuery(CacheEndingLengthData *d, int i)
{
	mdata = d;
	word_id = i;
	start_index = end_index = curr_index = -1;
}
	
CacheEndingLengthQuery::~CacheEndingLengthQuery(){}

int
CacheEndingLengthQuery::query(int id) {
	char keybuf[256];

//printf("KQUERY: \"%s\" %d\n", c, m);
//fflush(stdout);
	word_id = id;
	CacheKey *k = (CacheKey *)keybuf;

	start_index = end_index = curr_index = -1;
	
	mdata->makeKey(k, word_id, 0);

	start_index = mdata->getRowIndex(k);
	int ret = 0;
	if(start_index!=-1) {
		end_index = start_index + 1;
		while(word_id==mdata->getWordId(end_index))
			end_index++;
		curr_index = start_index;
		ret = end_index-start_index;
	}

	return ret;
}

bool
CacheEndingLengthQuery::fetch(int *len) {
	if(curr_index==-1 || curr_index >= (mdata->getSize()-1))
		return false;

	*len = mdata->getEndingLength(curr_index);

	curr_index++;
	return (curr_index <= end_index);
}
