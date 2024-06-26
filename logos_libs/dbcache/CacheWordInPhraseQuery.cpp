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
program. If not, write to Globalware AG, Hospitalstraße 6, D-99817 Eisenach.

Linux port modifications and additions by Bernd Kiefer, Walter Kasper,
Deutsches Forschungszentrum fuer kuenstliche Intelligenz (DFKI)
Stuhlsatzenhausweg 3, D-66123 Saarbruecken
*/

#ifdef _MSC_VER
#include <windows.h>
#else
#include "logos_include/windows.h"
#endif

#include <logos_libs/dbcache/CacheWordInPhraseQuery.h>
#include <logos_libs/dbcache/CacheWordInPhraseData.h>
#include <logos_libs/dbcache/CacheKey.h>

CacheWordInPhraseQuery::CacheWordInPhraseQuery(CacheWordInPhraseData *bd, int id, int wl) {
	udata = bd;
	wid = id;
	wloc = wl;
	start_index = end_index = curr_index = -1;
}
	
CacheWordInPhraseQuery::~CacheWordInPhraseQuery(){}

int
CacheWordInPhraseQuery::query(int id, int wl) {
	char keybuf[256];

	wid = id;
	wloc = wl;
	CacheKey *k = (CacheKey *)keybuf;

	start_index = end_index = curr_index = -1;
	
	udata->makeKey(k, wid, wloc, 0);

	start_index = udata->getRowIndex(k);
	int ret = 0;
	if(start_index!=-1) {
		end_index = start_index + 1;
		while((wid==udata->getWordId(end_index))
			&& (wloc==udata->getWordLocation(end_index)))
			end_index++;
		curr_index = start_index;
		ret = end_index-start_index;
	}

	return ret;
}

bool
CacheWordInPhraseQuery::fetch(int *pn, int *wcc) {
	if(curr_index==-1 || curr_index >= (udata->getSize()-1))
		return false;

	*pn = udata->getPatNumber(curr_index);
	*wcc = udata->getWordClassCode(curr_index);

	curr_index++;
	return (curr_index <= end_index);
}
