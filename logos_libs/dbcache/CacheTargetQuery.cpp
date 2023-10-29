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

#include <logos_libs/dbcache/CacheTargetQuery.h>
#include <logos_libs/dbcache/CacheTargetData.h>
#include <logos_libs/dbcache/CacheKey.h>

CacheTargetQuery::CacheTargetQuery(CacheTargetData *bd, int w) {
	ddata = bd;
	usid = w;
	start_index = end_index = curr_index = -1;
}
	
CacheTargetQuery::~CacheTargetQuery(){}

int
CacheTargetQuery::query(int w) {
	char keybuf[256];

	usid = w;
	CacheKey *k = (CacheKey *)keybuf;

	start_index = end_index = curr_index = -1;
	
	ddata->makeKey(k, w, 0);

	start_index = ddata->getRowIndex(k);
	int ret = 0;
	if(start_index!=-1) {
		end_index = start_index + 1;
		while(w==ddata->getUsageId(end_index))
			end_index++;
		curr_index = start_index;
		ret = end_index-start_index;
	}

	return ret;
}

bool
CacheTargetQuery::fetch(int *wid, char *word, int *wc, int *wtc, int *hw,
		int *bhl, int *wp, char *as, char *pes, int *wcc,
		int *pn, int *gc, int *nc, int *ac, int *vpi,
		int *vps, int *vpil, int *vpsl, int *ssn,
		int *ruid, int *sac) {
	if(curr_index==-1 || curr_index >= (ddata->getSize()-1))
		return false;

	*wid = ddata->getWordId(curr_index);
	strcpy(word, ddata->getWord(curr_index));
	*wc = ddata->getWordCount(curr_index);
	*wtc = ddata->getWordTypeCode(curr_index);
	*hw = ddata->getHeadWord(curr_index);
	*bhl = ddata->getBlackHoleLocation(curr_index);
	*wp = ddata->getWildcardPosition(curr_index);

	memcpy(as, ddata->getAspireSwitch(curr_index), 2);
	memcpy(pes, ddata->getPatExceptionSwitch(curr_index), 2);

	*wcc = ddata->getWordClassCode(curr_index);
	*pn = ddata->getPatNumber(curr_index);
	*gc = ddata->getGenderCode(curr_index);
	*nc = ddata->getNumericConstraint(curr_index);
	*ac = ddata->getAuxCode(curr_index);
	*vpi = ddata->getVerbPrefixInsep(curr_index);
	*vps = ddata->getVerbPrefixSep(curr_index);
	*vpil = ddata->getVerbPrefixInsepLen(curr_index);
	*vpsl = ddata->getVerbPrefixSepLen(curr_index);
	*ssn = ddata->getSourceStemNumber(curr_index);
	*ruid = ddata->getRootUsageId(curr_index);
	*sac = ddata->getSourceAnalCode(curr_index);

	curr_index++;
	return (curr_index <= end_index);
}
