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


#include <logos_libs/dbcache/CacheGwordQuery.h>
#include <logos_libs/dbcache/CacheGphraseData.h>
#include <logos_libs/dbcache/CacheKey.h>

CacheGwordQuery::CacheGwordQuery(CacheGphraseData *bd, char *s) {
	bdata = bd;
	str = s;
	start_index = end_index = curr_index = -1;
}
	
CacheGwordQuery::~CacheGwordQuery(){}

int
CacheGwordQuery::query(char *s) {
	str = s;
	qcc = NULL;
	CacheKey k;

	start_index = end_index = curr_index = -1;

// we want ONLY exact matches
	int fullen = strlen(s);
	memcpy(k.getBuffer(), s, fullen);
	k.setLength(fullen);
	start_index = bdata->getRowIndex(&k);
	int nf=0;
	if(start_index!=-1) {
		end_index = start_index + 1;
		while(strcmp(s, bdata->getWord(end_index))==0)
			end_index++;
		curr_index = start_index;
		nf = end_index - start_index;
	}

//printf("CacheGwordQuery: \"%s\" %d\n", s, nf);
//fflush(stdout);
	return nf;
}

int
CacheGwordQuery::query(char *s, char *_qcc) {
	str = s;
	qcc = _qcc;
	CacheKey k;

	start_index = end_index = curr_index = -1;

// we want ONLY exact matches
	int fullen = strlen(s);
	memcpy(k.getBuffer(), s, fullen);
	k.setLength(fullen);
	start_index = bdata->getRowIndex(&k);
	int nf=0;
	if(start_index!=-1) {
		end_index = start_index + 1;
		while(strcmp(s, bdata->getWord(end_index))==0)
			end_index++;
		curr_index = start_index;
		nf = end_index - start_index;
	}

//printf("CacheGwordQuery: \"%s\" %d\n", s, nf);
//fflush(stdout);
	return nf;
}

bool
CacheGwordQuery::fetch(int *word_id, int *wcount, int *h1, int *h2, 
						int *rhash1, int *rhash2, int *hennum1,
						int *hennum2, int *rhen1, int *rhen2, int *hword,
						int *hashloc, int *wtc, char *cc) {
	if(curr_index==-1)
		return false;
	if(qcc) {
		while(strcmp(qcc, bdata->getCompanyCode(curr_index))!=0) {
			curr_index++;
			if(curr_index == bdata->getSize())
				return false;
		}
	}
	if(curr_index >= (bdata->getSize()-1))
		return false;

	*word_id = bdata->getWordId(curr_index);
	*wcount = bdata->getWordCount(curr_index);
	*h1 = bdata->getHashCode1(curr_index);
	*h2 = bdata->getHashCode2(curr_index);
	*rhash1 = bdata->getRootHash1(curr_index);
	*rhash2 = bdata->getRootHash2(curr_index);
	*hennum1 = bdata->getHenNum1(curr_index);
	*hennum2 = bdata->getHenNum2(curr_index);
	*rhen1 = bdata->getRootHen1(curr_index);
	*rhen2 = bdata->getRootHen2(curr_index);
	*hword = bdata->getHeadWord(curr_index);
	*hashloc = bdata->getHashLocation(curr_index);
	*wtc = bdata->getWordTypeCode(curr_index);
	strcpy(cc, bdata->getCompanyCode(curr_index));

	curr_index++;
	return (curr_index <= end_index);
}
