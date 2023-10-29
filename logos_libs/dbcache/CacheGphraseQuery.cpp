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

#include <logos_libs/dbcache/CacheGphraseQuery.h>
#include <logos_libs/dbcache/CacheGphraseData.h>
#include <logos_libs/dbcache/CacheKey.h>

CacheGphraseQuery::CacheGphraseQuery(CacheGphraseData *bd, char *s) {
	bdata = bd;
	if(s)
		strncpy(str, s, BDATA_MAX_STRLEN);
	else
		str[0] = '\0';
	start_index = end_index = curr_index = -1;
	permutations = new int[BDATA_MAX_NFETCH];
	counts = new int[BDATA_MAX_NFETCH];
}
	
CacheGphraseQuery::~CacheGphraseQuery(){
	delete [] permutations;
	delete [] counts;
}

int
CacheGphraseQuery::query(char *s) {
	char keybuf[256];
	strncpy(str, s, BDATA_MAX_STRLEN);
	CacheKey *k = (CacheKey *)keybuf;

//printf("CacheGphraseQuery: looking for \"%s\"\n", s);
//fflush(stdout);

	start_index = end_index = curr_index = -1;

// first let's try to find exact match
	int fullen = strlen(s) - 1;	// !!! -1 is for cutting off "%" in the end

	memcpy(k->getBuffer(), s, fullen);
	k->setLength(fullen);
	start_index = bdata->getRowIndex(k);
	if(start_index!=-1) {
		end_index = start_index + 1;
		while(memcmp(s, bdata->getWord(end_index), fullen)==0) {
//printf("exact match \"%s\"\n", bdata->getWord(end_index));
//			if(bdata->getWordCount(end_index) < 2)
//				break;
			end_index++;
//printf("exact match next is \"%s\"\n", bdata->getWord(end_index));
		}

		sortByCount();
		int nf = end_index-start_index;
		return nf;
	}

	int len = fullen;
	while(len) {
		k->setLength(len);
		int start = bdata->getRowIndex(k);
		if(start<0) {
			len--;
			continue;
		}

//if(strncmp(s, of_interest, 4)==0) {
//printf("CacheGphraseQuery found start for \"%s\": %d \"%s\"\n", s, start,
//	bdata->getWord(start));
//fflush(stdout);
//}
		start_index = findMatch(s, fullen, start, 16, true);
		if(start_index==-1)
			return 0;
		end_index = findMatch(s, fullen, start_index, 16, false);

		sortByCount();
		int nf = end_index - start_index;

//printf("XXX no exact match start = %d, end = %d\n", start_index, end_index);
//fflush(stdout);
//printf("NOT \"%s\" %d\n", s, nf);
//fflush(stdout);

		return nf;
	}

	return 0;
}

void
CacheGphraseQuery::sortByCount() { 
	// we need to sort selected range of words by word_cnt
		int i;
		int nf = end_index - start_index;
		if( nf > BDATA_MAX_NFETCH) {
			delete [] permutations;
			delete [] counts;
			permutations = new int[nf];
			counts = new int[nf];
		}
		for(i=0;i<nf;i++) {
			int j = start_index + i;
			permutations[i] = j;
			counts[i] = bdata->getWordCount(j);
		}
		for(int j=0;j<nf;j++) {
			bool changed = false;
			for(i=nf-1;i>j;i--) {
				if(counts[i] > counts[i-1]) {
					changed = true;
					int tmp = counts[i-1];
					counts[i-1] = counts[i];
					counts[i] = tmp;
					tmp = permutations[i-1];
					permutations[i-1] = permutations[i];
					permutations[i] = tmp;
				}
			}
			if(!changed)
				break;
		}
//if(nf > 2) {
//for(int j=0;j<nf;j++)
//printf("\t i=%d, count=%d, perm=%d\n", j, counts[j], permutations[j]);
//fflush(stdout);
//}
		start_index = 0;
		for(i=0;i<nf;i++)
			if(counts[i] < 2)
				break;
		end_index = i;
		if(end_index >= bdata->getSize() - 1)
			end_index = bdata->getSize() - 1;
		curr_index = start_index;

}

int
CacheGphraseQuery::findMatch(char *s, int fullen, int start, int niter, bool first) {

//if(strncmp(s, of_interest, 5)==0)
//printf("\"%s\" start %d \"%s\"\n", s, start, bdata->getWord(start));
	int end = start + (1<<(niter-1));
	int diff = 0;
	int middle = 0;
	if(end>bdata->getSize())
		end = bdata->getSize();
	while(niter) {
		middle = (start + end + 1)/2;
		diff = strncmp(s, bdata->getWord(middle), fullen);
		if(first) {
			if(diff <= 0)
				end = middle;
			else
				start = middle;
		} else {
			if(diff < 0)
				end = middle;
			else
				start = middle;
		}
		niter--;
/*
if(strncmp(s, of_interest, 5)==0) {
printf("\"%s\" start=%d, middle=%d, end=%d, diff=%d \"%s\"\n", s, start, 
middle, end, diff, bdata->getWord(middle));
fflush(stdout);
}
*/
	}
	if(first && diff)
		return -1;

	return middle;
}

bool
CacheGphraseQuery::fetch(int *word_id, char *word, int *hword, int *hashloc, 
						 int *ending_length, char *cc,
		int *hash_code_1, int *hash_code_2, int *root_hash_1, 
		int *root_hash_2, int *hen_1, int *hen_2, 
		int *root_hen_1, int *root_hen_2, int *word_count) {

	if(curr_index==-1 || curr_index==end_index)
		return false;
	int i = permutations[curr_index];
	strcpy(word, bdata->getWord(i));
	strcpy(cc, bdata->getCompanyCode(i));
	*word_id = bdata->getWordId(i);
	*hword = bdata->getHeadWord(i);
	*hashloc = bdata->getHashLocation(i);
	*ending_length = bdata->getEndingLength(i);
	*hash_code_1 = bdata->getHashCode1(i);
	*hash_code_2 = bdata->getHashCode2(i);
	*root_hash_1 = bdata->getRootHash1(i);
	*root_hash_2 = bdata->getRootHash2(i);
	*hen_1 = bdata->getHenNum1(i);
	*hen_2 = bdata->getHenNum2(i);
	*root_hen_1 = bdata->getRootHen1(i);
	*root_hen_2 = bdata->getRootHen2(i);
	*word_count =  bdata->getWordCount(i);
	curr_index++;
	return (curr_index <= end_index);
}
