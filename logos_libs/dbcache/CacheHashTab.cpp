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
#include <logos_libs/dbcache/CacheHashTab.h>
#include <logos_libs/dbcache/CacheKey.h>

CacheHashTab::CacheHashTab(int sz, int keysz, void *keys, void *heads, double *wghts) {

	size = sz;

	key_array = keys;
	keysize = keysz;

	head_array = (int *)heads;

	weight_array = wghts;

	nkeys = ngets = ngethits = 0;
}

void
CacheHashTab::reset() {
	free_key_index = 0;

	int n = size * keysize;
	memset(key_array, 0, n);
        int i;
	for(i=0;i<size;i++)
		getCacheKeyAt(i)->setNextIndex(-1);

	for(i=0;i<size;i++) {
		head_array[i] = -1;
	}

// reverse weights - in calculations it's better to multiply than divide.
	for(i=0;i<256;i++)
		if(weight_array[i]!=0.0)
			weight_array[i] = 1./weight_array[i];
}

CacheHashTab::~CacheHashTab() { 
// not our business to delete anything. We're being supplied w/
// keys array by [A-Z]CacheData classes.
}

int
CacheHashTab::get(CacheKey *key) {
	int idx = computeHashIndex(key);
	ngets++;
//if(size=10367 && idx==3646)
//printf("CacheHashTab.get idx = %d\n", idx);

	int i = head_array[idx];
//if(size=10367 && idx==3646) {
//	for(int j=3640;j<3650;j++)
//		printf("CacheHashTab.get head_array[%d] = %d\n", j, head_array[j]);
//}
	if(i==-1) 
		return -1;

	while(1) {
//if(size=10367 && idx==3646)
//printf("CacheHashTab.get head i = %d\n", i);
		CacheKey *kp = getCacheKeyAt(i);

//		if(*key==*kp) {
		int l1 = kp->getLength();
		int l2 = key->getLength();
		if(l1==l2) {
			void *vp1 = kp->getBuffer();
			void *vp2 = key->getBuffer();
			int ret = memcmp(vp1, vp2, l1);
			if(ret==0) {
				ngethits++;
//if(size=10367 && idx==3646)
//printf("hit %d\n", getCacheKeyAt(i)->getValue());
				return kp->getValue();
			}
		}
		i = kp->getNextIndex();
//if(size=10367 && idx==3646)
//printf("non-hit next %d\n", i);
		if(i==-1)
			return -1;
	}
	
	return -1;
}

bool
CacheHashTab::put(CacheKey *key) {
	int idx = computeHashIndex(key);
//printf("put: \"%s\" into %d\n", key->getBuffer(), idx);
//fflush(stdout);
	nkeys++;
	int i = head_array[idx];
	if(i==-1) {
		i = allocateNextCacheKey();
		head_array[idx] = i;
	} else {
		while(1) {
			int next_idx = getCacheKeyAt(i)->getNextIndex();
			if(next_idx==-1) {
				next_idx = allocateNextCacheKey();
				if(next_idx==-1)
					return false;	// should not happen
				getCacheKeyAt(i)->setNextIndex(next_idx);
				i = next_idx;
				break;
			} else {
				i = next_idx;
				continue;
			}
		}
	}

	memcpy(getCacheKeyAt(i), key, keysize);

	return true;
}

int
CacheHashTab::computeHashIndex(CacheKey *k) {
	register int n = k->getLength();
	register unsigned char *cp = (unsigned char *)k->getBuffer();
	register unsigned int hash = 0;
	register int i;

	double A = 0.6180339887;
	for(i=0;i<n;i++) {
		int val = *cp;
//XXX weights should be language-specific
		val *= (int)weight_array[*cp];
                // _fix_me_ added the casts to avoid the warnings, hope that is
                // what was intended
		int ihash = (int) (((double) val) * A);
		ihash = (int)((((double) val) * A - ((double) ihash)) * size);
		hash += ihash;
		cp++;
	}
	hash %= size;

// 2.0 ave, 8 max - the best so far for stmts 5-14 inclusive, 48.html
/*
	double A = 0.6180339887;
	for(i=0;i<n;i++) {
		int val = *cp;
		int ihash = val * A;
		ihash = (val*A - ihash) * size;
		hash += ihash;
		cp++;
	}

	hash %= size;
*/

//	3.23 ave, 13 max
/*
	for(i=0;i<n;i++) {
		hash += *cp++;
	}
	hash %= size;
*/
	return hash;
}

void
CacheHashTab::print() {
	printf("CacheHashTab: %d bins, # uniq keys %d, ave bin len %4.1f, max len %d\n",
		size, getNCacheKeys(), getAverageSearchLength(), getMaxSearchLength());
//	printf("\t# gets/hits %d/%d\n", getNGets(), getNGetHits());
}

void
CacheHashTab::calculateStat() {
	int i;

	ave_search_len = 0.;
	for(i=max_search_len=0;i<size;i++) {
		int len = 0;
		int j = head_array[i];
		if(j==-1)
			continue;

		while(1) {
			int next_idx = getCacheKeyAt(j)->getNextIndex();
			if(next_idx==-1) {
				len++;
				break;
			} else {
				len++;
				j = next_idx;
			}
		}

		if(len>max_search_len)
			max_search_len = len;
		ave_search_len += len * len;
	}

	ave_search_len /= nkeys;
}

int CacheHashTab::getSize() { return size; }
int CacheHashTab::getNCacheKeys() { return nkeys; }
int CacheHashTab::getNGetHits() { return ngethits; }
int CacheHashTab::getNGets() { return ngets; }
double CacheHashTab::getAverageSearchLength() { return ave_search_len; }
int CacheHashTab::getMaxSearchLength() { return max_search_len; }

int CacheHashTab::allocateNextCacheKey() {
	free_key_index++;
	if(free_key_index==size) 
		return -1;
	return free_key_index - 1;
}
CacheKey * CacheHashTab::getCacheKeyAt(int i) { 
	char *cp = (char *)key_array;
	return (CacheKey *)(cp + i*keysize);
}
