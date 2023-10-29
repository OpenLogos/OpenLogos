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
#ifndef _CacheHashTabClass
#define _CacheHashTabClass

#ifdef _MSC_VER
#include <windows.h>
#else
#include "logos_include/windows.h"
#endif

class CacheKey;	// CacheKey is varied length sequence of bytes w/ some interface.
			// Stored value - integer index in a data table.

class DLLEXPORT CacheHashTab {

public:

	CacheHashTab(int size, int ksz, void *dt, void *hd, double *wghts);
	~CacheHashTab();

	void reset();
	int get(CacheKey *key);
	bool put(CacheKey * key);

	int computeHashIndex(CacheKey *);

	void print();
	void calculateStat();

	int getNGets();
	int getSize();
	int getNGetHits();
	int getNCacheKeys();
	int getMaxSearchLength();
	double getAverageSearchLength();
	int allocateNextCacheKey();
	CacheKey *getCacheKeyAt(int i);
/*
	inline CacheKey *getCacheKeyAt(int i) { 
		char *cp = (char *)key_array;
		return (CacheKey *)(cp + i*keysize);
	}
*/

private:
	int size;
	int free_key_index;
	int nkeys;

	int ngets;
	int ngethits;
	double ave_search_len;
	int max_search_len;

	void *key_array;
	int keysize;
	int *head_array;
	double *weight_array;
};

#endif // _CacheHashTabClass
