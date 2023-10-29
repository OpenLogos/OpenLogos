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
#ifndef _CacheGwordQueryClass
#define _CacheGwordQueryClass

// this is to replace DB-query called from "gwordquery.cpp"

#ifdef _MSC_VER
#include <windows.h>
#else
#include "logos_include/windows.h"
#endif

class CacheGphraseData;

class DLLEXPORT CacheGwordQuery {

public:

	CacheGwordQuery(CacheGphraseData *bd, char *s);
	~CacheGwordQuery();

	int query(char *str);
	int query(char *str, char *qcc);
	bool fetch(int *word_id, int *wcount, int *h1, int *h2,
		int *rhash1, int *rhash2, int *hennum1, int *hennum2,
		int *rhen1, int *rhen2, int *hw, int *hl, int *wtc, char *cc);

private:
	CacheGphraseData *bdata;
	char *str;
	char *qcc;
	int start_index;
	int end_index;
	int curr_index;
};

#endif // _CacheGwordQueryClass
