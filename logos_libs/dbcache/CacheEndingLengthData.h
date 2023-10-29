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
#ifndef _CacheEndingLengthDataClass
#define _CacheEndingLengthDataClass

#define ENDING_LENGTH_KEYLEN 16 // sizeof(word_id) + 3 * sizeof(int) 

#ifdef _MSC_VER
#include <windows.h>
#else
#include "logos_include/windows.h"
#endif
#include <logos_libs/dbcache/CacheData.h>

struct el_row {
	int word_id;
	int ending_length;
};

class DLLEXPORT CacheEndingLengthData : public CacheData {

public:

	CacheEndingLengthData(SqlConnection *con, int langcode,
		bool loader, bool ff, bool sav);

	void makeKey(CacheKey *, int wid, int val);
	virtual bool 	populateFromDatabase();	
	virtual void getSizeFromDB();

/*
select ending_length
from word_phrase 
where 
	word_id=:wid
*/
	inline struct el_row * getRow(int i) {
		struct el_row *arp = 
		(struct el_row *)((char *)data_array + i*sizeof(struct el_row));
		return arp;
	}
	inline int getWordId(int i) { return getRow(i)->word_id; }
	inline int getEndingLength(int i) { return getRow(i)->ending_length; }
};

#endif // _CacheEndingLengthDataClass
