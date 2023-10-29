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
#ifndef _CacheWordInPhraseDataClass
#define _CacheWordInPhraseDataClass

#define WORD_IN_PHRASE_KEYLEN 20	// sizeof(word_id) + sizeof(location)
			// + 3 * sizeof(int)

#ifdef _MSC_VER
#include <windows.h>
#else
#include "logos_include/windows.h"
#endif
#include <logos_libs/dbcache/CacheData.h>

struct u_row {
	int word_id;
	int word_location;
	int pat_number;
	int word_class_code;
};

class DLLEXPORT CacheWordInPhraseData : public CacheData {

public:

	CacheWordInPhraseData(SqlConnection *con, int langcode, bool loader, bool ff, bool sav);
	void makeKey(CacheKey *, int pn, int wcc, int val);
	virtual bool 	populateFromDatabase();
	virtual void getSizeFromDB();
	inline struct u_row * getRow(int i) {
		struct u_row *lrp = 
		(struct u_row *)((char *)data_array + i*sizeof(struct u_row));
		return lrp;
	}

/*
Statement "U"

select w.pat_number, w.word_class_code
from word_in_phrase w, morphology m
where
	w.word_id=:wid and w.word_location=:wloc
	and w.usage_id=m.usage_id and w.company_code=m.company_code
*/

	inline int getPatNumber(int i) { return getRow(i)->pat_number; }
	inline int getWordClassCode(int i) {
		return getRow(i)->word_class_code; }
	inline int getWordId(int i) { return getRow(i)->word_id; }
	inline int getWordLocation(int i) {
		return getRow(i)->word_location; }
};

#endif // _CacheWordInPhraseDataClass
