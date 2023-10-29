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
#ifndef _CacheGphraseDataClass
#define _CacheGphraseDataClass

#define GPHRASE_KEYLEN 86	// 73 of max(length(word)) and 3 * sizeof(int)

#ifdef _MSC_VER
#include <windows.h>
#else
#include "logos_include/windows.h"
#endif
#include <logos_libs/dbcache/CacheData.h>

struct b_row {
	char word[73];
	int head_word;
	int hash_location;
	char company_code[4];
// the rest is for statement G, which is
// "select word_id, hash_code_1, hash_code_2, head_word, hash_location,
//		word_type_code, company_code
//	from word_phrase where word = :1 and language_code = :2
// AND for statement T:
// "select word_id, word_count, hash_code_1, hash_code_2, 
//		root_hash_1, root_hash_2, hen_num_1, hen_num_2,
//		root_hen_1, root_hen_2, hash_location,
//		head_word, company_code
//	from word_phrase where word = :1 and language_code = :2
	int word_id;
	int hash_code_1;
	int hash_code_2;
	int root_hash_1;
	int root_hash_2;
	int root_hen_1;
	int root_hen_2;
	int hen_num_1;
	int hen_num_2;
	int word_type_code;
	int word_count;
	int ending_length;
};

class DLLEXPORT CacheGphraseData : public CacheData {

public:

	CacheGphraseData(SqlConnection *con, int langcode, bool loader, bool ff, bool sav);

	void makeKey(CacheKey *k, char *s, int val);
	virtual bool 	populateFromDatabase();
	virtual void getSizeFromDB();
	inline struct b_row * getRow(int i) {
		struct b_row *brp = 
		(struct b_row *)((char *)data_array + i*sizeof(struct b_row));
		return brp;
	}

// Statement B-specific
	inline char * getWord(int i) { return getRow(i)->word; }
	inline int getHeadWord(int i) { return getRow(i)->head_word; }
	inline int getHashLocation(int i) { return getRow(i)->hash_location; }
	inline char * getCompanyCode(int i) { return getRow(i)->company_code; }

// Additional fields for statement G
	inline int getWordId(int i) { return getRow(i)->word_id;}
	inline int getHashCode1(int i) { return getRow(i)->hash_code_1;}
	inline int getHashCode2(int i) { return getRow(i)->hash_code_2;}
	inline int getWordTypeCode(int i) { return getRow(i)->word_type_code;}
	inline int getWordCount(int i) { return getRow(i)->word_count;}
	inline int getEndingLength(int i) { return getRow(i)->ending_length;}
// Additions to the table used in gwordquery, gwordcapquery and phraseidquery
	inline int getRootHash1(int i) { return getRow(i)->root_hash_1;}
	inline int getRootHash2(int i) { return getRow(i)->root_hash_2;}
	inline int getRootHen1(int i) { return getRow(i)->root_hen_1;}
	inline int getRootHen2(int i) { return getRow(i)->root_hen_2;}
	inline int getHenNum1(int i) { return getRow(i)->hen_num_1;}
	inline int getHenNum2(int i) { return getRow(i)->hen_num_2;}
};

#endif // _CacheGphraseDataClass
