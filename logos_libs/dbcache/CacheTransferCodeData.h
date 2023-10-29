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
#ifndef _CacheTransferCodeDataClass
#define _CacheTransferCodeDataClass

#define TRANSFER_CODE_KEYLEN 20	// sizeof(meaning_id) + sizeof(company_code)
					// + 3 * sizeof(int)

#ifdef _MSC_VER
#include <windows.h>
#else
#include "logos_include/windows.h"
#endif
#include <logos_libs/dbcache/CacheData.h>

struct c_row {
	char company_code[4];
	int alternate_sequence;
	int overflow2a;
	int overflow2b;
	int overflow3a;
	int overflow3b;
	int word_class_code;
	int pat_number;
	int gender_code;
	int meaning_id;
};

class DLLEXPORT CacheTransferCodeData : public CacheData {

public:

	CacheTransferCodeData(SqlConnection *con, int langcode, bool loader, bool ff, bool sav);
	void makeKey(CacheKey *, char *cc, int meaningid, int val);
	virtual bool 	populateFromDatabase();
	virtual void getSizeFromDB();
	inline struct c_row * getRow(int i) {
		struct c_row *crp = 
		(struct c_row *)((char *)data_array + i*sizeof(struct c_row));
		return crp;
	}

// Statement C
// select a.company_code, a.alternate_sequence, b.overflow2a,
//	b.overflow2b, b.overflow3a, b.overflow3b, c.word_class_code,
//	c.pat_number, c.gender_code
// from transfer a, overflow b, morphology c
// where
//	a.meaning_id = :1
//	and a.company_code = :2
//	and a.target_language_code = :3
//	and a.deactivation_switch = 'N'
//	and (a.alternate_sequence < 2 or a.alternate_sequence is NULL)
//	and b.company_code (+) = a.company_code
//	and b.transfer_id (+) = a.transfer_id
//	and c.company_code = a.company_code
//	and c.usage_id = a.target_usage_id

	inline char * getCompanyCode(int i) {
		return getRow(i)->company_code; }
	inline int getAlternateSequence(int i) {
		return getRow(i)->alternate_sequence; }
	inline int getOverflow2a(int i) { return getRow(i)->overflow2a; }
	inline int getOverflow2b(int i) { return getRow(i)->overflow2b; }
	inline int getOverflow3a(int i) { return getRow(i)->overflow3a; }
	inline int getOverflow3b(int i) { return getRow(i)->overflow3b; }
	inline int getWordClassCode(int i) { return getRow(i)->word_class_code; }
	inline int getGenderCode(int i) { return getRow(i)->gender_code;}
	inline int getPatNumber(int i) { return getRow(i)->pat_number; }
	inline int getMeaningId(int i) { return getRow(i)->meaning_id; }

};

#endif // _CacheTransferCodeDataClass
