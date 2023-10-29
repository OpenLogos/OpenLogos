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
#ifndef _CacheConstantCodeDataClass
#define _CacheConstantCodeDataClass

#define CONSTANT_CODE_KEYLEN 24	
	// sizeof(constant_id) + sizeof(constant_type) + 3 * sizeof(int)

#ifdef _MSC_VER
#include <windows.h>
#else
#include "logos_include/windows.h"
#endif
#include <logos_libs/dbcache/CacheData.h>

struct h_row {
	int constant_id;
	int alternate_usage_id;
	int pat_number1;
	int gender_code1;
	int gender_code2;
	int pat_number2;
	char constant_type[4];
	char numeric_constraint[2];
	char word_class_code[4];
	char company_code[4];
};

class DLLEXPORT CacheConstantCodeData : public CacheData {

public:

	CacheConstantCodeData(SqlConnection *con, int langcode, bool loader, bool ff, bool sav);
	void makeKey(CacheKey *, char *ctp, int cid, int val);
	virtual bool 	populateFromDatabase();
	virtual void getSizeFromDB();
	inline struct h_row * getRow(int i) {
		struct h_row *lrp = 
		(struct h_row *)((char *)data_array + i*sizeof(struct h_row));
		return lrp;
	}

/*
Statement "H"

select a.company_code, a.alternate_usage_id, b.word_class_code,
	b.pat_number, b.gender_code, b.numeric_constraint,
	c.pat_number, c.gender_code
from constant_pointer a, morphology b, morphology c
	where constant_id=:cid and constant_type=:ctp
	and language_code=:lc and b.usage_id = a.primary_usage_id
	and c.usage_id = a.alternate_usage_id
*/

	inline char * getConstantType(int i){
		return getRow(i)->constant_type;}
	inline int getConstantId(int i) {
		return getRow(i)->constant_id;}
	inline char * getCompanyCode(int i){return getRow(i)->company_code;}
	inline int getAlternateUsageId(int i) {
		return getRow(i)->alternate_usage_id;}
	inline char * getWordClassCode(int i) {
		return getRow(i)->word_class_code;}
	inline int getPatNumber1(int i) {
		return getRow(i)->pat_number1;}
	inline int getPatNumber2(int i) {
		return getRow(i)->pat_number2;}
	inline int getGenderCode1(int i) {
		return getRow(i)->gender_code1;}
	inline int getGenderCode2(int i) {
		return getRow(i)->gender_code2;}
	inline char * getNumericConstraint(int i) {
		return getRow(i)->numeric_constraint;}
};

#endif // _CacheConstantCodeDataClass
