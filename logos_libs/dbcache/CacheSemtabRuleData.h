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
#ifndef _CacheSemtabRuleDataClass
#define _CacheSemtabRuleDataClass

#define SEMTAB_RULE_KEYLEN 32	
			// sizeof(company_code)
			// sizeof(deactivation_switch)
			// + sizeof(word_class_code)
			// + sizeof(set_id)
			// + sizeof(subset_id)
			// + 3 * sizeof(int)

#ifdef _MSC_VER
#include <windows.h>
#else
#include "logos_include/windows.h"
#endif
#include <logos_libs/dbcache/CacheData.h>

struct f_row {
	char company_code[4];
	int source_language_code;
	int target_language_code;
	char deactivation_switch[2];
	int semtab_id;
	int word_class_code;
	int subset_id;
	int set_id;

	int wc1;
	int ty1;
	int fm1;

	int wc2;
	int ty2;
	int fm2;

	int wc3;
	int ty3;
	int fm3;

	int wc4;
	int ty4;
	int fm4;

	int wc5;
	int ty5;
	int fm5;

	int wc6;
	int ty6;
	int fm6;

	int wc7;
	int ty7;
	int fm7;
	
	int wc8;
	int ty8;
	int fm8;
	
	int wc9;
	int ty9;
	int fm9;

	int tag11;
	int tag12;
	int tag21;
	int tag22;
	int tag31;
	int tag32;
	int tag41;
	int tag42;
	int tag51;
	int tag52;
	int tag61;
	int tag62;
	int tag71;
	int tag72;
	int tag81;
	int tag82;
	int tag91;
	int tag92;

	int rule_level;
	int specificity;
	int numbytesindatablob;

	char data_blob[165];
};

class DLLEXPORT CacheSemtabRuleData : public CacheData {

public:

	CacheSemtabRuleData(SqlConnection *con, int srcl, int targl,
		bool loader, bool ff, bool sav);
	void makeKey(CacheKey *k,char *cc,char ds,int wcc, int subsid, int sid, int val);
	virtual bool 	populateFromDatabase();
	virtual void getSizeFromDB();
	inline struct f_row * getRow(int i) {
		struct f_row *lrp = 
		(struct f_row *)((char *)data_array + i*sizeof(struct f_row));
		return lrp;
	}

	inline char * getCompanyCode(int i){return getRow(i)->company_code;}
	inline int getSourceLanguageCode(int i) {
		return getRow(i)->source_language_code;}
	inline int getTargetLanguageCode(int i) {
		return getRow(i)->target_language_code;}
	inline char getDeactivationSwitch(int i){
		return getRow(i)->deactivation_switch[0];}
	
	inline int getSemtabId(int i) { return getRow(i)->semtab_id;}
	inline int getWordClassCode(int i) {
		return getRow(i)->word_class_code;}
	inline int getSubsetId(int i) { return getRow(i)->subset_id;}
	inline int getSetId(int i) { return getRow(i)->set_id;}

	inline int getWc1(int i) { return getRow(i)->wc1;}
	inline int getTy1(int i) { return getRow(i)->ty1;}
	inline int getFm1(int i) { return getRow(i)->fm1;}

	inline int getWc2(int i) { return getRow(i)->wc2;}
	inline int getTy2(int i) { return getRow(i)->ty2;}
	inline int getFm2(int i) { return getRow(i)->fm2;}

	inline int getWc3(int i) { return getRow(i)->wc3;}
	inline int getTy3(int i) { return getRow(i)->ty3;}
	inline int getFm3(int i) { return getRow(i)->fm3;}

	inline int getWc4(int i) { return getRow(i)->wc4;}
	inline int getTy4(int i) { return getRow(i)->ty4;}
	inline int getFm4(int i) { return getRow(i)->fm4;}

	inline int getWc5(int i) { return getRow(i)->wc5;}
	inline int getTy5(int i) { return getRow(i)->ty5;}
	inline int getFm5(int i) { return getRow(i)->fm5;}

	inline int getWc6(int i) { return getRow(i)->wc6;}
	inline int getTy6(int i) { return getRow(i)->ty6;}
	inline int getFm6(int i) { return getRow(i)->fm6;}

	inline int getWc7(int i) { return getRow(i)->wc7;}
	inline int getTy7(int i) { return getRow(i)->ty7;}
	inline int getFm7(int i) { return getRow(i)->fm7;}

	inline int getWc8(int i) { return getRow(i)->wc8;}
	inline int getTy8(int i) { return getRow(i)->ty8;}
	inline int getFm8(int i) { return getRow(i)->fm8;}

	inline int getWc9(int i) { return getRow(i)->wc9;}
	inline int getTy9(int i) { return getRow(i)->ty9;}
	inline int getFm9(int i) { return getRow(i)->fm9;}

	inline int getTag11(int i) { return getRow(i)->tag11;}
	inline int getTag12(int i) { return getRow(i)->tag12;}
	inline int getTag21(int i) { return getRow(i)->tag21;}
	inline int getTag22(int i) { return getRow(i)->tag22;}
	inline int getTag31(int i) { return getRow(i)->tag31;}
	inline int getTag32(int i) { return getRow(i)->tag32;}
	inline int getTag41(int i) { return getRow(i)->tag41;}
	inline int getTag42(int i) { return getRow(i)->tag42;}
	inline int getTag51(int i) { return getRow(i)->tag51;}
	inline int getTag52(int i) { return getRow(i)->tag52;}
	inline int getTag61(int i) { return getRow(i)->tag61;}
	inline int getTag62(int i) { return getRow(i)->tag62;}
	inline int getTag71(int i) { return getRow(i)->tag71;}
	inline int getTag72(int i) { return getRow(i)->tag72;}
	inline int getTag81(int i) { return getRow(i)->tag81;}
	inline int getTag82(int i) { return getRow(i)->tag82;}
	inline int getTag91(int i) { return getRow(i)->tag91;}
	inline int getTag92(int i) { return getRow(i)->tag92;}

	inline int getRuleLevel(int i) { return getRow(i)->rule_level;}
	inline int getSpecificity(int i) { return getRow(i)->specificity;}
	inline int getNumBytesInDataBlob(int i) {
		return getRow(i)->numbytesindatablob;}

	inline char * getDataBlob(int i) { return getRow(i)->data_blob;}
};

#endif // _CacheSemtabRuleDataClass
