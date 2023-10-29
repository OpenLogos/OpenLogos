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

#include <logos_include/logoscommon.h>
#include <logos_libs/sql/sqlconnection.h>
#include <logos_libs/sql/sqlstatement.h>
#ifdef _MSC_VER
#include <windows.h>
#else
#include "logos_include/windows.h"
#endif
#include <logos_libs/dbcache/CachePatternRulesData.h>
#include <logos_libs/dbcache/CacheKey.h>
#include <logos_libs/dbcache/CacheHashTab.h>

static DWORD t0, t1, t2;
static double thash, tload;

CachePatternRulesData::CachePatternRulesData(SqlConnection *con, 
		int srcl, int targl, bool loader, bool fromFile, bool saveIt) :
	CacheData(con, srcl, targl, "PatternRules", loader, fromFile, saveIt) {

	row_len = sizeof(struct pm_row);
	max_key_len = (3*sizeof(int))+PATTERN_RULE_KEYLEN;

	mapAndLoad();

}

void
CachePatternRulesData::getSizeFromDB() {
	stmt = (SqlStatement *)oracleConnection->CreateStatement();
	stmtText = " select count(*) "
		" from pattern_rules "
		" where source_language_code=:sl and target_language_code=:tl "
		" and deactivation_switch='N'";

	stmt->AddToCommandString(stmtText);
	stmt->Parse();

	stmt->BindInputIntToString(":sl","%2d",source_language_id,source_language);
	stmt->BindInputIntToString(":tl","%2d",target_language_id,target_language);

	SqlColumn *cnt = stmt->BindOutputColumn(1, SqlColumn::Integer);
	stmt->Execute();
	stmt->Fetch();
	size = cnt->AsInteger() + 1;
	delete stmt;
//printf("PatternRule size from DB = %d\n", size);
//fflush(stdout);
}

bool
CachePatternRulesData::populateFromDatabase() {
	char keybuf[256];
	int weights[256];
	int nletters = 0;
	int printstep = 1;

	printf("Loading from database (one # mark - %d records)\n",
		printstep);
	fflush(stdout);

	CLOCK_T t0 = GetTickCount();

        int i;
	for(i=0;i<256;i++)
		weights[i] = 0;

	stmt = (SqlStatement *)oracleConnection->CreateStatement();
	stmtText = " select Company_Code, sequence_number, "
		" rule_type_code, search_expression, "
		" replace_expression, comment_line "
		" from pattern_rules "
		" where source_language_code=:sl and target_language_code=:tl"
		" and deactivation_switch='N' "
		" order by company_code, sequence_number";

	stmt->AddToCommandString(stmtText);
	stmt->Parse();

	stmt->BindInputIntToString(":sl","%2d",source_language_id,source_language);
	stmt->BindInputIntToString(":tl","%2d",target_language_id,target_language);

	SqlColumn *cc=stmt->BindOutputColumn(1,SqlColumn::StringType);
	SqlColumn *sn=stmt->BindOutputColumn(2, SqlColumn::Integer);
	SqlColumn *rtc=stmt->BindOutputColumn(3,SqlColumn::StringType);
	SqlColumn *se=stmt->BindOutputColumn(4,SqlColumn::StringType);
	SqlColumn *re=stmt->BindOutputColumn(5,SqlColumn::StringType);
	SqlColumn *cl=stmt->BindOutputColumn(6,SqlColumn::StringType);

	stmt->Execute();

	nrows = 0;

// first load the data and calculate weights
	CacheKey *key = (CacheKey *)keybuf;
	while(stmt->Fetch()) {
		struct pm_row *lrp = getRow(nrows);
		strncpy(lrp->company_code, cc->AsCharArray(), 3);
		lrp->sequence_number = sn->AsInteger();
		strncpy(lrp->rule_type_code, rtc->AsCharArray(), 1);
		strncpy(lrp->search_expression, se->AsCharArray(), 500);
		strncpy(lrp->replace_expression, re->AsCharArray(), 500);
		strncpy(lrp->comment_line, cl->AsCharArray(), 500);

// key is (company_code)
		unsigned char *cp = (unsigned char *) lrp->company_code;
		int len = strlen(lrp->company_code);
		for(i=0;i<len;i++) {
			int val = *cp;
			weights[val]++;
			nletters++;
			cp++;
		}

		nrows++;

		if(nrows%printstep==0) {
			printf("#");
			fflush(stdout);
		}
	}

	delete stmt;

// calc weight and reverse it because it's faster to multiply than divide
// (later, when computing hash runtime).
        int j;
	for(j=0;j<256;j++) {
		if(weights[j])
			weights_array[j] = nletters/((double)weights[j]);
		else
			weights_array[j] = 0.;
	}
	CLOCK_T t1 = GetTickCount();
	tload = (t1-t0)/1000.;
	printf("\nDone loading, %d records, load time %4.1f sec\n", nrows, tload);

	printf("Hashing...");
	fflush(stdout);
	for(j=0;j<nrows;j++) {
		struct pm_row *lrp = getRow(j);
		makeKey(key, lrp->company_code, j);
		hashtab->put(key);
	}

	CLOCK_T t2 = GetTickCount();
	thash = (t2-t1)/1000.;
	printf("done, time %4.1f sec\n", thash);

	hashtab->calculateStat();
	hashtab->print();

	fflush(stdout);

	return true;
}

void
CachePatternRulesData::makeKey(CacheKey *key, char *cc, int val) {
// Hash is not actually used in CachePatternRulesQuery.
	char *keybuf = (char *)key->getBuffer();
	memset( keybuf,0, max_key_len);

	int len = strlen(cc);
	memcpy(keybuf, cc, len);
	key->setLength(len);
	key->setValue(val);
	key->setNextIndex(-1);

//printf("\"%s\", %c %d %d %d: hash %d, val %d\n", cc, ds, wcc, subsid, sid,
//	   hashtab->computeHashIndex(key), val);
//fflush(stdout);
}
