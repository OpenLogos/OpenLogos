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
#include <logos_libs/dbcache/CacheStemgenRuleData.h>
#include <logos_libs/dbcache/CacheKey.h>
#include <logos_libs/dbcache/CacheHashTab.h>

static DWORD t0, t1, t2;
static double thash, tload;

CacheStemgenRuleData::CacheStemgenRuleData(SqlConnection *con, int langcode, bool loader, bool fromFile,
			 bool saveIt) :
	CacheData(con, langcode, "StemgenRule", loader, fromFile, saveIt) {

	row_len = sizeof(struct l_row);
	max_key_len = STEMGEN_RULE_KEYLEN;

	mapAndLoad();
}

void
CacheStemgenRuleData::getSizeFromDB() {
	stmt = (SqlStatement *)oracleConnection->CreateStatement();
	stmtText = "select count(*) "
		" from stem_generation_rule "
		" where language_code = :lc";

	stmt->AddToCommandString(stmtText);
	stmt->Parse();

	stmt->BindInputIntToString(":lc","%2d",language_id,language_code);

	SqlColumn *cnt=stmt->BindOutputColumn(1,SqlColumn::Integer);
	stmt->Execute();
	stmt->Fetch();
	size = cnt->AsInteger() + 1;
	delete stmt;
//printf("StemgenRule size from DB = %d\n", size);
//fflush(stdout);
}

bool
CacheStemgenRuleData::populateFromDatabase() {
	char keybuf[256];
	int weights[256];
	int nletters = 0;
	int printstep = 256;

	printf("Loading from database (one # mark - %d records)\n",
		printstep);
	fflush(stdout);

	CLOCK_T t0 = GetTickCount();

        int i;
	for(i=0;i<256;i++)
		weights[i] = 0;

	stmt = (SqlStatement *)oracleConnection->CreateStatement();
	stmtText = "select pat_number, stem_number, add_ending, "
		" drop_ending, replace_rule, add_prefix "
		" from stem_generation_rule "
		" where language_code = :lc";

	stmt->AddToCommandString(stmtText);
	stmt->Parse();

	stmt->BindInputIntToString(":lc","%2d",language_id,language_code);

	SqlColumn *pn=stmt->BindOutputColumn(1,SqlColumn::Integer);
	SqlColumn *sn=stmt->BindOutputColumn(2,SqlColumn::Integer);
	SqlColumn *ae=stmt->BindOutputColumn(3,SqlColumn::StringType);
	SqlColumn *de=stmt->BindOutputColumn(4,SqlColumn::StringType);
	SqlColumn *rr=stmt->BindOutputColumn(5,SqlColumn::StringType);
	SqlColumn *ap=stmt->BindOutputColumn(6,SqlColumn::StringType);

	stmt->Execute();

	nrows = 0;

// first load the data and calculate weights
	CacheKey *key = (CacheKey *)keybuf;
	while(stmt->Fetch()) {
		struct l_row *lrp = getRow(nrows);
		lrp->pat_number = pn->AsInteger();
		lrp->stem_number = sn->AsInteger();
		strcpy(lrp->add_ending, ae->AsCharArray());
		strcpy(lrp->drop_ending, de->AsCharArray());
		strcpy(lrp->replace_rule, rr->AsCharArray());
		strcpy(lrp->add_prefix, ap->AsCharArray());

// calculating frequencies, key is (pat_numer, stem_number)
		unsigned char *cp = (unsigned char *) & lrp->pat_number;
		for(i=0;i<sizeof(int);i++) {
			int val = *cp;
			weights[val]++;
			nletters++;
			cp++;
		}
		cp = (unsigned char *) & lrp->stem_number;
		for(i=0;i<sizeof(int);i++) {
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
//printf("row # %d, usage_id = %d\n", j, lrp->usage_id);
//fflush(stdout);
		struct l_row *lrp = getRow(j);
		makeKey(key, lrp->pat_number, lrp->stem_number, j);
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
CacheStemgenRuleData::makeKey(CacheKey *key, int pn, int sn, int val) {
	char *keybuf = (char *)key->getBuffer();
	memset(key, 0, max_key_len);
	char *cp = (char *) & pn;
	memcpy(keybuf, cp, sizeof(int));
	keybuf += sizeof(int);
	cp = (char *) & sn;
	memcpy(keybuf, cp, sizeof(int));
	int len = sizeof(int) << 1;
	key->setLength(len);
	key->setValue(val);
	key->setNextIndex(-1);
}
