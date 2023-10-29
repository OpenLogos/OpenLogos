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
#include <logos_libs/dbcache/CacheConstantCodeData.h>
#include <logos_libs/dbcache/CacheKey.h>
#include <logos_libs/dbcache/CacheHashTab.h>

static DWORD t0, t1, t2;
static double thash, tload;

CacheConstantCodeData::CacheConstantCodeData(SqlConnection *con, int langcode, bool loader, 
									bool fromFile, bool saveIt) :
	CacheData(con, langcode, "ConstantCode", loader, fromFile, saveIt) {

	row_len = sizeof(struct h_row);
	max_key_len = CONSTANT_CODE_KEYLEN;

	mapAndLoad();
}

void
CacheConstantCodeData::getSizeFromDB() {
	stmt = (SqlStatement *)oracleConnection->CreateStatement();
	stmtText = "select count(*) "
#ifdef USE_ORACLE8
		" from constant_pointer a, morphology b, morphology c "
			" where a.language_code=:lc "
	  " and b.usage_id = a.primary_usage_id "
	  " and c.usage_id (+) = a.alternate_usage_id";
#else
		" from constant_pointer a LEFT OUTER JOIN morphology c on c.usage_id = a.alternate_usage_id, morphology b "
			" where a.language_code=:lc "
	  " and b.usage_id = a.primary_usage_id ";
#endif

	stmt->AddToCommandString(stmtText);
	stmt->Parse();

	stmt->BindInputIntToString(":lc","%2d",language_id,language_code);

	SqlColumn *cnt=stmt->BindOutputColumn(1, SqlColumn::Integer);

	stmt->Execute();

	stmt->Fetch();

	size = cnt->AsInteger() + 1;
//printf("CacheConstantCode size from DB = %d\n", size);
//fflush(stdout);
	delete stmt;
}

bool
CacheConstantCodeData::populateFromDatabase() {
	char keybuf[256];
	int weights[256];
	int nletters = 0;
	int printstep = 1024;

	printf("Loading from database (one # mark - %d records)\n",
		printstep);
	fflush(stdout);

	CLOCK_T t0 = GetTickCount();

        int i;
	for(i=0;i<256;i++)
		weights[i] = 0;

	stmt = (SqlStatement *)oracleConnection->CreateStatement();
	stmtText = "select a.constant_id, a.constant_type, a.company_code, "
			" a.alternate_usage_id, b.word_class_code, "
			" b.pat_number, b.gender_code, "
			" b.numeric_constraint, c.pat_number, c.gender_code "
#ifdef USE_ORACLE8
		" from constant_pointer a, morphology c, morphology b "
			" where a.language_code=:lc "
			" and b.usage_id = a.primary_usage_id "
                        " and c.usage_id (+) = a.alternate_usage_id "
			" order by a.constant_id, a.constant_type ";
#else
		" from constant_pointer a LEFT OUTER JOIN morphology c on c.usage_id = a.alternate_usage_id, morphology b "
			" where a.language_code=:lc "
			" and b.usage_id = a.primary_usage_id "
			" order by a.constant_id, a.constant_type ";
#endif

	stmt->AddToCommandString(stmtText);
	stmt->Parse();

	stmt->BindInputIntToString(":lc","%2d",language_id,language_code);

	SqlColumn *cid=stmt->BindOutputColumn(1, SqlColumn::Integer);
	SqlColumn *ctp=stmt->BindOutputColumn(2, SqlColumn::StringType);
	SqlColumn *cc=stmt->BindOutputColumn(3,SqlColumn::StringType);
	SqlColumn *auid=stmt->BindOutputColumn(4,SqlColumn::Integer);
	SqlColumn *wcc=stmt->BindOutputColumn(5,SqlColumn::StringType);
	SqlColumn *pn1=stmt->BindOutputColumn(6,SqlColumn::Integer);
	SqlColumn *gc1=stmt->BindOutputColumn(7,SqlColumn::Integer);
	SqlColumn *nc=stmt->BindOutputColumn(8,SqlColumn::StringType);
	SqlColumn *pn2=stmt->BindOutputColumn(9,SqlColumn::Integer);
	SqlColumn *gc2=stmt->BindOutputColumn(10,SqlColumn::Integer);

	stmt->Execute();

	nrows = 0;

// first load the data and calculate weights
	CacheKey *key = (CacheKey *)keybuf;
	while(stmt->Fetch()) {
		struct h_row *lrp = getRow(nrows);
		lrp->constant_id = cid->AsInteger();
		strcpy(lrp->constant_type, ctp->AsCharArray());
		strcpy(lrp->company_code, cc->AsCharArray());
		lrp->alternate_usage_id = auid->AsInteger();
		strcpy(lrp->word_class_code, wcc->AsCharArray());
		lrp->pat_number1 = pn1->AsInteger();
		lrp->gender_code1 = gc1->AsInteger();
		strcpy(lrp->numeric_constraint, nc->AsCharArray());
		lrp->pat_number2 = pn2->AsInteger();
		lrp->gender_code2 = gc2->AsInteger();
/*
printf("\"%s\" \"%s\" cid=%d puid=%d auid=%d cfc=%d\n", lrp->company_code,
	   lrp->constant_type, lrp->constant_id, 
	   lrp->primary_usage_id, lrp->alternate_usage_id, lrp->combining_form_code);
fflush(stdout);
*/

// calculating frequencies, key is (constant_type, constant_id)
		unsigned char *cp = (unsigned char *) lrp->constant_type;
		int len = strlen(lrp->constant_type);
		for(i=0;i<len;i++) {
			int val = *cp;
			weights[val]++;
			nletters++;
			cp++;
		}
		cp = (unsigned char *) & lrp->constant_id;
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
		struct h_row *lrp = getRow(j);
		makeKey(key, lrp->constant_type, lrp->constant_id, j);
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
CacheConstantCodeData::makeKey(CacheKey *key, char *ctp, int cid, int val) {
	char *keybuf = (char *)key->getBuffer();
	memset(key, 0, max_key_len);
	int len = strlen(ctp);
	memcpy(keybuf, ctp, len);
	keybuf += len;
	char *cp = (char *) & cid;
	memcpy(keybuf, cp, sizeof(int));
	len += sizeof(int);
	key->setLength(len);
	key->setValue(val);
	key->setNextIndex(-1);
}
