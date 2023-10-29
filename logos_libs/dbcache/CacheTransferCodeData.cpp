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
#include <logos_libs/dbcache/CacheTransferCodeData.h>
#include <logos_libs/dbcache/CacheKey.h>
#include <logos_libs/dbcache/CacheHashTab.h>

static DWORD t0, t1, t2;
static double thash, tload;

CacheTransferCodeData::CacheTransferCodeData(SqlConnection *con, int langcode, bool loader, bool fromFile,
			 bool saveIt) :
	CacheData(con, langcode, "TransferCode", loader, fromFile, saveIt) {

	row_len = sizeof(struct c_row);
	max_key_len = TRANSFER_CODE_KEYLEN;

	mapAndLoad();
}

void
CacheTransferCodeData::getSizeFromDB() {
	stmt = (SqlStatement *)oracleConnection->CreateStatement();
	stmtText =
		"select count(*) "
#ifdef USE_ORACLE8
		"from transfer a, Overflow b, morphology c "
		"where "
			" a.target_language_code = :lc "
			" and a.deactivation_switch = 'N' "
			" and (a.alternate_sequence < 2 "
				" or a.alternate_sequence is NULL) "
                        " and b.company_code (+) = a.company_code "
	                " and b.transfer_id (+) = a.transfer_id ";
#else
		"from transfer a LEFT OUTER JOIN Overflow b ON b.Company_Code = a.Company_Code AND b.Transfer_ID = a.Transfer_ID, morphology c "
		"where "
			" a.target_language_code = :lc "
			" and a.deactivation_switch = 'N' "
			" and (a.alternate_sequence < 2 "
				" or a.alternate_sequence is NULL) "
			" and c.company_code = a.company_code "
			" and c.usage_id = a.target_usage_id ";
#endif
	stmt->AddToCommandString(stmtText);
	stmt->Parse();

	stmt->BindInputIntToString(":lc","%2d",language_id,language_code);

	SqlColumn *cnt = stmt->BindOutputColumn(1, SqlColumn::Integer);
	stmt->Execute();
	stmt->Fetch();
	size = cnt->AsInteger() + 1;
	delete stmt;
//printf("TransferCode size from DB = %d\n", size);
//fflush(stdout);
}

bool
CacheTransferCodeData::populateFromDatabase() {
	char keybuf[256];
	int weights[256];
	int nletters = 0;
	int printstep = 4096;

	printf("Loading from database (# - %d records)\n", printstep);
	fflush(stdout);

	CLOCK_T t0 = GetTickCount();

        int i;
	for(i=0;i<256;i++)
		weights[i] = 0;

	stmt = (SqlStatement *)oracleConnection->CreateStatement();
	stmtText =
		"select a.company_code, a.alternate_sequence, b.overflow2a,"
			"b.overflow2b, b.overflow3a, b.overflow3b, "
			"c.word_class_code, c.pat_number, c.gender_code, "
			"a.meaning_id "
#ifdef USE_ORACLE8
		"from transfer a, Overflow b, morphology c "
		"where "
			" a.target_language_code = :lc "
			" and a.deactivation_switch = 'N' "
			" and (a.alternate_sequence < 2 "
				" or a.alternate_sequence is NULL) "
                        " and b.company_code (+) = a.company_code "
                        " and b.transfer_id (+) = a.transfer_id "
			" and c.company_code = a.company_code "
			" and c.usage_id = a.target_usage_id "
			" order by a.meaning_id, a.company_code ";
#else

		"from transfer a LEFT OUTER JOIN Overflow b ON b.Company_Code = a.Company_Code AND b.Transfer_ID = a.Transfer_ID, morphology c "
		"where "
			" a.target_language_code = :lc "
			" and a.deactivation_switch = 'N' "
			" and (a.alternate_sequence < 2 "
				" or a.alternate_sequence is NULL) "
			" and c.company_code = a.company_code "
			" and c.usage_id = a.target_usage_id "
			" order by a.meaning_id, a.company_code ";
#endif
	stmt->AddToCommandString(stmtText);
	stmt->Parse();

	stmt->BindInputIntToString(":lc","%2d",language_id,language_code);

	SqlColumn *cc=stmt->BindOutputColumn(1,SqlColumn::StringType);
	SqlColumn *as = stmt->BindOutputColumn(2,SqlColumn::Integer);
	SqlColumn *o2a = stmt->BindOutputColumn(3,SqlColumn::Integer);
	SqlColumn *o2b = stmt->BindOutputColumn(4,SqlColumn::Integer);
	SqlColumn *o3a = stmt->BindOutputColumn(5,SqlColumn::Integer);
	SqlColumn *o3b = stmt->BindOutputColumn(6,SqlColumn::Integer);
	SqlColumn *wcc = stmt->BindOutputColumn(7,SqlColumn::Integer);
	SqlColumn *pn = stmt->BindOutputColumn(8,SqlColumn::Integer);
	SqlColumn *gc = stmt->BindOutputColumn(9,SqlColumn::Integer);
	SqlColumn *mid = stmt->BindOutputColumn(10,SqlColumn::Integer);

	stmt->Execute();

	nrows = 0;

// first load the data and calculate weights
	CacheKey *key = (CacheKey *)keybuf;
	while(stmt->Fetch()) {
		struct c_row *crp = getRow(nrows);
		memcpy(crp->company_code, cc->AsCharArray(), 4);
		crp->alternate_sequence = as->AsInteger();
		crp->overflow2a = o2a->AsInteger();
		crp->overflow2b = o2b->AsInteger();
		crp->overflow3a = o3a->AsInteger();
		crp->overflow3b = o3b->AsInteger();
		crp->word_class_code = wcc->AsInteger();
		crp->pat_number = pn->AsInteger();
		crp->gender_code = gc->AsInteger();
		crp->meaning_id = mid->AsInteger();

// calculating frequencies, key is (company_code + meaning_id)
		unsigned char *cp = (unsigned char *)cc->AsCharArray();
		int len = strlen((char *)cp);
		for(i=0;i<len;i++) {
			int val = *cp;
			weights[val]++;
			nletters++;
			cp++;
		}
		cp = (unsigned char *) & crp->meaning_id;
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
		struct c_row *crp = getRow(j);
		makeKey(key, crp->company_code, crp->meaning_id, j);
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
CacheTransferCodeData::makeKey(CacheKey *key, char *cc, int wordid, int val) {
	char *keybuf = (char *)key->getBuffer();
	memset(key, 0, max_key_len);
	int len = strlen(cc);
	memcpy(keybuf, cc, len);
	keybuf += len;
	char *cp = (char *) & wordid;
	memcpy(keybuf, cp, sizeof(int));
	len += sizeof(int);
	key->setLength(len);
	key->setValue(val);
	key->setNextIndex(-1);
}
