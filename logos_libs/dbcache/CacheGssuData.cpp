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
#include <logos_libs/dbcache/CacheGssuData.h>
#include <logos_libs/dbcache/CacheKey.h>
#include <logos_libs/dbcache/CacheHashTab.h>

static DWORD t0, t1, t2;
static double thash, tload;

CacheGssuData::CacheGssuData(SqlConnection *con, int langcode, bool loader, bool fromFile,
			 bool saveIt):
	CacheData(con, langcode, "GSSU", loader, fromFile, saveIt) {

	row_len = sizeof(struct a_row);
	max_key_len = GSSU_KEYLEN;

	mapAndLoad();
}

void
CacheGssuData::getSizeFromDB() {
	stmt = (SqlStatement *)oracleConnection->CreateStatement();
	stmtText =
		"select count(*) "
		"from morphology mr, meaning mn "
		"where mr.language_code=:lc "
		"and mn.company_code=mr.company_code "
		"and mn.language_code=mr.language_code "
		"and mn.usage_id=mr.usage_id "
		"and mr.source_analysis_code='1' "
		"and mn.primary_switch='Y'";

	stmt->AddToCommandString(stmtText);
	stmt->Parse();

	stmt->BindInputIntToString(":lc","%2d",language_id,language_code);

	SqlColumn *cnt = stmt->BindOutputColumn(1 ,SqlColumn::Integer);
	stmt->Execute();
	stmt->Fetch();
	size = cnt->AsInteger() + 1;
	delete stmt;
//printf("GSSU size from DB = %d\n", size);
//fflush(stdout);
}

void
CacheGssuData::makeKey(CacheKey *key, char *cc, int wordid, int val) {
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

bool
CacheGssuData::populateFromDatabase() {
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
		"select mr.company_code, mr.word_id, mr.word_class_code,"
		"mr.gender_code, mr.pat_number,"
		"mr.source_stem_number, mr.usage_id, mr.auxiliary_code,"
		"mn.meaning_id, mn.set_id, mn.subset_id, mn.superset_id,"
		"mn.priority_order, mn.form_code,"
		"mn.subject_matter_code, mn.atomic_code, mn.generic_code, "
		"mn.transitivity "
		"from morphology mr, meaning mn "
		"where mr.language_code=:lc "
		"and mn.company_code=mr.company_code "
		"and mn.language_code=mr.language_code "
		"and mn.usage_id=mr.usage_id "
		"and mr.source_analysis_code='1' "
		"and mn.primary_switch='Y' "
//		"order by mn.priority_order";
		"order by mr.word_id, mn.priority_order";

	stmt->AddToCommandString(stmtText);
	stmt->Parse();

	stmt->BindInputIntToString(":lc","%2d",language_id,language_code);

	SqlColumn *cc=stmt->BindOutputColumn(1,SqlColumn::StringType);
	SqlColumn *wid = stmt->BindOutputColumn(2,SqlColumn::Integer);
	SqlColumn *wcc = stmt->BindOutputColumn(3,SqlColumn::Integer);
	SqlColumn *gc = stmt->BindOutputColumn(4, SqlColumn::Integer);
	SqlColumn *pn = stmt->BindOutputColumn(5, SqlColumn::Integer);
	SqlColumn *ssn = stmt->BindOutputColumn(6,SqlColumn::Integer);
	SqlColumn *usid=stmt->BindOutputColumn(7,SqlColumn::Integer);
	SqlColumn *ac = stmt->BindOutputColumn(8,SqlColumn::Integer);
	SqlColumn *mid = stmt->BindOutputColumn(9,SqlColumn::Integer);
	SqlColumn *setid = stmt->BindOutputColumn(10, SqlColumn::Integer);
	SqlColumn *subsetid = stmt->BindOutputColumn(11, SqlColumn::Integer);
	SqlColumn *supersetid = stmt->BindOutputColumn(12,SqlColumn::Integer);
	SqlColumn *po = stmt->BindOutputColumn(13, SqlColumn::Integer);
	SqlColumn *fc = stmt->BindOutputColumn(14, SqlColumn::Integer);
	SqlColumn *smc = stmt->BindOutputColumn(15, SqlColumn::StringType);
	SqlColumn *acode = stmt->BindOutputColumn(16, SqlColumn::Integer);
	SqlColumn *gcode = stmt->BindOutputColumn(17, SqlColumn::Integer);
	SqlColumn *tr = stmt->BindOutputColumn(18, SqlColumn::StringType);

	stmt->Execute();

	nrows = 0;

// first load the data and calculate weights
	CacheKey *key = (CacheKey *)keybuf;
	while(stmt->Fetch()) {
		struct a_row *dt = (struct a_row *)((char *)data_array + nrows*row_len);
		memcpy(dt->company_code, cc->AsCharArray(), 4);
		dt->word_id = wid->AsInteger();
		dt->word_class_code = wcc->AsInteger();
		dt->gender_code = gc->AsInteger();
		dt->pat_number = pn->AsInteger();
		dt->source_stem_number = ssn->AsInteger();
		dt->usage_id = usid->AsInteger();
		dt->aux_code = ac->AsInteger();
		dt->meaning_id = mid->AsInteger();
		dt->set_id = setid->AsInteger();
		dt->subset_id = subsetid->AsInteger();
		dt->superset_id = supersetid->AsInteger();
		dt->priority_order = po->AsInteger();
		dt->form_code = fc->AsInteger();
		memcpy(dt->smc, smc->AsCharArray(), 7);
		dt->atomic_code = acode->AsInteger();
		dt->generic_code = gcode->AsInteger();
		memcpy(dt->transitivity, tr->AsCharArray(), 2);


//		if(debug) {
//		printf("AQuery row %d: wid=%d, mid=%d\n", nrows, wid->AsInteger(),
//			mid->AsInteger());
//		fflush(stdout);

// calculating frequencies, key is (company_code + word_id)
		unsigned char *cp = (unsigned char *)cc->AsCharArray();
		int len = strlen((char *)cp);
		for(i=0;i<len;i++) {
			int val = *cp;
			weights[val]++;
			nletters++;
			cp++;
		}
		cp = (unsigned char *) & dt->word_id;
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
		struct a_row *dt = getRow(j);
		makeKey(key, dt->company_code, dt->word_id, j);
		hashtab->put(key);
	}

	CLOCK_T t2 = GetTickCount();
	thash = (t2-t1)/1000.;
	printf("done, time %4.1f seconds\n", thash);

	hashtab->calculateStat();
	hashtab->print();

	fflush(stdout);

	return true;
}
