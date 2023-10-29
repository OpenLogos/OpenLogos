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
#include <logos_libs/dbcache/CacheTargetData.h>
#include <logos_libs/dbcache/CacheKey.h>
#include <logos_libs/dbcache/CacheHashTab.h>

static DWORD t0, t1, t2;
static double thash, tload;

CacheTargetData::CacheTargetData(SqlConnection *con, int langcode, bool loader, bool fromFile,
			 bool saveIt) :
	CacheData(con, langcode, "Target", loader, fromFile, saveIt) {

	row_len = sizeof(struct d_row);
	max_key_len = TARGET_KEYLEN;

	mapAndLoad();
}

void
CacheTargetData::getSizeFromDB() {
	stmt = (SqlStatement *)oracleConnection->CreateStatement();
	stmtText = "select count(*) "
		" from Word_Phrase w, Morphology m "
		" where "
		" w.language_code = :lc "
		" and w.Company_Code = m.Company_Code "
		" and w.Word_ID = m.word_id";

	stmt->AddToCommandString(stmtText);
	stmt->Parse();

	stmt->BindInputIntToString(":lc","%2d",language_id,language_code);

	SqlColumn *cnt = stmt->BindOutputColumn(1, SqlColumn::Integer);
	stmt->Execute();
	stmt->Fetch();
	size = cnt->AsInteger() + 1;
	delete stmt;
//printf("Target size from DB = %d\n", size);
//fflush(stdout);
}

bool
CacheTargetData::populateFromDatabase() {
	char keybuf[256];
	int weights[256];
	int nletters = 0;
	int printstep = 4096;

	printf("Loading from database (one # mark - %d records)\n",
		printstep);
	fflush(stdout);

	CLOCK_T t0 = GetTickCount();

        int i;
	for(i=0;i<256;i++)
		weights[i] = 0;

	stmt = (SqlStatement *)oracleConnection->CreateStatement();
	stmtText =
"select w.Word_ID, w.Word, w.Word_Count, w.Word_Type_Code, w.Head_Word, "
" w.Black_Hole_Location, w.Wildcard_Position, w.Aspire_Switch, "
" w.Pat_Exception_Switch, "
" m.Word_Class_Code, "
" m.Pat_Number, "
" m.Gender_Code, m.Numeric_Constraint, m.Auxiliary_Code, "
" m.Verb_Prefix_Inseparable, m.Verb_Prefix_Separable, "
" m.Verb_Prefix_Inseparable_Length, m.Verb_Prefix_Separable_Length, "
" m.Source_Stem_Number, m.Root_Usage_ID, m.Source_Analysis_Code, "
" m.Usage_Id "
" from "
" Word_Phrase w, Morphology m "
" where "
" w.language_code = :lc "
" and w.Company_Code = m.Company_Code "
" and w.Word_ID = m.word_id";

	stmt->AddToCommandString(stmtText);
	stmt->Parse();

	stmt->BindInputIntToString(":lc","%2d",language_id,language_code);

	SqlColumn *wid=stmt->BindOutputColumn(1,SqlColumn::Integer);
	SqlColumn *word = stmt->BindOutputColumn(2,SqlColumn::StringType);
	SqlColumn *wc=stmt->BindOutputColumn(3,SqlColumn::Integer);
	SqlColumn *wtc=stmt->BindOutputColumn(4,SqlColumn::Integer);
	SqlColumn *hw=stmt->BindOutputColumn(5,SqlColumn::Integer);
	SqlColumn *bhl=stmt->BindOutputColumn(6,SqlColumn::Integer);
	SqlColumn *wp=stmt->BindOutputColumn(7,SqlColumn::Integer);
	SqlColumn *as=stmt->BindOutputColumn(8,SqlColumn::StringType);
	SqlColumn *pes=stmt->BindOutputColumn(9,SqlColumn::StringType);
	SqlColumn *wcc=stmt->BindOutputColumn(10,SqlColumn::StringType);

	SqlColumn *pn=stmt->BindOutputColumn(11,SqlColumn::Integer);
	SqlColumn *gc=stmt->BindOutputColumn(12,SqlColumn::Integer);
	SqlColumn *nc=stmt->BindOutputColumn(13,SqlColumn::Integer);
	SqlColumn *ac=stmt->BindOutputColumn(14,SqlColumn::Integer);
	SqlColumn *vpi=stmt->BindOutputColumn(15,SqlColumn::Integer);
	SqlColumn *vps=stmt->BindOutputColumn(16,SqlColumn::Integer);
	SqlColumn *vpil=stmt->BindOutputColumn(17,SqlColumn::Integer);
	SqlColumn *vpsl=stmt->BindOutputColumn(18,SqlColumn::Integer);
	SqlColumn *ssn=stmt->BindOutputColumn(19,SqlColumn::Integer);
	SqlColumn *ruid=stmt->BindOutputColumn(20,SqlColumn::Integer);
	SqlColumn *sac=stmt->BindOutputColumn(21,SqlColumn::Integer);
	SqlColumn *usid=stmt->BindOutputColumn(22, SqlColumn::Integer);

	stmt->Execute();

	nrows = 0;

// first load the data and calculate weights
	CacheKey *key = (CacheKey *)keybuf;
	while(stmt->Fetch()) {
		struct d_row *crp = getRow(nrows);
		crp->word_id = wid->AsInteger();
		strcpy(crp->word, word->AsCharArray());
		crp->word_count = wc->AsInteger();
		crp->word_type_code = wtc->AsInteger();
		crp->head_word = hw->AsInteger();
		crp->black_hole_location = bhl->AsInteger();
		crp->wildcard_position = wp->AsInteger();
		crp->aspire_switch[0] = as->AsCharArray()[0];
		crp->pat_exception_switch[0] = pes->AsCharArray()[0];
		crp->word_class_code = wcc->AsInteger();

		crp->pat_number = pn->AsInteger();
		crp->gender_code = gc->AsInteger();
		crp->numeric_constraint = nc->AsInteger();
		crp->aux_code = ac->AsInteger();
		crp->verb_prefix_insep = vpi->AsInteger();
		crp->verb_prefix_sep = vps->AsInteger();
		crp->verb_prefix_insep_len = vpil->AsInteger();
		crp->verb_prefix_sep_len = vpsl->AsInteger();
		crp->source_stem_number = ssn->AsInteger();
		crp->root_usage_id = ruid->AsInteger();
		crp->source_anal_code = sac->AsInteger();
		crp->usage_id = usid->AsInteger();

// calculating frequencies, key is (usage_id)
		unsigned char *cp = (unsigned char *) & crp->usage_id;
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
//printf("row # %d, usage_id = %d\n", j, drp->usage_id);
//fflush(stdout);
		struct d_row *drp = getRow(j);
		makeKey(key, drp->usage_id, j);
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
CacheTargetData::makeKey(CacheKey *key, int usid, int val) {
	char *keybuf = (char *)key->getBuffer();
	memset(key, 0, max_key_len);
	char *cp = (char *) & usid;
	memcpy(keybuf, cp, sizeof(int));
	int len = sizeof(int);
	key->setLength(len);
	key->setValue(val);
	key->setNextIndex(-1);
}
