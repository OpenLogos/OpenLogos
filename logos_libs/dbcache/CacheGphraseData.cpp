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
#include <logos_libs/dbcache/CacheGphraseData.h>
#include <logos_libs/dbcache/CacheKey.h>
#include <logos_libs/dbcache/CacheHashTab.h>

static DWORD t0, t1, t2;
static double thash, tload;

CacheGphraseData::CacheGphraseData(SqlConnection *con, int langcode, bool loader, bool fromFile,
			 bool saveIt):
	CacheData(con, langcode, "GPhrase", loader, fromFile, saveIt) {

	row_len = sizeof(struct b_row);
	max_key_len = GPHRASE_KEYLEN;

	mapAndLoad();
}

void
CacheGphraseData::getSizeFromDB() {
	stmt = (SqlStatement *)oracleConnection->CreateStatement();
	stmtText = "select count(*) "
		"from word_phrase "
		"where "
			"language_code = :lc";
	stmt->AddToCommandString(stmtText);
	stmt->Parse();
	stmt->BindInputIntToString(":lc","%2d",language_id,language_code);
	SqlColumn *cnt = stmt->BindOutputColumn(1, SqlColumn::Integer);
	stmt->Execute();
	stmt->Fetch();
	size = cnt->AsInteger() + 1;
	delete stmt;
//printf("GPhrase size from DB = %d\n", size);
//fflush(stdout);
}

bool
CacheGphraseData::populateFromDatabase() {
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
	stmtText = "select word, head_word, hash_location, company_code, "
		"word_id, hash_code_1, hash_code_2, root_hash_1, root_hash_2, "
		"root_hen_1, root_hen_2, hen_num_1, hen_num_2, "
		"word_type_code, word_count, ending_length "
		"from word_phrase "
		"where "
			"language_code = :lc "
//			"order by substr(word, 0, greatest(instr(word, ' ',1),4)), word_count desc";
			"order by word, word_count desc";
	stmt->AddToCommandString(stmtText);
	stmt->Parse();
	stmt->BindInputIntToString(":lc","%2d",language_id,language_code);
	SqlColumn *word = stmt->BindOutputColumn(1, SqlColumn::StringType);
	SqlColumn *head_word = stmt->BindOutputColumn(2, SqlColumn::Integer);
	SqlColumn *hloc = stmt->BindOutputColumn(3, SqlColumn::Integer);
	SqlColumn *cc = stmt->BindOutputColumn(4, SqlColumn::StringType);
	SqlColumn *word_id = stmt->BindOutputColumn(5, SqlColumn::Integer);
	SqlColumn *hash_code_1 = stmt->BindOutputColumn(6, SqlColumn::Integer);
	SqlColumn *hash_code_2 = stmt->BindOutputColumn(7, SqlColumn::Integer);
	SqlColumn *root_hash_1 = stmt->BindOutputColumn(8, SqlColumn::Integer);
	SqlColumn *root_hash_2 = stmt->BindOutputColumn(9, SqlColumn::Integer);
	SqlColumn *root_hen_1 = stmt->BindOutputColumn(10, SqlColumn::Integer);
	SqlColumn *root_hen_2 = stmt->BindOutputColumn(11, SqlColumn::Integer);
	SqlColumn *hen_num_1 = stmt->BindOutputColumn(12, SqlColumn::Integer);
	SqlColumn *hen_num_2 = stmt->BindOutputColumn(13, SqlColumn::Integer);
	SqlColumn *word_type_code = stmt->BindOutputColumn(14, 
		SqlColumn::Integer);
	SqlColumn *word_count = stmt->BindOutputColumn(15, SqlColumn::Integer);
	SqlColumn *ending_length = stmt->BindOutputColumn(16, SqlColumn::Integer);
	stmt->Execute();

	nrows = 0;

// first load the data and calculate weights
	CacheKey *key = (CacheKey *)keybuf;
	while(stmt->Fetch()) {
//		if(debug)
//		printf("row %d: %s, %d, %d, %s\n", nrows, 
//				word->AsCharArray(), head_word->AsInteger(),
//				hloc->AsInteger(), cc->AsCharArray());
		unsigned char *cp = (unsigned char *)word->AsCharArray();
		int len = strlen((char *)cp);
		if(len>=GPHRASE_KEYLEN) len = GPHRASE_KEYLEN-1;

		struct b_row *brp = getRow(nrows);
		brp->head_word = head_word->AsInteger();
		brp->hash_location = hloc->AsInteger();
		memcpy(brp->company_code, cc->AsCharArray(), 3);
		memcpy(brp->word, cp, len+1);
		brp->word_id = word_id->AsInteger();
		brp->hash_code_1 = hash_code_1->AsInteger();
		brp->hash_code_2 = hash_code_2->AsInteger();
		brp->root_hash_1 = root_hash_1->AsInteger();
		brp->root_hash_2 = root_hash_2->AsInteger();
		brp->root_hen_1 = root_hen_1->AsInteger();
		brp->root_hen_2 = root_hen_2->AsInteger();
		brp->hen_num_1 = hen_num_1->AsInteger();
		brp->hen_num_2 = hen_num_2->AsInteger();
		brp->word_type_code = word_type_code->AsInteger();
		brp->word_count = word_count->AsInteger();
		brp->ending_length = ending_length->AsInteger();

// calculating how frequent each letter is.
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
		struct b_row *brp = getRow(j);
		makeKey(key, brp->word, j);
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
CacheGphraseData::makeKey(CacheKey *k, char *s, int val) {
	memset(k, 0, max_key_len);
	int len = strlen(s);
	memcpy((char *)k->getBuffer(), s, len);
	k->setLength(len);
	k->setValue(val);
	k->setNextIndex(-1);
}

