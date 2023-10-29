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
#include <logos_libs/dbcache/CacheSMCData.h>
#include <logos_libs/dbcache/CacheKey.h>
#include <logos_libs/dbcache/CacheHashTab.h>

static DWORD t0, t1, t2;
static double thash, tload;

CacheSMCData::CacheSMCData(SqlConnection *con, int langcode, bool loader, bool fromFile,
			 bool saveIt) :
	CacheData(con, langcode, "SMC", loader, fromFile, saveIt) {

	row_len = sizeof(struct smc_row);
	max_key_len = SMC_KEYLEN;

	mapAndLoad();
}

void
CacheSMCData::getSizeFromDB() {
	stmt = (SqlStatement *)oracleConnection->CreateStatement();
	stmtText = "select count(*) "
		" from subject_matter_tree";

	stmt->AddToCommandString(stmtText);
	stmt->Parse();

	SqlColumn *cnt = stmt->BindOutputColumn(1, SqlColumn::Integer);
	stmt->Execute();
	stmt->Fetch();
	size = cnt->AsInteger() + 1;
	delete stmt;
//printf("SMC size from DB = %d\n", size);
//fflush(stdout);
}

bool
CacheSMCData::populateFromDatabase() {
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
	stmtText = "select subject_matter_code, sequence, tree_name "
		" from subject_matter_tree";

	stmt->AddToCommandString(stmtText);
	stmt->Parse();

	SqlColumn *smc = stmt->BindOutputColumn(1,SqlColumn::StringType);
	SqlColumn *seq = stmt->BindOutputColumn(2,SqlColumn::Integer);
	SqlColumn *tn = stmt->BindOutputColumn(3,SqlColumn::StringType);

	stmt->Execute();

	nrows = 0;

// first load the data and calculate weights
	CacheKey *key = (CacheKey *)keybuf;
	while(stmt->Fetch()) {
		struct smc_row *crp = getRow(nrows);
		crp->sequence = seq->AsInteger();
		strncpy(crp->smc, smc->AsCharArray(), 60);
		strncpy(crp->tree_name, tn->AsCharArray(), 60);

// calculating frequencies, key is (tree_name)
		unsigned char *cp = (unsigned char *) & crp->tree_name;
		int len = strlen(crp->tree_name);
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
//printf("row # %d, usage_id = %d\n", j, drp->usage_id);
//fflush(stdout);
		struct smc_row *drp = getRow(j);
		makeKey(key, drp->tree_name, j);
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
CacheSMCData::makeKey(CacheKey *key, char *tn, int val) {
	char *keybuf = (char *)key->getBuffer();
	memset(key, 0, max_key_len);
	char *cp = tn;
	memcpy(keybuf, cp, sizeof(int));
	int len = strlen(tn);
	key->setLength(len);
	key->setValue(val);
	key->setNextIndex(-1);
}
