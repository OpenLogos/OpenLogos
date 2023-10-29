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
#include <logos_libs/dbcache/CacheDerivedFormData.h>

#include <fcntl.h>
#include <sys/io.h>

static DWORD t0, t1, t2;
static double thash, tload;

CacheDerivedFormData::CacheDerivedFormData(SqlConnection *con, int langcode, bool loader, bool fromFile, bool saveIt) :
	CacheData(con, langcode, "DerivedForm", loader, fromFile, saveIt) {

	row_len = sizeof(struct df_row);
	max_key_len = DERIVED_FORM_KEYLEN;

	mapAndLoad();
}

void
CacheDerivedFormData::getSizeFromDB() {
	stmt = (SqlStatement *)oracleConnection->CreateStatement();
	stmtText = "select count(*) "
		" from derived_form where language_code=:lc";

	stmt->AddToCommandString(stmtText);
	stmt->Parse();

	stmt->BindInputIntToString(":lc","%2d",language_id,language_code);

	SqlColumn *cnt = stmt->BindOutputColumn(1, SqlColumn::Integer);
	stmt->Execute();
	stmt->Fetch();
	size = cnt->AsInteger() + 1;
	delete stmt;
printf("DerivedForm (%d) size from DB = %d\n", language_code, size);
fflush(stdout);
}

bool
CacheDerivedFormData::populateFromDatabase() {
	int printstep = 8;

	printf("Loading from database (one # mark - %d records)\n",
		printstep);
	fflush(stdout);

	CLOCK_T t0 = GetTickCount();

	stmt = (SqlStatement *)oracleConnection->CreateStatement();
	stmtText = "select derived_form_id, pat_number, stem_number, "
		" ending, word_class_code, form_code "
		" from derived_form where language_code=:lc";

	stmt->AddToCommandString(stmtText);
	stmt->Parse();

	stmt->BindInputIntToString(":lc","%2d",language_id,language_code);

	SqlColumn *dfi = stmt->BindOutputColumn(1, SqlColumn::StringType);
	SqlColumn *pn = stmt->BindOutputColumn(2, SqlColumn::Integer);
	SqlColumn *sn = stmt->BindOutputColumn(3, SqlColumn::Integer);
	SqlColumn *en = stmt->BindOutputColumn(4, SqlColumn::StringType);
	SqlColumn *wcc = stmt->BindOutputColumn(5, SqlColumn::Integer);
	SqlColumn *fc = stmt->BindOutputColumn(6, SqlColumn::Integer);

	stmt->Execute();

	nrows = 0;

	while(stmt->Fetch()) {
		struct df_row *lrp = getRow(nrows);
		strncpy(lrp->derived_form_id, dfi->AsCharArray(), 60);
		strncpy(lrp->ending, en->AsCharArray(), 32);
		lrp->word_class_code = wcc->AsInteger();
		lrp->form_code = fc->AsInteger();
		lrp->pat_number = pn->AsInteger();
		lrp->stem_number = sn->AsInteger();

		nrows++;

		if(nrows%printstep==0) {
			printf("#");
			fflush(stdout);
		}
	}

	delete stmt;

	CLOCK_T t1 = GetTickCount();
	tload = (t1-t0)/1000.;
	printf("\nDone loading, %d records, load time %4.1f sec\n", nrows, tload);

	fflush(stdout);

	return true;
}

void
CacheDerivedFormData::makeKey() { }

void CacheDerivedFormData::mapAndLoadInternal() {
  mapAndLoadReduced();
}
