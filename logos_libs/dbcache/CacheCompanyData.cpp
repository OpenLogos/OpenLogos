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
#include <logos_libs/dbcache/CacheCompanyData.h>

#include <fcntl.h>
#include <sys/io.h>

static DWORD t0, t1, t2;
static double thash, tload;

CacheCompanyData::CacheCompanyData(SqlConnection *con, int langcode, bool loader, bool fromFile, bool saveIt) :
	CacheData(con, langcode, "Company", loader, fromFile, saveIt) {

	row_len = sizeof(struct cc_row);
	max_key_len = COMPANY_KEYLEN;

	mapAndLoad();
}

void
CacheCompanyData::getSizeFromDB() {
	stmt = (SqlStatement *)oracleConnection->CreateStatement();
	stmtText = "select count(*) "
		" from company";

	stmt->AddToCommandString(stmtText);
	stmt->Parse();

	SqlColumn *cnt = stmt->BindOutputColumn(1, SqlColumn::Integer);
	stmt->Execute();
	stmt->Fetch();
	size = cnt->AsInteger() + 1;
	delete stmt;
//printf("Company size from DB = %d\n", size);
//fflush(stdout);
}

bool
CacheCompanyData::populateFromDatabase() {
	int printstep = 8;

	printf("Loading from database (one # mark - %d records)\n",
		printstep);
	fflush(stdout);

	CLOCK_T t0 = GetTickCount();

	stmt = (SqlStatement *)oracleConnection->CreateStatement();
	stmtText = "select company_code, description, restrict_switch"
		" from company "
		" order by search_sequence";

	stmt->AddToCommandString(stmtText);
	stmt->Parse();

	SqlColumn *cc=stmt->BindOutputColumn(1, SqlColumn::StringType);
	SqlColumn *desc=stmt->BindOutputColumn(2, SqlColumn::StringType);
	SqlColumn *rs=stmt->BindOutputColumn(3,SqlColumn::StringType);

	stmt->Execute();

	nrows = 0;

	while(stmt->Fetch()) {
		struct cc_row *lrp = getRow(nrows);
		strncpy(lrp->company_code, cc->AsCharArray(), 3);
		strncpy(lrp->description, desc->AsCharArray(), 200);
		strncpy(lrp->restrict_switch, rs->AsCharArray(), 1);

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
CacheCompanyData::makeKey() { }

void
CacheCompanyData::mapAndLoadInternal() {
  mapAndLoadReduced();
}
