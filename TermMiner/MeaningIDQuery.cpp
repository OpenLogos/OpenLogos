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
// MeaningIDQuery.cpp: implementation of the MeaningIDQuery class.
//
//////////////////////////////////////////////////////////////////////

#include <logos_include/logoscommon.h>
#include <TermMiner/MeaningIDQuery.h>
#include <logos_libs/sql/sqlcolumn.h>
#include <logos_libs/sql/sqlexception.h>
#include <logos_libs/linguistic/llanguage.h>
#include <logos_libs/linguistic/sourcesentenceunit.h>
#include <logos_libs/SubjectMatterCodes/SubjectMatterCode.h>

// -------------------------------------------------------------------
// Constructor
// -------------------------------------------------------------------
MeaningIDQuery::MeaningIDQuery() 
{
}

// -------------------------------------------------------------------
// Destructor
// -------------------------------------------------------------------
MeaningIDQuery::~MeaningIDQuery() 
{
}

// -------------------------------------------------------------------
// Builds and parses the SQL statement
// (See comments in MeaningIDQuery::fetch() about generic_code and atomic_code)
// -------------------------------------------------------------------
void MeaningIDQuery::open(SqlConnection* databaseConnection) 
{
	LgsString query="select meaning_id"
				 ",      subject_matter_code"
				 " from meaning"
				 " where "
					"     company_code=:aCompanyCode"
					" and language_code=:aLanguageCode"
					" and usage_id=:aUsageID"
               " and primary_switch='Y'"
					;

	try {
		SqlQuery::Open(databaseConnection);
		Statement()->AddToCommandString(query);
		Statement()->Parse();
		meaningID_=Statement()->BindOutputColumn(1,SqlColumn::StringType);
		subjectMatterCode_=Statement()->BindOutputColumn(2,SqlColumn::StringType);
	}
	catch(SqlException& x) {
		cerr << x.Message() << endl;
		throw(x);
	}
}

// -------------------------------------------------------------------
// Closes the cursor and cleans up memory
// -------------------------------------------------------------------
void MeaningIDQuery::close() 
{
    SqlQuery::Close();
}

// -------------------------------------------------------------------
// Executes the actual query
// (See comments in MeaningIDQuery::fetch() about generic_code and atomic_code)
// -------------------------------------------------------------------
void MeaningIDQuery::execute(SourceSentenceUnit* ssu,			// info about the expanded form
							 const LLanguage& language,
							 int usageID)						// info about the canonical form
{
    try {
		Statement()->BindInputString(":aCompanyCode",(*ssu).companyCode().c_str());
		char x1[10];
		Statement()->BindInputIntToString(":aLanguageCode","%02d",x1,language.id());
		char x2[10];
        Statement()->BindInputIntToString(":aUsageID","%d",x2,usageID);

        Statement()->Execute();
	}
    catch(SqlException& x) {
        cerr << x.Message() << endl;
        throw(x);
    }
}

// -------------------------------------------------------------------
// Get query's result.
// Return whether the method found a value.
// Strategy: due to the way the database is set up, there was a performance issue in selecting
// meaning id based on generic_code and atomic_code. The preformance degrades when these 2 fields
// are added to the query. The approach is to get all records matching the query without the
// atomic_code and the generic_code, then to select in in this method the record that matches the
// given atomic_code and generic-code. Performance was better doing so.
// -------------------------------------------------------------------
bool MeaningIDQuery::fetch(SourceSentenceUnit* ssu,			// info about the expanded form
						   int& meaningID)					// the selected meaningID
{
	bool queryResult=true;
	bool foundValue=false;
	int candidateMeaningID;

	while (queryResult) {
		try {
			queryResult=Statement()->Fetch();
		}
		catch(SqlException& x) {
			throw(x);
		}
		if (queryResult) {
			// get next value selected from the query
			candidateMeaningID=meaningID_->AsIntegerFromString();
			SubjectMatterCode candidateSMC(subjectMatterCode_->AsString());
         LgsString smcValue(candidateSMC.content());
			// keep this value if it matches the request
			if (!foundValue && !strncmp(smcValue.c_str(), (*ssu).subjectMatterCode().content().c_str(), smcValue.length()))
			{
				meaningID=candidateMeaningID;		// selected meaningID
				foundValue=true;
			}
		}
	}

	return foundValue;
}
