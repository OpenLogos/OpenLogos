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
// TransferGenderQuery.cpp: implementation of the TransferGenderQuery class.
//
//////////////////////////////////////////////////////////////////////

#include <logos_include/logoscommon.h>
#include <TermMiner/TransferGenderQuery.h>
#include <logos_libs/sql/sqlcolumn.h>
#include <logos_libs/sql/sqlexception.h>
#include <logos_libs/linguistic/llanguage.h>
#include <logos_libs/linguistic/lsemantosyntacticunit.h>

// -------------------------------------------------------------------
// Constructor
// -------------------------------------------------------------------
TransferGenderQuery::TransferGenderQuery() 
{
}

// -------------------------------------------------------------------
// Destructor
// -------------------------------------------------------------------
TransferGenderQuery::~TransferGenderQuery() 
{
}

// -------------------------------------------------------------------
// Builds and parses the SQL statement
// -------------------------------------------------------------------
void TransferGenderQuery::open(SqlConnection* databaseConnection, const LLanguage& targetLanguage) 
{
	LgsString query = "select gender_code "
				      "from morphology tm, transfer t "
					  "where t.company_code = :aCompanyCode "
					  //" and Usage_ID = :aUsageID"
					  "and t.meaning_id = :aMeaningID "
					  "and t.target_language_code = :aTargetLanguage "
					  "and t.alternate_sequence IS NULL "
					  "and tm.company_code = t.company_code "
					  "and tm.language_code = t.target_language_code "
					  "and tm.usage_id = t.target_usage_id "
					  ;

	try 
	{
		SqlQuery::Open(databaseConnection);
		Statement()->AddToCommandString(query);
		Statement()->Parse();
		targetLanguageCode_ = targetLanguage.id();
		gender_ = Statement()->BindOutputColumn(1,SqlColumn::StringType);
	}
	catch(SqlException& x) 
	{
		cerr << x.Message() << endl;
		throw(x);
	}
}

// -------------------------------------------------------------------
// Closes the cursor and cleans up memory
// -------------------------------------------------------------------
void TransferGenderQuery::close() 
{
    SqlQuery::Close();
}

// -------------------------------------------------------------------
// Executes the actual query
// -------------------------------------------------------------------
void TransferGenderQuery::execute(const LSemantoSyntacticUnit& unit) 
{
	//char usageID[10];
	char inputMeaningID[10];
	char languageID[10];

	try 
	{
		Statement()->BindInputString(":aCompanyCode", unit.companyCode().c_str());
		//Statement()->BindInputIntToString(":aUsageID", "%d" , usageID, unit->usageID());
		Statement()->BindInputIntToString(":aMeaningID", "%d", inputMeaningID, unit.meaningID());
		Statement()->BindInputIntToString(":aTargetLanguage", "%02d", languageID, targetLanguageCode_);
		Statement()->Execute();
	}
    catch(SqlException& x) 
	 {
        cerr << x.Message() << endl;
        throw(x);
    }
}

// -------------------------------------------------------------------
// Get query's result
// -------------------------------------------------------------------
bool TransferGenderQuery::fetch(int& value) 
{
	bool queryResult=false;

	try 
	{
		queryResult=Statement()->Fetch();
	}
	catch(SqlException& x) 
	{
		throw(x);
	}
	if (queryResult) 
	{
		value = gender_->AsIntegerFromString();
	}

	return queryResult;
}
