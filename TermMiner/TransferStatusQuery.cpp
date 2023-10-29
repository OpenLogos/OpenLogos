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
// TransferStatusQuery.cpp: implementation of the TransferStatusQuery class.
//
//////////////////////////////////////////////////////////////////////

#include <logos_include/logoscommon.h>
#include <TermMiner/TransferStatusQuery.h>
#include <logos_libs/sql/sqlcolumn.h>
#include <logos_libs/sql/sqlexception.h>
#include <logos_libs/linguistic/llanguage.h>

// -------------------------------------------------------------------
// Constructor
// -------------------------------------------------------------------
TransferStatusQuery::TransferStatusQuery() 
{
}

// -------------------------------------------------------------------
// Destructor
// -------------------------------------------------------------------
TransferStatusQuery::~TransferStatusQuery() 
{
}

// -------------------------------------------------------------------
// Builds and parses the SQL statement
// -------------------------------------------------------------------
void TransferStatusQuery::open(SqlConnection* databaseConnection, const LLanguage& language) 
{
	LgsString query="select Status"
				 " from Transfer"
				 " where "
					"     Meaning_ID=:aMeaningID"
					" and Company_Code=:aCompanyCode"
					" and Target_Language_Code=:aLanguageCode"
					" and Deactivation_Switch='N' ";

    try 
	 {
		SqlQuery::Open(databaseConnection);
        Statement()->AddToCommandString(query);
        Statement()->Parse();
        languageCode_=language.id();
        transferStatus_=Statement()->BindOutputColumn(1,SqlColumn::StringType);
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
void TransferStatusQuery::close() 
{
    SqlQuery::Close();
}

// -------------------------------------------------------------------
// Executes the actual query
// -------------------------------------------------------------------
void TransferStatusQuery::execute(const LgsString& companyCode, int meaningID) 
{
	sprintf(companyCode_,"%s",companyCode.c_str());
    try 
	 {
		Statement()->BindInputIntToString(":aLanguageCode","%02d",inputLanguageCode_,languageCode_);
        Statement()->BindInputIntToString(":aMeaningID","%d",inputMeaningID_,meaningID);
		Statement()->BindInputString(":aCompanyCode",companyCode_);
        Statement()->Execute();
    }
    catch(SqlException& x) 
	 {
        cerr << x.Message() << endl;
        throw( x );
    }
}

// -------------------------------------------------------------------
// Capture transfer status from query
// -------------------------------------------------------------------
bool TransferStatusQuery::fetch(int& transferStatus) 
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
		transferStatus=transferStatus_->AsIntegerFromString();		// value from query
	 }
	else 
	{
		transferStatus=0;											// default value
	}

    return queryResult;
}
