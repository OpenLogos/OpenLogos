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
// WordQuery.cpp: implementation of the WordQuery class.
//
//////////////////////////////////////////////////////////////////////

#include <logos_include/logoscommon.h>
#include <TermMiner/WordQuery.h>
#include <logos_libs/sql/sqlcolumn.h>
#include <logos_libs/sql/sqlexception.h>

// -------------------------------------------------------------------
// Constructor
// -------------------------------------------------------------------
WordQuery::WordQuery() 
{
}

// -------------------------------------------------------------------
// Destructor
// -------------------------------------------------------------------
WordQuery::~WordQuery() 
{
}

// -------------------------------------------------------------------
// Builds and parses the SQL statement
// -------------------------------------------------------------------
void WordQuery::open(SqlConnection* databaseConnection) 
{
	LgsString query=" select word "
				 " from word_phrase "
				 " where     word_id=:aWordID "
				 "       and company_code=:aCompanyCode "
				 ;

	try 
	{
		SqlQuery::Open(databaseConnection);
		Statement()->AddToCommandString(query);
		Statement()->Parse();
		word_=Statement()->BindOutputColumn(1,SqlColumn::StringType);
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
void WordQuery::close() 
{
    SqlQuery::Close();
}

// -------------------------------------------------------------------
// Executes the actual query
// -------------------------------------------------------------------
void WordQuery::execute(int wordID, LgsString companyCode) 
{
	char x[10];
    try 
	 {
        Statement()->BindInputIntToString(":aWordID","%d",x,wordID);
		Statement()->BindInputString(":aCompanyCode",companyCode.c_str());
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
bool WordQuery::fetch(LgsString& word) 
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
		word=word_->AsString();
	}

	return queryResult;
}
