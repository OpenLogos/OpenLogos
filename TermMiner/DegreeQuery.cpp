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
// DegreeQuery.cpp: implementation of the DegreeQuery class.
//
//////////////////////////////////////////////////////////////////////

#include <logos_include/logoscommon.h>
#include <TermMiner/DegreeQuery.h>
#include <logos_libs/sql/sqlcolumn.h>
#include <logos_libs/sql/sqlexception.h>
#include <logos_libs/linguistic/sourcesentenceunit.h>

// -------------------------------------------------------------------
// Constructor
// -------------------------------------------------------------------
DegreeQuery::DegreeQuery() 
{
}

// -------------------------------------------------------------------
// Destructor
// -------------------------------------------------------------------
DegreeQuery::~DegreeQuery() 
{
}

// -------------------------------------------------------------------
// Builds and parses the SQL statement
// -------------------------------------------------------------------
void DegreeQuery::open(SqlConnection* databaseConnection) 
{
	LgsString query="select Degree"
				 " from Word_Class_Subtype"
				 " where "
					"     Usage_ID=:aUsageID"
					" and Company_Code=:aCompanyCode"
					;

	try 
	{
		SqlQuery::Open(databaseConnection);
		Statement()->AddToCommandString(query);
		Statement()->Parse();
		degree_=Statement()->BindOutputColumn(1,SqlColumn::StringType);
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
void DegreeQuery::close() 
{
    SqlQuery::Close();
}

// -------------------------------------------------------------------
// Executes the actual query
// -------------------------------------------------------------------
void DegreeQuery::execute(SourceSentenceUnit* ssu) 
{
	char x[10];

	try 
	{
		Statement()->BindInputIntToString(":aUsageID","%d",x,ssu->usageID());
		Statement()->BindInputString(":aCompanyCode",ssu->companyCode().c_str());
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
bool DegreeQuery::fetch(int& value) 
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
		value=degree_->AsIntegerFromString();
	}

	return queryResult;
}
