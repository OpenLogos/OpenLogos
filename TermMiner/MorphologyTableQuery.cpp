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
program. If not, write to Globalware AG, Hospitalstraße 6, D-99817 Eisenach.

Linux port modifications and additions by Bernd Kiefer, Walter Kasper,
Deutsches Forschungszentrum fuer kuenstliche Intelligenz (DFKI)
Stuhlsatzenhausweg 3, D-66123 Saarbruecken
*/
// MorphologyTableQuery.cpp: implementation of the MorphologyTableQuery class.
//
//////////////////////////////////////////////////////////////////////

#include <logos_include/logoscommon.h>
#include <TermMiner/MorphologyTableQuery.h>
#include <logos_libs/sql/sqlcolumn.h>
#include <logos_libs/sql/sqlexception.h>
#include <logos_libs/linguistic/sourcesentenceunit.h>

// -------------------------------------------------------------------
// Constructor
// -------------------------------------------------------------------
MorphologyTableQuery::MorphologyTableQuery() 
{
}

// -------------------------------------------------------------------
// Destructor
// -------------------------------------------------------------------
MorphologyTableQuery::~MorphologyTableQuery() 
{
}

// -------------------------------------------------------------------
// Builds and parses the SQL statement
// -------------------------------------------------------------------
void MorphologyTableQuery::open(SqlConnection* databaseConnection) 
{
	LgsString query=" select inflected_form_switch, "
		         "        root_usage_id, "
				 "        numeric_constraint "
				 " from Morphology "
				 " where     Usage_ID=:aUsageID "
				 "       and Company_Code=:aCompanyCode "
				 ;

	try 
	{
		SqlQuery::Open(databaseConnection);
		Statement()->AddToCommandString(query);
		Statement()->Parse();
		inflectedFormSwitch_=Statement()->BindOutputColumn(1,SqlColumn::StringType);
		rootUsageID_=Statement()->BindOutputColumn(2,SqlColumn::StringType);
		numericConstraint_=Statement()->BindOutputColumn(3,SqlColumn::StringType);
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
void MorphologyTableQuery::close() 
{
    SqlQuery::Close();
}

// -------------------------------------------------------------------
// Executes the actual query
// -------------------------------------------------------------------
void MorphologyTableQuery::execute(SourceSentenceUnit* ssu) 
{
	char x[10];
    try 
	 {
        Statement()->BindInputIntToString(":aUsageID","%d",x,(*ssu).usageID());
		Statement()->BindInputString(":aCompanyCode",(*ssu).companyCode().c_str());
        Statement()->Execute();
    }
    catch(SqlException& x) 
	 {
        cerr << x.Message() << endl;
        throw( x );
    }
}

// -------------------------------------------------------------------
// Get query's result
// -------------------------------------------------------------------
bool MorphologyTableQuery::fetch(LgsString& inflectedFormSwitch, 
								 int& rootUsageID, 
								 int& numericConstraint)
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
		inflectedFormSwitch=inflectedFormSwitch_->AsString();
		rootUsageID=rootUsageID_->AsIntegerFromString();
		numericConstraint=numericConstraint_->AsIntegerFromString();
	}

	return queryResult;
}
