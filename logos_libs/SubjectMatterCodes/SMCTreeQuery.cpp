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
// --------------------------------------------------------------------------
// File: SMCTreeQuery.cpp (implementation)
// --------------------------------------------------------------------------

#include <logos_include/logoscommon.h>
#include <logos_libs/SubjectMatterCodes/SMCTreeQuery.h>
#include <logos_libs/sql/sqlcolumn.h>
#include <logos_libs/sql/sqlexception.h>

#include <logos_libs/dbcache/CacheSMCData.h>
#include <logos_libs/dbcache/CacheSMCQuery.h>

// --------------------------------------------------------------------------
// Default constructor
// --------------------------------------------------------------------------
SMCTreeQuery::SMCTreeQuery()
{
   cache_data = NULL;
   cache_query = NULL;
}

// --------------------------------------------------------------------------
// Default destructor
// --------------------------------------------------------------------------
SMCTreeQuery::~SMCTreeQuery()
{
   if (cache_data)
   {
      delete cache_data;
      cache_data = NULL;
   }
   if (cache_query)
   {
      delete cache_query;
      cache_query = NULL;
   }
}

// -------------------------------------------------------------------
// Builds and parses the SQL statement
// -------------------------------------------------------------------
void SMCTreeQuery::open(SqlConnection* databaseConnection) 
{
	cache_data = new CacheSMCData(NULL, 0, false, false, false);
	if (cache_data->isValid()) {
		cache_query = new CacheSMCQuery(cache_data, NULL);
		return;
	} else {
		delete cache_data;
		cache_data = NULL;
		cache_query = NULL;
	}

	LgsString query="select subject_matter_code, sequence "
				 "from subject_matter_tree "
				 "where tree_name=:aTreeName";

	try 
	{
		SqlQuery::Open(databaseConnection);
		Statement()->AddToCommandString(query);
		Statement()->Parse();
		subjectMatterCode_=Statement()->BindOutputColumn(1,SqlColumn::StringType);
		sequence_=Statement()->BindOutputColumn(2,SqlColumn::StringType);
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
void SMCTreeQuery::close() 
{
	if (cache_data == NULL)
		SqlQuery::Close();
}

// -------------------------------------------------------------------
// Executes the actual query, ie, get all smcs and their order sequence number
// based on a selected tree name
// -------------------------------------------------------------------
void SMCTreeQuery::execute(LgsString selectedTreeName)
{
	if (cache_data) {
		cache_query->query((char *)selectedTreeName.c_str());
		return;
	}

	try 
	{
		Statement()->BindInputString(":aTreeName",selectedTreeName.c_str());
		Statement()->Execute();
	}
	catch(SqlException& x) 
	{
		cerr << x.Message() << endl;
		throw(x);
	}
}

// -------------------------------------------------------------------
// Get query's result. Each call to this method returns a pair <smc value,sequence>.
// Return whether the method found a pair.
// -------------------------------------------------------------------
bool SMCTreeQuery::fetch(LgsString& smcValue,
						 int& sequence)
{
	bool foundAnotherInstance=false;
	bool usingCache = false;

	try 
	{
		if (cache_data) {
			foundAnotherInstance = cache_query->fetch(cache_smc, &cache_seq);
			usingCache = true;
//printf("SMCQuery.fetch: ret=%d, seq=%d, smc=\"%s\"\n",
//	   foundAnotherInstance, cache_seq, cache_smc); 
//fflush(stdout);
		} else {
			foundAnotherInstance = Statement()->Fetch();
		}

		if (foundAnotherInstance) {
			if(usingCache) {
				smcValue = cache_smc;
				sequence = cache_seq;
			} else {
				smcValue=subjectMatterCode_->AsString();
				sequence=sequence_->AsIntegerFromString();
			}
		}
		else
		{
			// default (erroneous) values
			smcValue="";
			sequence=0;
		}
	}
	catch(SqlException& x) 
	{
		throw(x);
	}

	return foundAnotherInstance;
}
