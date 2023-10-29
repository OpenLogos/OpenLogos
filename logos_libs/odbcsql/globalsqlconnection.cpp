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
#include <logos_libs/odbcsql/globalsqlconnection.h>
#include <lgs_db_io/dbinstances.h>
#include <logos_libs/odbcsql/odbcconnection.h>
#ifndef USE_ODBC
#include <logos_libs/orasql/oracleconnection.h>
#endif
#include <logos_libs/multithreadlib/lgscritsect.h>
#include <lgs_db_io/lgsdbcommonobjects.h>

GlobalDBInstances *globalObjects = 0;
static SqlConnection *globalConnection = 0;
extern LgsCriticalSection dbConnectInitCS;

SqlConnection* getSqlConnection()
{

//XXX API
	if(!JobControlArguments::isDatabaseMode()) {
//		printf("globalSqlConnection.getSqlConnection: mode API\n"); 
//		fflush(stdout);
		return NULL;
	}

//	printf("globalSqlConnection.getSqlConnection: mode DB\n"); fflush(stdout);

	dbConnectInitCS.enter();
	if (!globalConnection)
	{
#ifdef USE_ODBC
		globalConnection = new ODBCConnection();
		LgsString connectString = LgsDBCommonObjects::GetServerProperties().ODBCDataSource();
#else
		globalConnection = new OracleConnection();
		LgsString connectString = LgsDBCommonObjects::GetServerProperties().OracleDataSource();
#endif
		LgsString username = LgsDBCommonObjects::GetServerProperties().DatabaseUserID();
		LgsString password = LgsDBCommonObjects::GetServerProperties().DatabasePassword();

		globalConnection->Open(connectString, username, password);
	}
	globalConnection->incRefCount();
	dbConnectInitCS.leave();
	return globalConnection;
}

void freeSqlConnection(SqlConnection * sqlConnection)
{

	if(sqlConnection==NULL)
		return;

	dbConnectInitCS.enter();
	if (sqlConnection == globalConnection)
	{
		sqlConnection->decRefCount();
		if (!sqlConnection->refCount())
		{
			try
			{
				delete sqlConnection;
				sqlConnection = 0;
				globalConnection = 0;
			}
			catch(...)
			{
				;// DO NOTHING
			}
		}
	}
	dbConnectInitCS.leave();
}

SqlConnection * createConnection(const LgsString & connectString, 
                                 const LgsString & username,
                                 const LgsString & password)
{
//XXX API
	if(!JobControlArguments::isDatabaseMode()) {
//		printf("globalSqlConnection.createConnection: mode API\n"); 
//		fflush(stdout);
		return NULL;
	}

//	printf("GlobalSqlConnection.createConnection: mode DB\n"); fflush(stdout);

	SqlConnection* conn = 0;

	try
   {
#ifdef USE_ODBC
		conn = new ODBCConnection();
#else
		conn = new OracleConnection();
#endif
		conn->Open(connectString, username, password);
   }
   catch (...)
   {
      return 0;
   }
   
	return conn;
}

SqlConnection * createConnAlways(
					const LgsString & connectString, 
					const LgsString & username,
					const LgsString & password)
{
	SqlConnection* conn = 0;
	try
	{
#ifdef USE_ODBC
		conn = new ODBCConnection();
#else
		conn = new OracleConnection();
#endif
		conn->Open(connectString, username, password);
	}
	catch (...)
	{
		return 0;
	}

	return conn;
}
