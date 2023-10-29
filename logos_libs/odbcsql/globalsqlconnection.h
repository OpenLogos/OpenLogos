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
#ifndef _globalsqlconnection_h_
#define _globalsqlconnection_h_

//*******************************************************************
// Description: This module is used to create a single instance
//              of SqlConection to the database. First time,
//              getGlobalSqlConnection creates a connection to the 
//              database, return a pointer to the connection. Next
//              time it returns the pointer to the same connection.
//
//  Motivation: The legacy code (Fortran) does not have the concept
//              of database connection. They make several C function
//              calls to retrieve data from the database. The C
//              functions provide a similar interface as provided
//              by the legacy C functions. The new implemenation of
//              these C function is wrapper for C++ code, retrieving
//              data from the RDBMS. These C functions need a database
//              connection to retrieve data from the data. Instead of
//              using separate connection from every C module to the
//              database, use of this module facilitates a single
//              database connection from different modules.
// 
//      Author: Manoj Agarwala
//
//     History: 10/28/96 - Concieved and initial implementation
//*******************************************************************
#include <logos_include/lgsstring.h>

class SqlConnection;

SqlConnection* getSqlConnection();
void freeSqlConnection(SqlConnection * sqlConnection);
SqlConnection * createConnection(const LgsString & connectString, 
								 const LgsString & username,
								 const LgsString & password);
// same as createConnection but without checking DatabaseMode flag
// Used by jni_stemgen dll
SqlConnection * createConnAlways(
					const LgsString & connectString,
					const LgsString & username,
					const LgsString & password);

#endif

