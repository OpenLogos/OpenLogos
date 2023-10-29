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
// TargetUsageIDQuery.h: interface for the TargetUsageIDQuery class.
//
//////////////////////////////////////////////////////////////////////

#ifndef _TargetUsageIDQuery_h_
#define _TargetUsageIDQuery_h_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include <logos_libs/sql/sqlquery.h>

class SqlColumn;
class LLanguage;

// --------------------------------------------------------------------------
// Definition of class TargetUsageIDQuery to query the database
// for TargetUsageID from the table transfer
// --------------------------------------------------------------------------
class TargetUsageIDQuery: public SqlQuery 
{
public:
	TargetUsageIDQuery();
	virtual ~TargetUsageIDQuery();
	virtual void open(SqlConnection*);					// builds and parses the SQL statement
	virtual void close();								// closes the cursor and cleans up memory
	void execute(LgsString,int,const LLanguage&);			// executes the query
	bool fetch(int&);									// get query's result (target usage id)

private:
	SqlColumn* targetUsageID_;
};

#endif // _TargetUsageIDQuery_h_
