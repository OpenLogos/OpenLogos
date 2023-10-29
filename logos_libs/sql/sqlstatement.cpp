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
//-------------------------------------------------------------------
// File - SqlStatement.cpp
//
// Class - SqlStatement (implementation)
//
//-------------------------------------------------------------------

#include <logos_include/logoscommon.h>
#include <stdio.h>
#include <logos_libs/sql/sqlstatement.h>
#include <logos_libs/sql/sqlconnection.h>

//----------------------------------------------------------------------
SqlStatement::SqlStatement(SqlConnection* pConnection)
             :p_connection(pConnection)
{
    p_outputColumns = new SqlColumnTable;
}
//----------------------------------------------------------------------
SqlStatement::~SqlStatement()
{
    assert( p_outputColumns );

    //p_outputColumns->clearAndDestroy();
    for (SqlColumnTable::iterator i = p_outputColumns->begin(); i != p_outputColumns->end(); i++)
    {
       delete *i;
    }
    p_outputColumns->erase(p_outputColumns->begin(), p_outputColumns->end());
    delete p_outputColumns;
}
//----------------------------------------------------------------------
void SqlStatement::AddColumn(SqlColumn* p)
{
   //p_outputColumns->append(p);
   p_outputColumns->push_back(p);
}
//----------------------------------------------------------------------
LgsString SqlStatement::ErrorMessage(const LgsString& s) const
{
    return p_connection->ErrorMessage(s);
}
//----------------------------------------------------------------------
SqlColumn* SqlStatement::GetOutputColumn(int index)
{
   if (index >= OutputColumnCount())
   {
      return 0;
   }
   //return p_outputColumns->at(index);
   return (*p_outputColumns)[index];
}
//----------------------------------------------------------------------
int SqlStatement::OutputColumnCount() const
{
   //return p_outputColumns->entries();
   return p_outputColumns->size();
}
//----------------------------------------------------------------------
void SqlStatement::BindInputIntToString(const char* position, const char* format,
                                        char* buffer, int value)
{
   sprintf(buffer, format, value);
   BindInputString(position, buffer);
}

int SqlStatement::getNumberOfFetchedRows() {
	return 0;
}
int SqlStatement::getNumberOfProcessedRows() {
	return 0;
}

SqlConnection *SqlStatement::getSqlConnection()
{
   return p_connection;
}
