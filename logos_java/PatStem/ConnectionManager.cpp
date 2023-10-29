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
/*******************************************************************
 *
 *    DESCRIPTION:   Manages connections and statements.
 *
 *    AUTHOR:        Vlad Yakimetz
 *
 *    HISTORY:       06/02/98
 *
 *******************************************************************/

#include <logos_include/logoscommon.h>
#include <logos_libs/odbcsql/globalsqlconnection.h>
#include <logos_libs/sql/sqlconnection.h>
#include "DBConnection.h"
#include "CompoundStatement.h" 
#include "GenericStatement.h" 
#include "ConnectionManager.h"


// One and the only instance of the PatStemGeneratorImpl class.
ConnectionManager& 
ConnectionManager
::theCM()
{
   static ConnectionManager cm;
   return cm;
}


int 
ConnectionManager
::openConnection(const LgsString& server_, 
                 const LgsString& user_, 
                 const LgsString& psw_)
{
   SqlConnection* conn = createConnAlways(server_, user_, psw_);
   if (!conn) return 0;
   return _connStore.create(conn);
}   

  
Statement&
ConnectionManager
::statement(int hstat_)
{
   return _statStore.fromHandle(hstat_).stmt();
}   


void 
ConnectionManager
::closeSession(int hstat_)
{
   try
   {
      GenericStatement& gns = _statStore.fromHandle(hstat_);
      DBConnection& dbc = _connStore.fromHandle(gns.hconn());
      
      dbc.removeFromPool(hstat_);
      _statStore.erase(hstat_);
   }
   catch(...)
   {
      return;
   }
}   


void 
ConnectionManager
::closeConnection(int hconn_)
{
   DBConnection& dbc = _connStore.fromHandle(hconn_);

   const list<int>& pool = dbc.pool();
   list<int>::const_iterator cit;
   
   for (cit = pool.begin(); cit != pool.end(); ++cit)
   {
      _statStore.erase(*cit);
   }
   
   dbc.conn().Close();
   _connStore.erase(hconn_);
}   
