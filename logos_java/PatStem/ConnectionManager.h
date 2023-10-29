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

#ifndef _CONNECTIONMANAGER_H_
#define _CONNECTIONMANAGER_H_

#include "statement.h"
#include "ObjectStore.h"
#include "DBConnection.h"
#include "CompoundStatement.h"
#include "GenericStatement.h"
#include <logos_libs/sql/sqlconnection.h>
#include <logos_libs/sql/sqlstatement.h>


class ConnectionManager
{
public:
	static ConnectionManager& theCM();
   ~ConnectionManager() {}

   int openConnection(const LgsString& server_, 
                      const LgsString& user_, 
                      const LgsString& psw_);
   template <class T> int openSession(int hconn_, const T&)
   {
      int hstat = 0;
      try
      {
         DBConnection& dbc = _connStore.fromHandle(hconn_);
         if (hstat = _statStore.create(pair<int, Statement*> 
            (hconn_, new CompoundStatement<T>(dbc.conn().CreateStatement()))))
         {
            dbc.addToPool(hstat);
         }
      }
      catch(...)
      {
         return 0;
      }
      return hstat;
   }   
   Statement& statement(int hstat_);
   void closeSession(int hstat_);
   void closeConnection(int hconn_);                      
   
private:
   ConnectionManager() {}
   
   ObjectStore<int, DBConnection, SqlConnection*>             _connStore;
   ObjectStore<int, GenericStatement, pair<int, Statement*> > _statStore;

};   


#endif //_CONNECTIONMANAGER_H_
