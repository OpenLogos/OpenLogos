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
 *    DESCRIPTION:   Holds the pointer to an SqlConnection together
 *                   with all statements allocated on this connection.
 *
 *    AUTHOR:        Vlad Yakimetz
 *
 *    HISTORY:       06/02/98
 *
 *******************************************************************/

#ifndef _DBCONNECTION_H_
#define _DBCONNECTION_H_

#include "statement.h"

class SqlConnection;


class DBConnection
{
public:
   DBConnection(SqlConnection* conn_)
      : _conn(conn_)
   {}
   ~DBConnection();
   
   SqlConnection& conn() const {return *_conn;}
   void addToPool(int hstmt_) {if (hstmt_) _hstatPool.push_front(hstmt_);}
   void removeFromPool(int hstmt_) {if (hstmt_) _hstatPool.remove(hstmt_);}
   const list<int>& pool() const {return _hstatPool;}
   
private:
   DBConnection(const DBConnection& dbc_);
   
   SqlConnection* _conn;
   list<int> _hstatPool;      // statements allocated on this connection
   
};   


#endif //_DBCONNECTION_H_
