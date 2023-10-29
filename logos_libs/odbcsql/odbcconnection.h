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
#ifndef __ODBCConnection_h__
#define __ODBCConnection_h__

//----------------------------------------------------------------------
// File - ODBCConnection.h
//
// Class - ODBCConnection
//
//----------------------------------------------------------------------

#include <logos_libs/odbcsql/odbcstuff.h>
#include <logos_libs/sql/sqlconnection.h>

class ODBCConnection: public SqlConnection
{
public:
   SQLHENV GetHENV();
   SQLHDBC GetHDBC();
   virtual SqlStatement* CreateStatement();
   virtual unsigned long getTotalDbTime();

   ODBCConnection();
   virtual ~ODBCConnection();

   virtual void Open(const LgsString& connectString, const LgsString& name, const LgsString& password);
   virtual void Close();

   unsigned char* Host() const;
   LgsString* Logon() const;
   short int ResultCode() const;
   virtual LgsString ErrorMessage(const LgsString& message) const;

private:
   LgsString makeErrMsg(const LgsString& usrMsg);

   RETCODE retcode;
   LgsString p_host;
   LgsString p_name;
   bool m_connectionIsOpen;
   // SQLHENV and SQLHDBC used instead of HENV, HDBC.
   // This is the way the functions are prototyped in MSDEV\INCLUDE\SQL.H
   static SQLHENV m_henv;  // ODBC environment handle
   static short m_counter;
   SQLHDBC m_hdbc;         // ODBC connection handle
};

//----------------------------------------------------------------------
inline unsigned char* ODBCConnection::Host() const
{
   return NULL;
}
//----------------------------------------------------------------------
inline LgsString* ODBCConnection::Logon() const
{
      return NULL;//p_logon;
}
//----------------------------------------------------------------------
inline short int ODBCConnection::ResultCode() const
{
   return 0; //Logon()->rc;
}

#endif // __ODBCConnection_h__

