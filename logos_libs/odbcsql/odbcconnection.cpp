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
// File - ODBCConnection.cpp
//
// Class - ODBCConnection (implementation)
//
//-------------------------------------------------------------------

#include <logos_include/logoscommon.h>
#include <logos_libs/odbcsql/odbcconnection.h>
#include <logos_libs/odbcsql/odbcstatement.h>
#include <logos_libs/sql/sqlexception.h>
#include <stdio.h>
#include <logos_libs/multithreadlib/lgscritsect.h>

extern LgsCriticalSection dbAccessCS;
SQLHENV ODBCConnection::m_henv = SQL_NULL_HENV;
short ODBCConnection::m_counter = 0;

//----------------------------------------------------------------------
ODBCConnection::ODBCConnection()
               :m_connectionIsOpen(false)
{
   m_counter++;
}

//----------------------------------------------------------------------
ODBCConnection::~ODBCConnection()
{
   // If anything is open, close it
   Close();

   // Following Section of code should be Critical Section 
   // Decrementing Counter, Check for NULL and freeing env should not
   // be interrupted
   m_counter--;
   if (!m_counter && (m_henv != SQL_NULL_HENV))
   {
      dbAccessCS.enter();
      ::SQLFreeEnv(m_henv);
      dbAccessCS.leave();
      m_henv = SQL_NULL_HENV;
   }
}

//----------------------------------------------------------------------
void ODBCConnection::Open(const LgsString& host, const LgsString& name, const LgsString& password)
{
   // If anything is open, close it
   Close();

   if (m_henv == SQL_NULL_HENV) // only one instance per app.
   {
      if (::SQLAllocEnv(&m_henv) == SQL_ERROR)
         throw(SqlException(ErrorMessage(makeErrMsg("Memory Allocation"))));
   }

   m_hdbc = SQL_NULL_HDBC;

   dbAccessCS.enter();
   retcode = ::SQLAllocConnect(m_henv, &m_hdbc);
   if ((retcode != SQL_SUCCESS) && (retcode != SQL_SUCCESS_WITH_INFO))
   {
      dbAccessCS.leave();
      throw(SqlException(ErrorMessage(makeErrMsg("SQLAllocConnect"))));
   }

   p_host = host;
   p_name = name;
   retcode = ::SQLConnect(m_hdbc, (unsigned char*)host.data(), host.length(),
                          (unsigned char*)name.data(), name.length(),
                          (unsigned char*)password.data(), password.length());
   dbAccessCS.leave();
   if ((retcode != SQL_SUCCESS) && (retcode != SQL_SUCCESS_WITH_INFO))
   {
      LgsString msg = "SQLConnect failed.\nUnable to connect to the ODBC data source ";
      msg.append(host);
      msg.append(LgsString("\nusing the user id "));
      msg.append(name);
      throw(SqlException(ErrorMessage(msg)));
   }

   m_connectionIsOpen = true;
}

//----------------------------------------------------------------------
void ODBCConnection::Close()
{
   if (!m_connectionIsOpen)
      return;
   dbAccessCS.enter();
   retcode = ::SQLDisconnect(m_hdbc);
   dbAccessCS.leave();
   if ((retcode != SQL_SUCCESS) && (retcode != SQL_SUCCESS_WITH_INFO))
   {
      throw(SqlException( ErrorMessage(makeErrMsg("SQLDisconnect"))));
   }

   m_connectionIsOpen = false;

   dbAccessCS.enter();
   retcode = ::SQLFreeConnect(m_hdbc);
   dbAccessCS.leave();
   if ((retcode != SQL_SUCCESS) && (retcode != SQL_SUCCESS_WITH_INFO))
   {
      throw(SqlException(ErrorMessage(makeErrMsg("SQLFreeConnect"))));
   }
}

//----------------------------------------------------------------------
LgsString ODBCConnection::ErrorMessage(const LgsString& s) const
{
   LgsString errorMessage("ODBC(");
   errorMessage += s;
   if (0 != ResultCode())
   {
      char buffer[20];
      sprintf(buffer, "-%d", ResultCode());
      errorMessage.append(LgsString(buffer));
   }
   errorMessage.append(LgsString(")\n"));
   return errorMessage;
}

//----------------------------------------------------------------------
LgsString ODBCConnection::makeErrMsg(const LgsString& usrMsg)
{
   UCHAR szErrMsg[200];
   UCHAR szSqlState[10];
   SDWORD nativeError;
   SWORD cbErrMsg;
   SWORD cbErrorMsgMax = sizeof(szErrMsg) - 1;

   LgsString retString(usrMsg);

   dbAccessCS.enter();
   retcode = ::SQLError(SQL_NULL_HENV, SQL_NULL_HDBC, SQL_NULL_HSTMT, szSqlState,
                        &nativeError, szErrMsg, cbErrorMsgMax, &cbErrMsg);
   dbAccessCS.leave();

   if ((retcode != SQL_SUCCESS) && (retcode != SQL_SUCCESS_WITH_INFO))
   {
      throw(SqlException( ErrorMessage("SQLError")));
      return LgsString("");
   }
   retString.append(LgsString("-"));
   retString.append(LgsString((char*)&szSqlState));
   retString.append(LgsString("(native "));
   char szLong[20];
   //no ltoa in gcc
   sprintf(szLong,"%il",nativeError);
   retString.append(LgsString(szLong));
   //retString.append(LgsString(ltoa(nativeError, szLong,10)));
   retString.append(LgsString(") "));
   retString.append(LgsString((char*)&szErrMsg));
   return retString;
}

//----------------------------------------------------------------------
SqlStatement* ODBCConnection::CreateStatement()
{
   return new ODBCStatement(this);
}

//----------------------------------------------------------------------
unsigned long ODBCConnection::getTotalDbTime()
{
	return 0; //XXX not implemented
}

//----------------------------------------------------------------------
SQLHDBC ODBCConnection::GetHDBC()
{
   return m_hdbc;
}

//----------------------------------------------------------------------
SQLHDBC ODBCConnection::GetHENV()
{
   return m_henv;
}
