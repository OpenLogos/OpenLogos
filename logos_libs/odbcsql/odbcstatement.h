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
#ifndef __ODBCStatement_h__
#define __ODBCStatement_h__

//----------------------------------------------------------------------
// File - ODBCStatement.h
//
// Class - ODBCStatement
//
// Description - An object of this class is a wrapper for a single
//      SQL expression. The expression can be parameterized by binding
//      to binding different input strings (see BindInputString()).
//
//----------------------------------------------------------------------

#include <logos_libs/odbcsql/odbcstuff.h>
#include <logos_libs/sql/sqlstatement.h>
#include <logos_libs/odbcsql/odbccolumn.h>

class ODBCConnection;

class ODBCStatement: public SqlStatement
{
   friend class ODBCConnection;

public:
   void ReplacePatterns();
   virtual int BindOutputRow(void);
   //------------------------------------------------------------------
   // The destructor is virtual because this class is intended to have
   // subclasses.Although this class can be directly instantiated
   // there are performance reasons for creating a subclass for
   // this class for an individual SQL expression.
   //------------------------------------------------------------------
   virtual ~ODBCStatement();

   //------------------------------------------------------------------
   // The following member functions wrap the ODBC commands that are used
   // to make most database queries. How to use these functions is
   // everything that a programmer needs to know about ODBC. Because
   // of the importance in understanding these functions they are
   // each described in detail below.
   //------------------------------------------------------------------
   virtual void AddToCommandString( const LgsString& s );
   virtual void BindInputString(const char* position, const char* aValue);
   virtual void BindInputInteger(const char* position, const int* aValue);
   virtual void BindInputTimeStamp(const char* position, const TimeStamp* p_TimeStamp);
   virtual void BindInputTime(const char* position, const Time* p_Time);
   virtual void BindInputDate(const char* position, const Date* p_Date);
   virtual void BindInputBlob(const char* pattern, const void* data, int length);
   void BindOutputInteger(const char* position, int* aValue); // Used By sequence generator - Manoj
   virtual SqlColumn* BindOutputColumn(int columnNumber, SqlColumn::Type type);
   virtual void Execute();
   virtual bool Fetch();
   virtual void Parse();
   virtual void ParseDeferred();
     // WK: used in logos_batch
   virtual int getNumberOfProcessedRows();
   LgsString makeErrMsg(const LgsString& usrMsg);

protected:
   //------------------------------------------------------------------
   // Constructor takes a SqlConnection object. Each statement must
   // have a connection in order to execute.
   //------------------------------------------------------------------
   ODBCStatement(ODBCConnection*);

private:
   void BindInputPreliminary();
   inline bool CheckSuccess(RETCODE rc);
   bool b_FetchedAlready;
   bool b_ExecutedAlready;
   ODBCColumn* DefineColumn(int iCol);
   ODBCConnection* GetConnection() { return m_pConnection; };
   ODBCConnection* m_pConnection;
   int LookupPattern(const char* pattern);
//   RWTValOrderedLgsVector(LgsString) v_params;
//   vector<LgsString, allocator<LgsString> > v_params;
   LgsVector(LgsString) v_params;
   LgsVector(long) blobLengths; // This is needed becuase blob is not null terminated and
                             // SQLBindParameter expects pointer to long for length of blob,
                             // pointer is used when SQLExecute is called
   RETCODE retcode;
   SDWORD cbValue;
   bool b_parseDeferred;
   SQLHSTMT m_hstmt;
};

//----------------------------------------------------------------------
// AddToCommandString() - The catenates a LgsString of any length onto
//     the command that is to go against the ODBC engine. This
//     only needs to be called once with the complete query or can
//     be called several times with pieces of the query be build up.
// BindInputString() - This allows a user to "parameterize" the
//     predicates of a query. The "select" statement contains
//     position holders that are prefixed by a colon (e.g. ":key").
//     The actual text of the executed query has these position
//     holders replaces by a value that the user binds to the
//     position holder.
// BindOutputColumn() - This function actually binds a SqlColumn
//     object with a column of the query. This function must be
//     called in the order that the columns exist in the query. For
//     example, the first time this function is called it binds a
//     SqlColumn object to the first column of the query, the second
//     time it is called it binds a SqlColumn object to the second
//     column of the query, etc.
// Execute() - Simply executes the query.
// Fetch() - retrieves one row of the executed query into the output
//     columns.
// Parse() - preformed before the binds and the execution. Gives the
//     text of the command to ODBC. ODBC is able to compile and
//     optimize the query at this time.
// ParseDeferred() - same as parse but doesn't cause the actual
//     parse to occur until execution time. This is a performance
//     winner where the statement is used sparingly (ODBC is
//     called once rather than twice). This is a performance loser
//     where the statement is used repeatedly.
//----------------------------------------------------------------------

#endif // __ODBCStatement_h__

