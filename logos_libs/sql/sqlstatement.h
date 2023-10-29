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
#ifndef __SqlStatement_h__
#define __SqlStatement_h__

//----------------------------------------------------------------------
// File - SqlStatement.h
//
// Class - SqlColumnTable
// Class - SqlStatement (Abstract)
//
// Description - An object of this class is a wrapper for a single
//      SQL expression. The expression can be parameterized by binding
//      to binding different input strings (see BindInputString()).
//
//----------------------------------------------------------------------
#include <logos_include/lgscontainers.h>
#include <logos_libs/sql/sqlcolumn.h>

class SqlConnection;

//----------------------------------------------------------------------
class SqlColumnTable: public LgsVector(SqlColumn*) {};

//----------------------------------------------------------------------
class SqlStatement
{
public:
   //------------------------------------------------------------------
   // Constructor takes a SqlConnection object. Each statement must
   //      have a connection in order to execute.
   // The destructor is virtual because this class is intended to have
   //      subclasses.Although this class can be directly instantiated
   //      there are performance reasons for creating a subclass for
   //      this class for an individual SQL expression.
   //------------------------------------------------------------------
   virtual ~SqlStatement();

   //------------------------------------------------------------------
   // The following member functions wrap the OCI commands that are used
   //     to make most database queries. How to use these functions is
   //     everything that a programmer needs to know about OCI. Because
   //     of the importance in understanding these functions they are
   //     each described in detail below.
   //------------------------------------------------------------------

   SqlConnection * getSqlConnection();

   virtual void BindInputString(const char* position, const char* aValue) = 0;
   virtual void BindInputInteger(const char* position, const int* aValue) = 0;
   void BindInputIntToString(const char* position, const char* format,
                             char* buffer, int value);
   virtual void BindInputBlob(const char* position, const void* apValue, int aiLen) = 0;
   virtual void BindInputTimeStamp(const char* position, const TimeStamp*) = 0;
   virtual void BindInputDate(const char* aPattern, const Date* p_Date) = 0;
   virtual void BindInputTime(const char* aPattern, const Time* p_Time) = 0;

   virtual SqlColumn* BindOutputColumn(int columnNumber, SqlColumn::Type type) = 0;
   virtual int BindOutputRow() = 0;

   SqlColumn* GetOutputColumn (int index);
   int OutputColumnCount() const;

   virtual void Execute() = 0;
   virtual bool Fetch() = 0;

   virtual void Parse() = 0;
   virtual void ParseDeferred() = 0;
   virtual void AddToCommandString( const LgsString& s ) = 0;

   /* _fix_me_ a quick hoax to get this compiled (bk, Jul 19 2005) */

   DLLEXPORT virtual int getNumberOfFetchedRows();
   DLLEXPORT virtual int getNumberOfProcessedRows();

   #undef __declspec

   LgsString  ErrorMessage(const LgsString& message) const;

protected:
  //------------------------------------------------------------------
  // This is an abstract class -- the constructor does not need to be
  // public.
  //------------------------------------------------------------------

       SqlStatement( SqlConnection* );

  void AddColumn( SqlColumn* );
  //------------------------------------------------------------------
  // Private variables. The actual command LgsString.
  //------------------------------------------------------------------

    LgsString           v_commandString;

private:
        //------------------------------------------------------------------
        //------------------------------------------------------------------

    SqlColumnTable* p_outputColumns;
    SqlConnection*  p_connection;

    char            v_intToCharBuffer[20];
};

//----------------------------------------------------------------------
// AddToCommandString() - The catenates a LgsString of any length onto
//     the command that is to go against the Oracle engine. This
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
// BindOutputRow() - Creates a collection of SqlColumns that defines
//         each result column using default types.
// Execute() - Simply executes the query.
// Fetch() - retrieves one row of the executed query into the output
//     columns.
// Parse() - preformed before the binds and the execution. Gives the
//     text of the command to Oracle. Oracle is able to compile and
//     optimize the query at this time.
// ParseDeferred() - same as parse but doesn't cause the actual
//     parse to occur until execution time. This is a performance
//     winner where the statement is used sparingly (oracle is
//     called once rather than twice). This is a performance loser
//     where the statement is used repeatedly.
//----------------------------------------------------------------------

#endif // __SqlStatement_h__

