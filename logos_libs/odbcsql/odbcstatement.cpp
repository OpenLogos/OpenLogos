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
// File - ODBCStatement.cpp
//
// Class - ODBCStatement (implementation)
//
//-------------------------------------------------------------------

#include <logos_include/logoscommon.h>
#include <logos_libs/odbcsql/odbcconnection.h>
#include <logos_libs/sql/sqlcolumn.h>
#include <logos_libs/sql/sqlexception.h>
#include <logos_libs/odbcsql/odbcstatement.h>
#include <logos_libs/multithreadlib/lgscritsect.h>
#include <logos_include/lgscontainers.h>

extern LgsCriticalSection dbAccessCS;

//----------------------------------------------------------------------
ODBCStatement::ODBCStatement(ODBCConnection* pConnection)
              :SqlStatement(pConnection)
{
   m_pConnection = pConnection; // stored also in the base class SqlStatement.

   dbAccessCS.enter();
   retcode = ::SQLAllocStmt(pConnection->GetHDBC(), &m_hstmt);
   dbAccessCS.leave();
   if (retcode)
   {
      throw(SqlException(ErrorMessage(makeErrMsg("SQLAllocStmt"))));
   }
   b_parseDeferred = true;
   b_FetchedAlready = false;
   b_ExecutedAlready = false;
}
//----------------------------------------------------------------------
ODBCStatement::~ODBCStatement()
{
   dbAccessCS.enter();
   retcode = ::SQLFreeStmt(m_hstmt, SQL_DROP);
   dbAccessCS.leave();

   if (retcode)
   {
      throw(SqlException(ErrorMessage(makeErrMsg("SQLFreeStmt"))));
   }
}
//----------------------------------------------------------------------
void ODBCStatement::Parse()
{
   ReplacePatterns();
   dbAccessCS.enter();
   retcode = ::SQLPrepare(m_hstmt, (unsigned char*)v_commandString.c_str(), v_commandString.length());
   dbAccessCS.leave();
   if (retcode)
   {
      throw(SqlException(ErrorMessage(makeErrMsg("SQLExecute"))));
   }
   b_parseDeferred = false;
}
//----------------------------------------------------------------------
void ODBCStatement::ParseDeferred()
{
   b_parseDeferred = true;
}
//----------------------------------------------------------------------
void ODBCStatement::BindInputPreliminary()
{
   if (b_FetchedAlready || b_ExecutedAlready)
   {
      dbAccessCS.enter();
      retcode = ::SQLFreeStmt(m_hstmt, SQL_CLOSE);
      if (retcode != SQL_SUCCESS && retcode != SQL_SUCCESS_WITH_INFO)
      {
         dbAccessCS.leave();
         throw(SqlException(ErrorMessage(makeErrMsg("SQLFreeStmt"))));
      }

      retcode = ::SQLFreeStmt(m_hstmt, SQL_RESET_PARAMS);
      dbAccessCS.leave();
      if (retcode != SQL_SUCCESS && retcode != SQL_SUCCESS_WITH_INFO)
      {
         throw(SqlException(ErrorMessage(makeErrMsg("SQLFreeStmt"))));
      }

      b_FetchedAlready = false;
      b_ExecutedAlready = false;
   }

   if (b_parseDeferred)
   {
      Parse();
      b_parseDeferred = false;
   }
}
//----------------------------------------------------------------------
void ODBCStatement::BindInputInteger(const char* aPattern, const int* aFiller)
{
   //fprintf(stderr, "BindInt(%s, %d)", aPattern, *aFiller); fflush(stderr);

   BindInputPreliminary();
   dbAccessCS.enter();
   retcode = ::SQLBindParameter(m_hstmt,
                                LookupPattern(aPattern),
                                SQL_PARAM_INPUT,
                                SQL_C_LONG,
                                SQL_FLOAT,
                                0, // Precision of the column - cbColDef
                                0, // ibScale
                                (int *)aFiller, // Pointer to buffer - rgbValue
                                0, // Max Length of rgbVlaue
                                0); // pcbValue - Pointer to buffer for parameter's length
   dbAccessCS.leave();

   if (!CheckSuccess(retcode))
   {
      throw(SqlException(ErrorMessage(makeErrMsg("bind"))));
   }
}
//----------------------------------------------------------------------
void ODBCStatement::BindInputString(const char* aPattern, const char* aFiller)
{
   //fprintf(stderr,"BindString(%s, %s)\n", aPattern, aFiller); fflush(stderr);

   BindInputPreliminary();
   cbValue = SQL_NTS;
   int len = strlen(aFiller);
   dbAccessCS.enter();
   retcode = ::SQLBindParameter(m_hstmt,
                                LookupPattern(aPattern),
                                SQL_PARAM_INPUT,
                                SQL_C_CHAR,
                                SQL_CHAR,
                                len,
                                0,
                                (char*)aFiller,
                                0,
                                &cbValue);
   dbAccessCS.leave();
   if (!CheckSuccess(retcode))
   {
      throw(SqlException(ErrorMessage(makeErrMsg("bind"))));
   }
}
//----------------------------------------------------------------------
void ODBCStatement::BindInputTimeStamp(const char* aPattern, const TimeStamp* p_TimeStamp)
{
   BindInputPreliminary();
   dbAccessCS.enter();
   retcode = ::SQLBindParameter(m_hstmt,
                                LookupPattern(aPattern),
                                SQL_PARAM_INPUT,
                                SQL_C_TIMESTAMP,
                                SQL_TIMESTAMP,
                                0, //Precision of the column - cbColDef
                                0, //ibScale
                                (TIMESTAMP_STRUCT *)p_TimeStamp, //Pointer to buffer - rgbValue
                                0, //Max Length of rgbVlaue
                                0); //pcbValue - Pointer to buffer for parameter's length
   dbAccessCS.leave();

   if (!CheckSuccess(retcode))
   {
      throw(SqlException(ErrorMessage(makeErrMsg("bind"))));
   }
}
//----------------------------------------------------------------------
void ODBCStatement::BindInputTime(const char* aPattern, const Time* p_Time)
{
   BindInputPreliminary();
   dbAccessCS.enter();
   retcode = ::SQLBindParameter(m_hstmt,
                                LookupPattern(aPattern),
                                SQL_PARAM_INPUT,
                                SQL_C_TIME,
                                SQL_TIME,
                                0, //Precision of the column - cbColDef
                                0, //ibScale
                                (TIME_STRUCT *)p_Time, //Pointer to buffer - rgbValue
                                0, //Max Length of rgbVlaue
                                0); //pcbValue - Pointer to buffer for parameter's length
   dbAccessCS.leave();

   if (!CheckSuccess(retcode))
   {
      throw(SqlException(ErrorMessage(makeErrMsg("bind"))));
   }
}
//----------------------------------------------------------------------
void ODBCStatement::BindInputDate(const char* aPattern, const Date* p_Date)
{
   BindInputPreliminary();
   dbAccessCS.enter();
   retcode = ::SQLBindParameter(m_hstmt,
                                LookupPattern(aPattern),
                                SQL_PARAM_INPUT,
                                SQL_C_DATE,
                                SQL_DATE,
                                0, //Precision of the column - cbColDef
                                0, //ibScale
                                (DATE_STRUCT *)p_Date, //Pointer to buffer - rgbValue
                                0, //Max Length of rgbVlaue
                                0); //pcbValue - Pointer to buffer for parameter's length
   dbAccessCS.leave();

   if (!CheckSuccess(retcode))
   {
      throw(SqlException(ErrorMessage(makeErrMsg("bind"))));
   }
}
//----------------------------------------------------------------------
SqlColumn* ODBCStatement::BindOutputColumn(int columnNumber, SqlColumn::Type type)
{
   SDWORD columnSize;
   switch(type)
   {
   case SqlColumn::Boolean:
      columnSize = 1;
      break;
   case SqlColumn::Integer:
      columnSize = 22;
      break;
   case SqlColumn::StringType:
      columnSize = 400;
      break;
   case SqlColumn::Long_Row:
      columnSize = sizeof(longRaw);
      break; // Added Manoj
   case SqlColumn::Raw:
      columnSize = 400;
      break; // Added Manoj
   case SqlColumn::TimeStampType:
      columnSize = sizeof(TIMESTAMP_STRUCT);
      break;
   case SqlColumn::TimeType:
      columnSize = sizeof(TIME_STRUCT);
      break;
   case SqlColumn::DateType:
      columnSize = sizeof(DATE_STRUCT);
      break;
   default:
      columnSize = 400;
      break;
   }
   ODBCColumn* pColumn = new ODBCColumn(type, columnSize);
   SqlStatement::AddColumn(pColumn);

   dbAccessCS.enter();
   retcode = ::SQLBindCol(m_hstmt, columnNumber, pColumn->BufferType(), pColumn->Buffer(),
                          pColumn->BufferLength(), pColumn->ReturnLength());
   dbAccessCS.leave();
   if (!CheckSuccess(retcode))
   {
      throw(SqlException( ErrorMessage(makeErrMsg("define"))));
   }
   return pColumn;
}
//----------------------------------------------------------------------
void ODBCStatement::Execute()
{
   if (b_parseDeferred)
   {
      Parse();
      b_parseDeferred = false;
   }

   //fprintf(stderr, "SQLExec:%s", v_commandString.c_str()); fflush(stderr);

   if (b_FetchedAlready || b_ExecutedAlready)
   {
      dbAccessCS.enter();
      retcode = ::SQLFreeStmt(m_hstmt, SQL_CLOSE);
      dbAccessCS.leave();

      if (retcode)
      {
         throw(SqlException(ErrorMessage(makeErrMsg("SQLFreeStmt"))));
      }

      b_FetchedAlready = false;
   }
   dbAccessCS.enter();
   retcode = ::SQLExecute(m_hstmt);
   dbAccessCS.leave();

   b_ExecutedAlready = true;

   if (!CheckSuccess(retcode))
   {
      throw(SqlException(ErrorMessage(makeErrMsg("execute"))));
   }
}
//----------------------------------------------------------------------
bool ODBCStatement::Fetch()
{
   dbAccessCS.enter();
   retcode = ::SQLFetch(m_hstmt);
   dbAccessCS.leave();
   b_FetchedAlready = true;

   if (retcode == SQL_NO_DATA_FOUND) //sort of EOF
      return false;

   if (!CheckSuccess(retcode))
   {
      throw(SqlException(ErrorMessage(makeErrMsg("fetch"))));
   }

   return true;
}
//----------------------------------------------------------------------
void ODBCStatement::BindInputBlob(const char* pattern, const void* data, int length)
{
   //Added By Manoj Agarwala
   if (b_FetchedAlready || b_ExecutedAlready)
   {
      blobLengths.erase(blobLengths.begin(), blobLengths.end());
      dbAccessCS.enter();
      retcode = ::SQLFreeStmt(m_hstmt, SQL_CLOSE);
      if (retcode)
      {
         dbAccessCS.leave();
         throw(SqlException(ErrorMessage(makeErrMsg("SQLFreeStmt"))));
      }

      retcode = ::SQLFreeStmt(m_hstmt, SQL_RESET_PARAMS);
      dbAccessCS.leave();
      if (retcode)
      {
         throw(SqlException(ErrorMessage(makeErrMsg("SQLFreeStmt"))));
      }

      b_FetchedAlready = false;
      b_ExecutedAlready = false;
   }

   if (b_parseDeferred)
   {
      Parse();
      b_parseDeferred = false;
   }

   blobLengths.push_back(length); //Add to the end of the vector

   dbAccessCS.enter();
   retcode = ::SQLBindParameter(m_hstmt,
                                LookupPattern(pattern),
                                SQL_PARAM_INPUT,
                                SQL_C_BINARY, // New tried by Manoj Agarwala
                                SQL_LONGVARBINARY,
                                length,
                                0,
                                (char*)data,
                                length, // Changed from 0 to length by Manoj
                                & blobLengths.back()); // Pointer to last element in the vector
                                                       // This last element contains the length
                                                       // of the blob when SQLExecute is called
   dbAccessCS.leave();
   if (!CheckSuccess(retcode))
   {
      throw(SqlException(ErrorMessage(makeErrMsg("bind"))));
   }
}
//----------------------------------------------------------------------
int ODBCStatement::BindOutputRow(void)
{
   ODBCColumn *apCol = 0;
   if(getSqlConnection()==NULL)
		return -1;	
   
   int i = 0;
   for (i = 0; apCol = DefineColumn(i+1); i++)
      AddColumn(apCol);
   return i; // how many columns were added?
}
//----------------------------------------------------------------------
LgsString ODBCStatement::makeErrMsg(const LgsString& usrMsg)
{
   UCHAR szErrMsg[200];
   UCHAR szSqlState[10];
   SDWORD nativeError;
   SWORD cbErrMsg;
   SWORD cbErrorMsgMax = sizeof(szErrMsg) - 1;
   LgsString retString(usrMsg);
   ODBCConnection* pconnection = GetConnection();

   dbAccessCS.enter();
   retcode = ::SQLError(pconnection->GetHENV(), pconnection->GetHDBC(), m_hstmt, szSqlState,
                        &nativeError, szErrMsg, cbErrorMsgMax, &cbErrMsg);
   dbAccessCS.leave();

   if (!CheckSuccess(retcode))
   {
      throw (SqlException(ErrorMessage("SQLError")));
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
   retString.append(LgsString(")"));
   retString.append(LgsString((char*)&szErrMsg));
   retString.append(LgsString("\nCommand: "));
   retString.append(v_commandString);
   return retString;
}
//----------------------------------------------------------------------
void ODBCStatement::ReplacePatterns()
{
   char* cmdStr = new char[v_commandString.length() + 1];
   strcpy(cmdStr,v_commandString.c_str());
   cmdStr[v_commandString.length()] = 0;
   int beginPos;
   int len = strlen(cmdStr);

   while(true)
   {
     // look for first ':"
      for (beginPos = 0; beginPos < len; beginPos++)
         if (cmdStr[beginPos] == ':')
            break;
      if (beginPos == len) // NO MATCH
         break;
      // copy rest of cmdStr including ':' to subString
      char* subString = new char[len - beginPos + 1];
      strncpy(subString, &cmdStr[beginPos], len - beginPos);
      subString[len - beginPos] = 0;

      // look for first of "Space ( ) , = < >"
      int blankPos;
      for (blankPos = 0; blankPos < strlen(subString); blankPos++)
      if (subString[blankPos] == ' ' || subString[blankPos] == ')' ||
          subString[blankPos] == '(' || subString[blankPos] == ',' ||
          subString[blankPos] == '=' || subString[blankPos] == '<' ||
          subString[blankPos] == '>')
         break;

      LgsString ls;
      if (blankPos == strlen(subString)) // end of LgsString
         ls = subString;
      else
      {
         char* subs = new char[blankPos + 1];
         strncpy(subs, subString,blankPos);
         subs[blankPos] = 0;
         ls = subs;
         delete[] subs;
      }
      delete[] subString;

      v_params.push_back(ls);

      cmdStr[beginPos] = '?';
      strcpy(&cmdStr[beginPos + 1], &cmdStr[beginPos + blankPos]);
   }
   v_commandString = cmdStr;
   delete[] cmdStr;
}
//----------------------------------------------------------------------
/* No Good for STL, Rogue Wave and so on and so forth....
void ODBCStatement::ReplacePatterns()
{
   while (true)
   {
      size_t beginPos = v_commandString.first(':');
      if (beginPos == RW_NPOS) // NO MATCH
         break;
      size_t len = v_commandString.length();
      LgsString subString = v_commandString(beginPos, len - beginPos);
      size_t blankPos = subString.first(' ');

      LgsString ls;
      if (blankPos == RW_NPOS) // end of LgsString
         ls = subString;
      else
         ls = v_commandString(beginPos,blankPos); // we don't need the blank

      v_commandString(beginPos,blankPos) = "?";
      v_params.insert(ls);
   }
}
*/
//----------------------------------------------------------------------
int ODBCStatement::LookupPattern(const char* pattern)
{
   int pos = 0;
   for (LgsVector(LgsString)::iterator i = v_params.begin(); i != v_params.end(); i++)
   {
      pos++;
      if (pattern == *i)
         return pos;
   }

   LgsString msg = "LookupPattern failed, unabel to find the pattern '";
   msg.append(LgsString(pattern));
   msg.append(LgsString("'"));

   throw(SqlException(ErrorMessage(msg)));
   return 0;
}

int ODBCStatement::getNumberOfProcessedRows() {
  int result = 0;
  dbAccessCS.enter();
  retcode = ::SQLRowCount(m_hstmt, (SQLINTEGER *)&result);
  dbAccessCS.leave();
  if (!CheckSuccess(retcode))
    {
      throw(SqlException(ErrorMessage(makeErrMsg("ProcessedRows"))));
   }
  return result;
}
//----------------------------------------------------------------------
ODBCColumn* ODBCStatement::DefineColumn(int iCol)
{
#define MAX_COL_NAME_LENGTH  100
   char colName[MAX_COL_NAME_LENGTH + 1];
   SWORD cbColName;
   SWORD sqlType;
   UDWORD cbColDef;
   SWORD bScale;
   SWORD Nullable;

   dbAccessCS.enter();
   retcode = ::SQLDescribeCol(m_hstmt, iCol, (UCHAR*)colName, sizeof(colName), &cbColName,
                              &sqlType, &cbColDef, &bScale, &Nullable);
   dbAccessCS.leave();
   SqlColumn::Type colType;
   switch(sqlType)
   {
   case SQL_CHAR:
      colType = SqlColumn::StringType;
      break;
   case SQL_LONGVARCHAR:
      colType = SqlColumn::Long_Row;
      break;
   case SQL_LONGVARBINARY:  // Added By Manoj Agarwala
      colType = SqlColumn::Long_Row;
      break;
   case SQL_INTEGER:
      colType = SqlColumn::Integer;
      break;
   default:
      throw(SqlException(ErrorMessage("SQLDescribeCol: col type not supported by SqlColumn")));
   }
   ODBCColumn* odbc_col = new ODBCColumn(colType,(int)cbColDef);

   odbc_col->Name() = colName;
   return odbc_col;
}
//----------------------------------------------------------------------
inline bool ODBCStatement::CheckSuccess(RETCODE rc)
{
   switch(rc)
   {
   case SQL_SUCCESS:
      return true; //return Added By Manoj Agarwala
      break; //break added By Manoj Agarwala
   case SQL_SUCCESS_WITH_INFO:
      return true;
      break; //break added By Manoj Agarwala

   default:
      return false;
      break; //break added By Manoj Agarwala
   }
   return false;
}
//----------------------------------------------------------------------
// This was added for sequence generator by Manoj Agarwala
void ODBCStatement::BindOutputInteger(const char* aPattern, int* aFiller)
{
   if (b_FetchedAlready || b_ExecutedAlready)
   {
      dbAccessCS.enter();
      retcode = ::SQLFreeStmt(m_hstmt, SQL_CLOSE);
      if (retcode)
      {
         dbAccessCS.leave();
         throw(SqlException(ErrorMessage(makeErrMsg("SQLFreeStmt"))));
      }

      retcode = ::SQLFreeStmt(m_hstmt, SQL_RESET_PARAMS);
      dbAccessCS.leave();
      if (retcode)
      {
         throw(SqlException(ErrorMessage(makeErrMsg("SQLFreeStmt"))));
      }

      b_FetchedAlready = false;
      b_ExecutedAlready = false;
   }

   if (b_parseDeferred)
   {
      Parse();
      b_parseDeferred = false;
   }

   dbAccessCS.enter();
   retcode = ::SQLBindParameter(m_hstmt,
                                LookupPattern(aPattern),
                                SQL_PARAM_OUTPUT,
                                SQL_C_LONG,
                                SQL_FLOAT,
                                0, //Precision of the column - cbColDef
                                0, //ibScale
                                (int *)aFiller, //Pointer to buffer - rgbValue
                                0, //Max Length of rgbVlaue
                                0); //pcbValue - Pointer to buffer for parameter's length
   dbAccessCS.leave();

   if (!CheckSuccess(retcode))
   {
      throw(SqlException(ErrorMessage(makeErrMsg("bind"))));
   }
}

void ODBCStatement::AddToCommandString( const LgsString& s ) {

	if(getSqlConnection()==NULL)
		return;

    v_commandString += s;
}
