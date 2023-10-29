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
#ifndef __ODBCColumn_h__
#define __ODBCColumn_h__

//----------------------------------------------------------------------
// File - ODBCColumn.h
//
// Class - ODBCColumn
//
//----------------------------------------------------------------------

#include <logos_libs/odbcsql/odbcstuff.h>
#include <logos_libs/sql/sqlcolumn.h>

class ODBCStatement;

class ODBCColumn: public SqlColumn
{
friend class ODBCStatement;
public:
//   long* GetpcbValue();
   virtual int GetSize(void);
   virtual Type GetType(void);
   virtual unsigned char* AsLongRaw(void);
   virtual unsigned char* AsRaw(void);

   //------------------------------------------------------------------
   // Destructor is virtual to allow inheritance of this class without
   // creating a danger of memory leaks.
   //------------------------------------------------------------------
   ODBCColumn(SqlColumn::Type aBufferType = StringType, int aBufferLength = 50);
   virtual ~ODBCColumn();

   virtual bool AsBoolean();
   virtual double AsDouble();
   virtual char* AsCharArray();
   virtual int AsInteger();
   virtual int AsIntegerFromString();
   virtual LgsString AsString();
   virtual TimeStamp AsTimeStamp();
   virtual Time AsTime();
   virtual Date AsDate();

   char* Buffer();
   SWORD BufferType();
   int BufferLength();
//   int* Indicator();
   SDWORD* ReturnLength();
   int* ReturnCode();

private:
   char* p_buffer;
   SqlColumn::Type v_bufferType;
   int v_bufferLength;
   int v_indicator;
   SDWORD v_returnLength;
   int v_returnCode;
protected:
};

//----------------------------------------------------------------------
inline char* ODBCColumn::Buffer()
{
   return (char*) p_buffer;
}
//----------------------------------------------------------------------
inline SWORD ODBCColumn::BufferType()
{
   SWORD C_dataType;
   switch(v_bufferType)
   {
   case SqlColumn::Boolean:
      C_dataType = SQL_C_SSHORT;
      v_returnLength = sizeof(int);
      break;
   case SqlColumn::Integer:
      C_dataType = SQL_C_LONG;
      v_returnLength = sizeof(int);
      break;
   case SqlColumn::StringType:
      C_dataType = SQL_C_CHAR;
      v_returnLength = sizeof(v_bufferLength);
      break;
   case SqlColumn::Long_Row:
      C_dataType = SQL_C_BINARY; //Changed from SQL_C_CHAR to SQL_C_BINARY - Manoj
      v_returnLength = sizeof(v_bufferLength);
      break;
   case SqlColumn::Raw:
      C_dataType = SQL_C_BINARY; //Added - Manoj
      v_returnLength = sizeof(v_bufferLength);
      break;
   default:
      C_dataType = SQL_C_CHAR;
      v_returnLength = sizeof(v_bufferLength);
      break;
   }
   return C_dataType;
}
//----------------------------------------------------------------------
inline int ODBCColumn::BufferLength()
{
   return v_bufferLength;
}
//----------------------------------------------------------------------
//inline int* ODBCColumn::Indicator()
//{
//   return &v_indicator;
//}
//----------------------------------------------------------------------
inline SDWORD* ODBCColumn::ReturnLength()
{
   //Comment By Manoj Agarwala, if a column is NULL, v_returnLength
   //is SQL_NO_DATA i.e. -1, we want to detect it and do different
   //things for different data types
//   if (v_returnLength<0)
//      v_returnLength = 0;
   return &v_returnLength;
}
//----------------------------------------------------------------------
inline int* ODBCColumn::ReturnCode()
{
   return &v_returnCode;
}
//----------------------------------------------------------------------
inline char* ODBCColumn::AsCharArray()
{
   if ( *(ReturnLength()) == SQL_NULL_DATA)
   {
      p_buffer[0] = '\0';
   }
   else
      p_buffer[*ReturnLength()] = '\0';

   return (char*) p_buffer;
}
//----------------------------------------------------------------------
inline LgsString ODBCColumn::AsString()
{
   LgsString s(AsCharArray());
   return s;
}
//----------------------------------------------------------------------
inline int ODBCColumn::AsIntegerFromString()
{
   if (*(ReturnLength()) == SQL_NULL_DATA)
   {
      return 0; //implies SQL_NULL_DATA
   }
   return atoi (p_buffer);
}
//----------------------------------------------------------------------
inline int ODBCColumn::AsInteger()
{
   switch(*ReturnLength())
   {
   case 2:
      return *(short int *)p_buffer;
   case 4:
      return *(int *)p_buffer;
   default:
      return 0; //implies SQL_NULL_DATA
   }
}
//----------------------------------------------------------------------
inline double ODBCColumn::AsDouble()
{
   if (*(ReturnLength()) == SQL_NULL_DATA)
   {
      return 0.0;
   }

   return *((double*)p_buffer);
}
//----------------------------------------------------------------------
inline bool ODBCColumn::AsBoolean()
{
   if (*ReturnLength() == SQL_NULL_DATA)       // for NULL columns
   {
      return false;
   }
   if (p_buffer[0] == 'Y')
   {
      return true;
   }
   return false;
}

#endif // __ODBCColumn_h__

