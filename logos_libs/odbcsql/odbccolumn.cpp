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
// File - ODBCColumn.cpp
//
// Class - ODBCColumn (implementation)
//
//-------------------------------------------------------------------

#include <logos_include/logoscommon.h>
#include <logos_libs/odbcsql/odbccolumn.h>

//----------------------------------------------------------------------
ODBCColumn::ODBCColumn(SqlColumn::Type aBufferType, int aBufferLength)
           :v_bufferType(aBufferType),
            v_bufferLength(aBufferLength)
{
   p_buffer = new char[aBufferLength];
   v_indicator = 0;
}
//----------------------------------------------------------------------
ODBCColumn::~ODBCColumn()
{
   delete[] p_buffer;
}
//----------------------------------------------------------------------
unsigned char* ODBCColumn::AsLongRaw(void)
{
   //return (unsigned char*)(AsCharArray()+30); //- Original
   return (unsigned char*)(AsCharArray()); //- Changed By Manoj Agarwala
}
//----------------------------------------------------------------------
unsigned char* ODBCColumn::AsRaw(void)
{
   return (unsigned char*)(AsCharArray());
}
//----------------------------------------------------------------------
TimeStamp ODBCColumn::AsTimeStamp()
{
   TimeStamp *p_ts  = (TimeStamp *)p_buffer;
   return (*p_ts);
}
//----------------------------------------------------------------------
Time ODBCColumn::AsTime()
{
   Time *p_t  = (Time *)p_buffer;
   return (*p_t);
}
//----------------------------------------------------------------------
Date ODBCColumn::AsDate()
{
   Date *p_d  = (Date *)p_buffer;
   return (*p_d);
}
//----------------------------------------------------------------------
SqlColumn::Type ODBCColumn::GetType(void)
{
   return v_bufferType;
}
//----------------------------------------------------------------------
int ODBCColumn::GetSize(void)
{
   return v_bufferLength;
}

