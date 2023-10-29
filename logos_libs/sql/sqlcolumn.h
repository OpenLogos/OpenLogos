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
#ifndef __SqlColumn_h__
#define __SqlColumn_h__

//----------------------------------------------------------------------
// File - SqlColumn.h
//
// Class - SqlColumn (Abstract)
//
//----------------------------------------------------------------------

#include <logos_libs/utility/timestamp.h>

struct longRaw
{
   int len;
   char buf[2000];
};

class SqlColumn
{
public:
   //------------------------------------------------------------------
   // Destructor is virtual to allow inheritance of this class without
   // creating a danger of memory leaks.
   //------------------------------------------------------------------
   virtual ~SqlColumn();

   //------------------------------------------------------------------
   virtual bool AsBoolean() = 0;
   virtual char* AsCharArray() = 0;
   virtual double AsDouble() = 0;
   virtual LgsString AsString() = 0;
   virtual int AsInteger() = 0;
   virtual int AsIntegerFromString() = 0;
   virtual unsigned char* AsLongRaw() = 0;
   virtual unsigned char* AsRaw() = 0;
   virtual TimeStamp AsTimeStamp() = 0;
   virtual Time AsTime() = 0;
   virtual Date AsDate() = 0;

   //------------------------------------------------------------------
   LgsString& Name() { return v_name; }

   enum Type
   {
      Boolean,
      Integer,
      StringType,
      Long_Row,
      Raw,
      TimeStampType, // Manoj Agarwala - 7/7/97, PS: Oracle's date stores both date and time
      TimeType,
      DateType
   };
   virtual Type GetType() = 0;
   virtual int GetSize() = 0;

   const SqlColumn& operator=(const SqlColumn&);

   virtual bool operator==(const SqlColumn&) const;
   virtual bool operator!=(const SqlColumn&) const;
   virtual bool operator< (const SqlColumn&) const;

   virtual int Compare(const SqlColumn&) const;

protected:
   //------------------------------------------------------------------
   // Constructors are protected because this is an abstract class.
   // Users should not be allowed to create instances of this class.
   //------------------------------------------------------------------
   SqlColumn();

   LgsString v_name;
};

#endif // __SqlColumn_h__

