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
//---------------------------------------------------------------------
// File - system.cpp
//
// Class - System (implementation)
//
//---------------------------------------------------------------------

#include <logos_include/logoscommon.h>
#include <logos_libs/utility/system.h>

LgsStringVector* System::CreateSubStrVector(char delimiter, LgsString& str)
{
   LgsStringVector* substrvec = new LgsStringVector;

   LgsString::size_type newBegin = 0;
   LgsString::size_type newEnd = 0;
   LgsString newstr;
   int working;

   while (true)
   {
      newEnd = str.find(delimiter, newBegin);

      // Did I find anymore delimiters?
      if (newEnd == NPOS)
         break;
      working = newEnd - newBegin;
      newstr.assign(str, newBegin, working);
      substrvec->push_back(newstr);
      newBegin = newEnd + 1;
   }

   working = str.size() - newBegin;
   if ((!working) || (str[newBegin] == 0xd))
      newstr = "";
   else
      newstr.assign(str, newBegin, working);
   substrvec->push_back(newstr);

   return substrvec;
}
//-------------------------------------------------------------------
LgsStringVector* System::CreateStringVector(char* delimiters, const LgsString& str)
{
   LgsStringVector * strvec = new LgsStringVector;

   LgsString::size_type newBegin = 0;
   LgsString::size_type newEnd = 0;
   LgsString newstr;
   int working;
   int count = 0;

   while (1)
   {
      newEnd = str.find_first_of(delimiters, newBegin);

      // Did I find anymore delimiters?
      if (newEnd == NPOS)
      {
         count++;
         break;
      }
      working = newEnd - newBegin;
      newstr.assign(str, newBegin, working);
      if (newstr != "")
      {
         strvec->push_back(newstr);
         count++;
      }
      newBegin = newEnd + 1;
   }

   working = str.size() - newBegin;
   if ((!working) || (str[newBegin] == 0xd))
      newstr = "";
   else
      newstr.assign(str, newBegin, working);
   strvec->push_back(newstr);

   return strvec;
}
//-------------------------------------------------------------------
LgsString System::FirstW(const LgsString& newValue)
{
   LgsString::size_type foundpos;
   LgsString sparestr;
   sparestr = newValue;

   foundpos = newValue.find_first_of(" -/");
   if (foundpos != NPOS)
   {
      sparestr.assign( newValue.c_str(), foundpos);
   }
   return sparestr;
}
