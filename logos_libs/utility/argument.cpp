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
// File - argument.cpp
//
// Class - Argument (implementation)
//
//---------------------------------------------------------------------

#include <logos_include/logoscommon.h>
#include <logos_libs/utility/argument.h>
#include <logos_libs/utility/argumentexception.h>
#include <logos_libs/regex/charutil.h>

class IsSpace
{
public:
   bool operator()(char c) { return CharUtil::isBlank(c); }
};

LgsString Argument::v_nullString;
Argument Argument::v_nullArgument(v_nullString, v_nullString);

//-------------------------------------------------------------------
Argument::Argument()
         :v_value(v_nullString)
{
}
//-------------------------------------------------------------------
Argument::Argument(const LgsString& keyword)
         :v_keyword(keyword),
          v_value(v_nullString)
{
}
//-------------------------------------------------------------------
Argument::Argument(LgsString& argString)
{
   SetFromString(argString);
}
//-------------------------------------------------------------------
Argument::Argument(const LgsString& keyword, const LgsString& value)
         :v_keyword(keyword),
          v_value(value)
{
}
//-------------------------------------------------------------------
Argument::Argument(const Argument& rhs)
         :v_keyword(rhs.Keyword()),
          v_value(rhs.Value())
{
}
//-------------------------------------------------------------------
Argument::~Argument()
{
}
//-------------------------------------------------------------------
void Argument::SetFromString(const LgsString& argString)
{
   LgsString temp;
   temp.append(argString);
   LgsString::iterator newEnd = remove_if(temp.begin(), temp.end(), IsSpace());
   temp.erase(newEnd, temp.end());
   LgsString::size_type pos = temp.find_first_of('=');
   if (pos == NPOS)
   {
      throw(ArgumentException("bad argument format!"));
   }
   SetKeyword(temp.substr(0, pos ));
   SetValue(temp.substr(pos + 1, temp.size() - pos - 1 ));
}
//-------------------------------------------------------------------
const Argument& Argument::operator=(const Argument& rhs)
{
   if (&rhs != this)
   {
      SetKeyword(rhs.Keyword());
      SetValue(rhs.Value());
   }
   return *this;
}
//-------------------------------------------------------------------
bool Argument::operator==(const Argument& rhs) const
{
   if ((Keyword() == rhs.Keyword()) && (Value() == rhs.Value()))
   {
      return true;
   }
   return false;
}
//-------------------------------------------------------------------
bool Argument::operator!=(const Argument& rhs) const
{
   return !(operator==(rhs));
}

