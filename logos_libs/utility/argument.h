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
#ifndef __utility_argument_h__
#define __utility_argument_h__

//---------------------------------------------------------------------
// File - argument.h
//
// Class - Argument (interface)
//
//---------------------------------------------------------------------

#include <logos_libs/utility/stringutil.h>

class Argument
{
public:
   Argument();
   Argument(const Argument&);
   Argument(const LgsString& keyword, const LgsString& value);
   Argument(const LgsString& keyword);
   Argument(LgsString& argString);
   ~Argument();

   const LgsString& Keyword() const;
   const LgsString& Value() const;

   const LgsString& AsString() const;
   const char* AsCharacterArray() const;
   int AsInteger() const;

   void SetKeyword(const LgsString& s);
   void SetValue(const LgsString& s);
   void SetFromString(const LgsString& argString);

   const Argument& operator=(const Argument&);

   bool operator==(const Argument&) const;
   bool operator!=(const Argument&) const;

   bool IsValueNull() const;
   bool IsNull() const;

   static const Argument& NullArgument();
   static const LgsString& NullString();

private:
   LgsString v_keyword;
   LgsString v_value;
   static LgsString v_nullString;
   static Argument v_nullArgument;
};

//---------------------------------------------------------------------
inline bool Argument::IsValueNull() const
{
   return (v_value == v_nullString);
}
//---------------------------------------------------------------------
inline bool Argument::IsNull() const
{
   return (*this == v_nullArgument);
}
//---------------------------------------------------------------------
inline const Argument& Argument::NullArgument()
{
   return v_nullArgument;
}
//---------------------------------------------------------------------
inline const LgsString& Argument::NullString()
{
   return v_nullString;
}
//---------------------------------------------------------------------
inline const LgsString& Argument::Keyword() const
{
   return v_keyword;
}
//---------------------------------------------------------------------
inline const LgsString& Argument::AsString() const
{
   return Value();
}
//---------------------------------------------------------------------
inline int Argument::AsInteger() const
{
   return StringUtil::asInteger(Value());
}
//---------------------------------------------------------------------
inline const LgsString& Argument::Value() const
{
   return v_value;
}
//---------------------------------------------------------------------
inline const char* Argument::AsCharacterArray() const
{
   return Value().c_str();
}
//---------------------------------------------------------------------
inline void Argument::SetKeyword(const LgsString& s)
{
   v_keyword = s;
}
//---------------------------------------------------------------------
inline void Argument::SetValue(const LgsString& s)
{
   v_value = s;
}

#endif      // __utiltity_argument_h__



