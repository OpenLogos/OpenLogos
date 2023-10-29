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
#ifndef __utility_argumentparser_h__
#define __utility_argumentparser_h__

//---------------------------------------------------------------------
// File - argumentparser.h
//
// Class - ArgumentMap
// Class - ArgumentParser (interface)
//
// Description - An abstract class. This is base class class for the
//     application specific Argument parsing classes. objects of this
//     type parse the command line Arguments of a program and store
//     them as ordered set of strings. Since there are no pure virtual
//     functions in this class it is made abstract by virtue of the
//     fact that the constructors are protected (they can not be used
//     to create objects of this class).
//
//---------------------------------------------------------------------

#include <logos_libs/utility/argument.h>

//---------------------------------------------------------------------
class ArgumentMap: public LgsMap(LgsString, Argument)
{
};

//---------------------------------------------------------------------
class ArgumentParser
{
public:
   ArgumentParser();
   virtual ~ArgumentParser();

   int ArgumentCount() const;
   virtual const Argument& GetArgument(const LgsString&);
   void AddArgument(const Argument&);
   void removeArguments(void);
   const Argument& operator=(const Argument&);

   const LgsString& Value(const LgsString& keyword);
   const LgsString& AsString(const LgsString& keyword);
   const char* AsCharacterArray(const LgsString& keyword);
   int AsInteger(const LgsString& keyword);

protected:
   virtual void ParseArgument(LgsString&);

private:
   //-----------------------------------------------------------------
   // This vector is the whole point of the object's existence. After
   // an object of this class has been created all of its information
   // lies inside of this vector.
   //-----------------------------------------------------------------

   ArgumentMap& Arguments() const;
   ArgumentMap* p_arguments;
};

//---------------------------------------------------------------------
inline const LgsString& ArgumentParser::Value(const LgsString& keyword)
{
   return GetArgument(keyword).Value();
}
//---------------------------------------------------------------------
inline int ArgumentParser::AsInteger(const LgsString& keyword)
{
   return GetArgument(keyword).AsInteger();
}
//---------------------------------------------------------------------
inline const LgsString& ArgumentParser::AsString(const LgsString& keyword)
{
   return GetArgument(keyword).AsString();
}
//---------------------------------------------------------------------
inline const char* ArgumentParser::AsCharacterArray(const LgsString& keyword)
{
   return GetArgument(keyword).AsCharacterArray();
}

#endif // __utility_argumentparser_h__


