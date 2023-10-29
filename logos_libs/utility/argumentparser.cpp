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
// File - argumentparser.cpp
//
// Class - ArgumentParser (implementation)
//
// Description - implementation of the base class for a command line
//     Argument parser. Its primary accomplishment is performed in
//     the parser method. It converts the command line Arguments
//     into an ordered vector of strings.
//
//-------------------------------------------------------------------

#include <logos_include/logoscommon.h>
#include <logos_libs/utility/argumentparser.h>
#include <logos_libs/utility/argumentexception.h>

//-------------------------------------------------------------------
ArgumentParser::ArgumentParser()
{
   p_arguments = new ArgumentMap;
}
//-------------------------------------------------------------------
ArgumentParser::~ArgumentParser()
{
   delete p_arguments;
}
//---------------------------------------------------------------------
int ArgumentParser::ArgumentCount() const
{
   return Arguments().size();
}
//---------------------------------------------------------------------
ArgumentMap& ArgumentParser::Arguments() const
{
   return *p_arguments;
}
//---------------------------------------------------------------------
void ArgumentParser::AddArgument(const Argument& argument)
{
   Arguments()[argument.Keyword()] = argument;
}
//----------------------------------------------------------------------
void ArgumentParser::removeArguments(void)
{
   Arguments().clear();
}
//---------------------------------------------------------------------
const Argument& ArgumentParser::GetArgument(const LgsString& s)
{
   ArgumentMap::iterator i = Arguments().find(s);
   if (i == Arguments().end())
   {
      return Argument::NullArgument();
   }
   return Arguments()[s];
}
//-------------------------------------------------------------------
void ArgumentParser::ParseArgument(LgsString& s)
{
   // Simply converts the command line Arguments into an ordered vector of
   // strings. This is the primary purpose of the base class.

   Argument argument(s);
   AddArgument(argument);
}
