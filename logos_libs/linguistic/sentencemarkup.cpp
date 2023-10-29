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
// File - SentenceMarkup.cpp
//
// Class - SentenceMarkup (implementation)
//
//-------------------------------------------------------------------

#include <logos_include/logoscommon.h>
#include <logos_libs/linguistic/sentencemarkup.h>

//-------------------------------------------------------------------
SentenceMarkup::SentenceMarkup()
               :v_id(0)
{
}
//-------------------------------------------------------------------
SentenceMarkup::SentenceMarkup(const SentenceMarkup& aWord)
               :v_id(aWord.id())
{
   // This is the copy constructor.
}
//-------------------------------------------------------------------
SentenceMarkup::~SentenceMarkup()
{
}
//-------------------------------------------------------------------
const SentenceMarkup& SentenceMarkup::operator=(const SentenceMarkup& rhs)
{
   if (&rhs != this)
   {
      id(rhs.id());
   }
   return *this;
}
//---------------------------------------------------------------------
LgsString SentenceMarkup::idAsString()
{
   char sentIdStr[25] = "XXX";
   if (v_id > 0)
      _ultoa(v_id, sentIdStr, 10);
   return LgsString(sentIdStr);
}
//---------------------------------------------------------------------
ostream& operator<<(ostream& stream, const SentenceMarkup& object)
{
   stream << object.id() << " ";
   return stream;
}
//-------------------------------------------------------------------
istream& operator>>(istream& stream, SentenceMarkup& object)
{
   unsigned long newID;
   stream >> newID;
   object.id(newID);
   return stream;
}
