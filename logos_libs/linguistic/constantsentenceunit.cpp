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
// File - constantsentenceunit.cpp
//
// Class - ConstantSentenceUnit (implementation)
//
//-------------------------------------------------------------------

#include <logos_include/logoscommon.h>
#include <logos_libs/linguistic/constantsentenceunit.h>
#include <logos_libs/linguistic/lsentence.h>
#include <logos_libs/linguistic/sourcesentenceunit.h>
#include <logos_libs/linguistic/blackhole.h>

//-------------------------------------------------------------------
ConstantSentenceUnit::ConstantSentenceUnit()
                     :v_isHigh(false)
{
}
//-------------------------------------------------------------------
ConstantSentenceUnit::ConstantSentenceUnit(LDictionary& dictionary)
                     :TargetDictionaryUnit(dictionary),
                      v_isHigh(false)
{
}
//-------------------------------------------------------------------
ConstantSentenceUnit::~ConstantSentenceUnit()
{
}
//---------------------------------------------------------------------
void ConstantSentenceUnit::generateDictionaryEntry()
{
   dictionary().createConstantEntry(*this);
   checkSconValues();
}
//---------------------------------------------------------------------
void ConstantSentenceUnit::setSurfaceExpressionFromDictionary()
{
   if (isDictionaryMatched())
   {
      surfaceExpressionFromString(word());
   }
   else
   {
      char buffer[30];
      sprintf(buffer, "@%d", constantID());
      surfaceExpressionFromString(buffer);
   }
}
//---------------------------------------------------------------------
void ConstantSentenceUnit::generateStem()
{
   if (isDictionaryMatched())
   {
      dictionary().generateStem(this);
   }
}
//---------------------------------------------------------------------
int ConstantSentenceUnit::precedingSpaces() const
{
   return 1;
}
//-------------------------------------------------------------------
int ConstantSentenceUnit::blackHoleSentenceAddress() const
{
   if ((LSentence::minimumSourceAddress <= sourceUnitPosition()) &&
       (LSentence::maximumSourceAddress >= sourceUnitPosition()))
   {
      return sourceUnitPosition();
   }
   return 0;
}
//-------------------------------------------------------------------
void ConstantSentenceUnit::insertBlackHole(const BlackHole* b)
{
   // This may eventually be turned into an exception, in any case the only unit
   // that receives this message should be of the TranslatedSentenceUnit subclass

   if ((isDictionaryMatched()) && (!sourceSentenceUnit()->isEndOfSentence()))
   {
      const LgsString& s = b->text();
      LWord w(s, &language());
      insertWord(blackHoleLocation(), w);
//      insertWord(1 + blackHoleLength(), w);
      incrementBlackHoleLength(1);
   }
}
//---------------------------------------------------------------------
void ConstantSentenceUnit::persistOut(ostream& stream)
{
    TargetSentenceUnit::persistOut(stream);
}
//-------------------------------------------------------------------
void ConstantSentenceUnit::persistIn(istream& stream)
{
   TargetSentenceUnit::persistIn(stream);
}
