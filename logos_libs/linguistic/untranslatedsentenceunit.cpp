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
// File - UntranslatedSentenceUnit.cpp
//
// Class - UntranslatedSentenceUnit (implementation)
//
//-------------------------------------------------------------------

#include <logos_include/logoscommon.h>
#include <logos_libs/linguistic/untranslatedsentenceunit.h>
#include <logos_libs/linguistic/sourcesentenceunit.h>
//#include <logos_libs/linguistic/termsummary.h>

//-------------------------------------------------------------------
UntranslatedSentenceUnit::UntranslatedSentenceUnit()
{
}
//-------------------------------------------------------------------
UntranslatedSentenceUnit::UntranslatedSentenceUnit(LDictionary& dictionary)
                         :LinkedTargetUnit(dictionary)
{
}
//-------------------------------------------------------------------
UntranslatedSentenceUnit::~UntranslatedSentenceUnit()
{
}
//-------------------------------------------------------------------
/*void UntranslatedSentenceUnit::cacheTermSearch(TermSummary* terms, int sentenceNumber)
{
   terms->add(sourceSentenceUnit()->word(), "*", SearchTerm::UnfoundTerm,
              wordClassCode(), sentenceNumber, sourceSentenceUnit()->genderCode(),
              sourceSentenceUnit()->genderCode());
}*/
//---------------------------------------------------------------------
void UntranslatedSentenceUnit::persistOut(ostream& stream)
{
   LinkedTargetUnit::persistOut(stream);
}
//-------------------------------------------------------------------
void UntranslatedSentenceUnit::persistIn(istream& stream)
{
   LinkedTargetUnit::persistIn(stream);
}
//-------------------------------------------------------------------
const LgsString& UntranslatedSentenceUnit::word() const
{
   // Again this supplies the default behavior for target units
   // that are linked to source units. The default is that they
   // use the word text from the source side. Only the subclasses
   // that have a dictionary match use their own version ov the
   // word.

   if (opadr() == UntranslatedSentenceUnit::FunctionID)
      return origSourceWord.toString();
   else
      return p_sourceSentenceUnit->surfaceExpressionAsString();
}
//-------------------------------------------------------------------
void UntranslatedSentenceUnit::reconcileMarkup()
{
   const LWordVector& sourceWords = sourceSentenceUnit()->surfaceWords();

   if (sourceWords.size())
   {
      wordMarkup()->setId(sourceWords.begin()->markupId());

      for (LWordVector::const_iterator j = sourceWords.begin(); j != sourceWords.end(); j++)
      {
         wordMarkup()->orMask(j->markup());
      }
   }
}
//-------------------------------------------------------------------
void UntranslatedSentenceUnit::assignMarkupToWords()
{
   for (LWordIterator i = surfaceWords().begin(); i != surfaceWords().end(); i++)
   {
      i->markupId(wordMarkup()->id());
      i->markup(*wordMarkup());
   }
}
//-------------------------------------------------------------------
void UntranslatedSentenceUnit::findOrigSourceWord(LWordVector origList)
{
   origSourceWord.clear();
   for (LWordIterator i = p_sourceSentenceUnit->surfaceWords().begin();
                      i != p_sourceSentenceUnit->surfaceWords().end(); i++)
   {
      for (LWordIterator j = origList.begin(); j != origList.end(); j++)
      {
         if ((*i).markupId() == (*j).markupId())
         {
            origSourceWord += (*j);
            break;
         }
      }
   }
}
//---------------------------------------------------------------------
int UntranslatedSentenceUnit::precedingSpaces() const
{
    return origSourceWord.precedingSpaces();
}
