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
// File - sentenceunit.cpp
//
// Class - SentenceUnit (implementation)
//
//-------------------------------------------------------------------

#include <logos_include/logoscommon.h>
#include <logos_libs/linguistic/sentenceunit.h>
#include <logos_libs/linguistic/ldictionary.h>
#include <logos_libs/linguistic/ldictionaryentry.h>

//-------------------------------------------------------------------
SentenceUnit::SentenceUnit()
             :v_sentenceAddress(0),
              v_isEndOfSentence(false),
              v_caseState(UndeterminedCase),
              p_sentence(0)
{
   // This is a default constructor. It should never be used. It is
   // required for STL vectors.
}
//-------------------------------------------------------------------
SentenceUnit::SentenceUnit(LDictionary& dictionary)
             :LSyntaxUnit(dictionary),
              v_sentenceAddress(0),
              v_isEndOfSentence(false),
              v_caseState(UndeterminedCase),
              p_sentence(0)
{
}
//-------------------------------------------------------------------
SentenceUnit::SentenceUnit(const SentenceUnit& rhs)
             :LSyntaxUnit(rhs),
              v_sentenceAddress(rhs.sentenceAddress()),
              v_surfaceExpression(rhs.v_surfaceExpression),
              p_sentence(rhs.p_sentence),
              v_caseState(rhs.v_caseState),
              v_isEndOfSentence(rhs.v_isEndOfSentence),
              v_wordMarkup(rhs.v_wordMarkup)
{
}
//-------------------------------------------------------------------
SentenceUnit::~SentenceUnit()
{
}
//---------------------------------------------------------------------
SentenceUnit::CaseState SentenceUnit::caseState() const
{
   if (UndeterminedCase == v_caseState)
   {
      const_cast<SentenceUnit*>(this)->determineCaseState ();
   }
   return v_caseState;
}
//-------------------------------------------------------------------
void SentenceUnit::determineCaseState()
{
   const LgsString& surface = surfaceExpressionAsString();

   setCaseState (IrrelevantCase);

   if ((SentenceUnit::BosIdentifier != sentenceAddress()) &&
       (StringUtil::containsAlphabetic(surface)))
   {
      if (StringUtil::isAllUpperCase(surface))
      {
         if (1 == surface.length())
         {
            setCaseState(BeginsUpperCase);
         }
         else
         {
            setCaseState(AllUpperCase);
         }
      }
      else if (StringUtil::beginsUpperCase(surface))
      {
         setCaseState(BeginsUpperCase);
      }
      else
      {
         setCaseState(LowerCase);
      }
   }
}
//---------------------------------------------------------------------
bool SentenceUnit::isComma() const
{
   const LgsString& surface = surfaceExpressionAsString();
   if (surface == ",")
      return true;
   else
      return false;
}
//---------------------------------------------------------------------
bool SentenceUnit::isPunctuation() const
{
   return (LLanguage::PUNCTUATION == wordClassCode());
}
//---------------------------------------------------------------------
bool SentenceUnit::isValidFirstWord()
{
   // If punctuation then not valid.
   if (isPunctuation())
      return false;

   // If noun, definite article or number then further processing is to be done
   if ((wordClassCode() == LLanguage::NOUN) || (wordClassCode() == LLanguage::ARTICLE_DEFINITE) ||
       (wordClassCode() == LLanguage::ARITHMATE))
   {
      LgsString firstWord = *(surfaceExpression().words().begin());

      // If there are any digits in the word then not valid
      if ((StringUtil::containsDigit(firstWord) || !StringUtil::isAllAlpha(firstWord.substr(0, 1))))
      {
         // If the word is a protected word, we'll assume it's valid as a first word in sentence.
         if (firstWord != "^p")
         {
            return false;
         }
      }
   }

   return true;
}
//---------------------------------------------------------------------
void SentenceUnit::persistOut(ostream& stream)
{
   //LSyntaxUnit::persistOut(stream);
}
//-------------------------------------------------------------------
void SentenceUnit::persistIn(istream& stream)
{
   //LSyntaxUnit::persistIn(stream);
}
//-------------------------------------------------------------------
int SentenceUnit::translatedWordCount() const
{
   return 0;
}
