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
// File - PunctuationConstantSentenceUnit.cpp
//
// Class - PunctuationConstantSentenceUnit (implementation)
//
//-------------------------------------------------------------------

#include <logos_include/logoscommon.h>
#include <logos_libs/linguistic/sourcesentenceunit.h>
#include <logos_libs/linguistic/punctuationconstantsentenceunit.h>

//-------------------------------------------------------------------
PunctuationConstantSentenceUnit::PunctuationConstantSentenceUnit()
{
}
//-------------------------------------------------------------------
PunctuationConstantSentenceUnit::PunctuationConstantSentenceUnit(int opadr)
{
   switch (opadr)
   {
   case Period:
      v_word = ".";
      break;
   case GmComma:
      v_word = ",";
      break;
   case Colon:
      v_word = ":";
      break;
   case Semicolon:
      v_word = ";";
      break;
   case RightParen:
      v_word = ")";
      break;
   case LeftParen:
      v_word = "(";
      break;
   case GmHyphen:
      v_word = "-";
      break;
   case EqualSign:
      v_word = "=";
      break;
   case Exclamation:
      v_word = "!";
      break;
   case Hyphen2:
      v_word = "-";
      break;
   case Hyphen3:
      v_word = "-";
      break;
   case HyphenRemove:
      v_word = " ";
      break;
   case LeftParen2:
      v_word = "(";
      break;
   case RightParen2:
      v_word = ")";
      break;
   case Period2:
      v_word = ".";
      break;
   case MinusMinus:
      v_word = "--";
      break;
   }
}
//-------------------------------------------------------------------
PunctuationConstantSentenceUnit::PunctuationConstantSentenceUnit(LDictionary& dictionary)
                                :LinkedTargetUnit(dictionary)
{
}
//-------------------------------------------------------------------
PunctuationConstantSentenceUnit::PunctuationConstantSentenceUnit(const PunctuationConstantSentenceUnit& rhs)
                                :LinkedTargetUnit(rhs)
{
}
//-------------------------------------------------------------------
PunctuationConstantSentenceUnit::~PunctuationConstantSentenceUnit()
{
}
//---------------------------------------------------------------------
bool PunctuationConstantSentenceUnit::allowsPrecedingSpaces () const
{
   if (opadr() == Hyphen3)
      return true;
   else
      return false;
}
//---------------------------------------------------------------------
bool PunctuationConstantSentenceUnit::allowsTrailingSpaces() const
{
   switch (opadr())
   {
   case GmHyphen:
   case Hyphen2:
      return false;
   default:
      return true;
   }
}
//---------------------------------------------------------------------
void PunctuationConstantSentenceUnit::setSurfaceExpressionFromDictionary()
{
   surfaceExpressionFromString(word());
}
//---------------------------------------------------------------------
void PunctuationConstantSentenceUnit::capitalize(int offset)
{
}
//-------------------------------------------------------------------
bool PunctuationConstantSentenceUnit::isPunctuation() const
{
   return true;
}
//---------------------------------------------------------------------
int PunctuationConstantSentenceUnit::precedingSpaces() const
{
   if (opadr() == Hyphen3)
      return TargetSentenceUnit::precedingSpaces();
   else
      return 0;
}
//---------------------------------------------------------------------
int PunctuationConstantSentenceUnit::trailingSpaces() const
{
   switch (opadr())
   {
   case GmHyphen:
   case Hyphen2:
      return 0;
   default:
      return TargetSentenceUnit::trailingSpaces();
   }
}
//---------------------------------------------------------------------
void PunctuationConstantSentenceUnit::persistOut(ostream& stream)
{
   TargetSentenceUnit::persistOut(stream);
}
//-------------------------------------------------------------------
void PunctuationConstantSentenceUnit::persistIn(istream& stream)
{
   TargetSentenceUnit::persistIn(stream);
}
//---------------------------------------------------------------------
const LgsString& PunctuationConstantSentenceUnit::word() const
{
   return v_word;
}
//---------------------------------------------------------------------
bool PunctuationConstantSentenceUnit::isPunctuationConstantAddress(int opadr)
{
   switch (opadr)
   {
   case Period:
   case GmComma:
   case Colon:
   case Semicolon:
   case RightParen:
   case LeftParen:
   case GmHyphen:
   case EqualSign:
   case Exclamation:
   case Hyphen2:
   case Hyphen3:
   case HyphenRemove:
   case LeftParen2:
   case RightParen2:
   case Period2:
   case MinusMinus:
      return true;
      break;
   default:
      return false;
      break;
   }
}
//---------------------------------------------------------------------
void PunctuationConstantSentenceUnit::findAssociatedSourceUnit(SourceUnitVector& sourceUnits)
{
   if (sourceUnitPosition() > 0)
   {
      for (SourceUnitIterator i = sourceUnits.begin(); i != sourceUnits.end(); i++)
      {
         if ((*i)->position() == sourceUnitPosition())
         {
            setSourceSentenceUnit((*i));
            setIsProperName((*i)->isProperName());
//            setTrailingSpaces((*i)->trailingSpaces());
            break;
         }
      }
   }
}
//---------------------------------------------------------------------
void PunctuationConstantSentenceUnit::reconcileMarkup()
{
   if (sourceSentenceUnit())
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
}
