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
// File - transfersentenceuit.cpp
//
// Class - TargetDictionaryUnit (implementation)
//
//-------------------------------------------------------------------

#include <logos_include/logoscommon.h>
#include <logos_libs/translutility/translcommonobjects.h>
#include <logos_libs/linguistic/targetdictionaryunit.h>
#include <logos_libs/linguistic/lsentence.h>

//---------------------------------------------------------------------
const LSemantoSyntacticUnit& TargetDictionaryUnit::primarySsu() const
{
   return dictionaryEntry().ssuAt(primarySsuPosition());
}
//-------------------------------------------------------------------
int TargetDictionaryUnit::sourceMeaningID()
{
   return sourceSentenceUnit()->meaningID();
}
//-------------------------------------------------------------------
TargetDictionaryUnit::TargetDictionaryUnit()
                     :p_dictionaryEntry(0),
                      v_blackHoleLength(0),
                      v_areTransfersSorted(false),
                      v_useCombiningForm(false),
                      v_isDecomposed(false)
{
}
//-------------------------------------------------------------------
TargetDictionaryUnit::TargetDictionaryUnit(LDictionary& dictionary)
                     :LinkedTargetUnit(dictionary),
                      p_dictionaryEntry(0),
                      v_blackHoleLength(0),
                      v_areTransfersSorted(false),
                      v_useCombiningForm(false),
                      v_isDecomposed(false)
{
}
//-------------------------------------------------------------------
TargetDictionaryUnit::TargetDictionaryUnit(const TargetDictionaryUnit& rhs)
                     :LinkedTargetUnit(rhs),
                      p_dictionaryEntry(rhs.p_dictionaryEntry),
                      v_blackHoleLength(rhs.v_blackHoleLength),
                      v_transfers(rhs.v_transfers),
                      v_areTransfersSorted(rhs.v_areTransfersSorted),
                      v_useCombiningForm(rhs.v_useCombiningForm),
                      v_isDecomposed(rhs.v_isDecomposed),
                      v_stem(rhs.v_stem),
                      v_separableVerbPrefix(rhs.v_separableVerbPrefix),
                      v_inseparableVerbPrefix(rhs.v_inseparableVerbPrefix)
{
}
//-------------------------------------------------------------------
TargetDictionaryUnit::~TargetDictionaryUnit()
{
   deleteEntry();
}
//-------------------------------------------------------------------
void TargetDictionaryUnit::capitalize(int offset)
{
   bool matchedOnFullyCapped = sourceSentenceUnit()->matchedOnFullyCapped();

   // If the sentence was not originally all upper case then special cases must be considered.
   if (!sentence().isTargetAllUpperCase())
   {
      // If source was all upper case and the match was on the fully capped source.
      if (SentenceUnit::AllUpperCase == sourceSentenceUnit()->caseState())
      {
         if (matchedOnFullyCapped)
         {
            acceptTransfer(true);
         }
         else
         {
            // If source was an abbreviation type and the transfer is not all upper case.
            if (sourceSentenceUnit()->wordTypeCode() == abbrevTYPE)
            {
               // If target is German or English and transfer is a word type.
               if ((wordTypeCode() == wordTYPE) &&
                   ((TranslCommonObjects::GetTargetLanguage()->id() == LLanguage::GermanID) ||
                    (TranslCommonObjects::GetTargetLanguage()->id() == LLanguage::EnglishID)))
               {
                  acceptTransfer(true);
               }
               else
               {
                  // If target is French, Spanish or Italian.
                  if ((TranslCommonObjects::GetTargetLanguage()->id() == LLanguage::FrenchID) ||
                      (TranslCommonObjects::GetTargetLanguage()->id() == LLanguage::SpanishID) ||
                      (TranslCommonObjects::GetTargetLanguage()->id() == LLanguage::ItalianID))
                  {
                     acceptTransfer(true);
                  }
               }
            }
         }
      }

      // If source is all upper case and source was word type and target is Italian.
      if ((SentenceUnit::AllUpperCase == sourceSentenceUnit()->caseState()) &&
          (sourceSentenceUnit()->wordTypeCode() == wordTYPE) &&
          (TranslCommonObjects::GetTargetLanguage()->id() == LLanguage::ItalianID))
      {
         // If transfer begins with an upper case letter.
         if (StringUtil::beginsUpperCase(surfaceExpressionAsString()))
         {
            acceptTransfer(true);
         }
      }
   }
   LinkedTargetUnit::capitalize(offset);
}
//-------------------------------------------------------------------
int TargetDictionaryUnit::patNumber() const
{
   int p = primarySsu().patNumber();

   if (LLanguage::GermanID == language().id())
   {
      switch (p)
      {
      case 158:
      case 160:
         p = 536;
         break;
      case 159:
         p = 546;
         break;
      }
   }
   return p;
}
//---------------------------------------------------------------------
void TargetDictionaryUnit::assignMarkupToWords()
{
   // The markup of each word in a target sentence unit must be given
   // the markup information from the sentence unit itself.
   for (LWordIterator i =  surfaceWords().begin(); i != surfaceWords().end(); i++)
   {
      i->markupId(wordMarkup()->id());
      i->markup(*wordMarkup());
   }
}
//-------------------------------------------------------------------
void TargetDictionaryUnit::deleteEntry()
{
   // Performs memory cleanup. However, care must be taken not to
   // delete objects that are cached.
   if (p_dictionaryEntry)
   {
	   if (! sourceSentenceUnit() || !(sourceSentenceUnit()->isEndOfSentence()))
	   {
			if (!dictionaryEntry().isCached())
			{
				p_dictionaryEntry->deleteSsuComponents();
				delete p_dictionaryEntry;
			}
		}
   }
}
//---------------------------------------------------------------------
void TargetDictionaryUnit::persistOut(ostream& stream)
{
   TargetSentenceUnit::persistOut(stream);
}
//-------------------------------------------------------------------
void TargetDictionaryUnit::persistIn(istream& stream)
{
   TargetSentenceUnit::persistIn(stream);
}
//-------------------------------------------------------------------
void TargetDictionaryUnit::decomposeWord() const
{
   TargetDictionaryUnit* p = const_cast<TargetDictionaryUnit*>(this);

   if (!v_isDecomposed)
   {
      p->v_isDecomposed = true;

      p->v_separableVerbPrefix = word().substr(verbPrefixSeparablePosition(),
                                               verbPrefixSeparableLength());
      p->v_inseparableVerbPrefix = word().substr(verbPrefixInseparablePosition(),
                                                 verbPrefixInseparableLength());
      p->v_stem = word().substr(v_separableVerbPrefix.length() +
                                v_inseparableVerbPrefix.length() - 1);
   }
}
//---------------------------------------------------------------------
void TargetDictionaryUnit::makeAspiration(TargetSentenceUnit* prev)
{
   LgsVector(int) positions;

   language().findAspirePoints(prev, this, &positions);
   if (!positions.empty())
   {
      for (LgsVector(int)::iterator i = positions.end() - 1; i >= positions.begin(); i--)
      {
         insertString(*i, "@ASPIRE");
      }
   }
}
//-------------------------------------------------------------------
const LgsString& TargetDictionaryUnit::stem() const
{
   decomposeWord();
   return v_stem;
}
//-------------------------------------------------------------------
const LgsString& TargetDictionaryUnit::separableVerbPrefix() const
{
   decomposeWord();
   return v_separableVerbPrefix;
}
//-------------------------------------------------------------------
const LgsString& TargetDictionaryUnit::inseparableVerbPrefix() const
{
   decomposeWord();
   return v_inseparableVerbPrefix;
}
//-------------------------------------------------------------------
int TargetDictionaryUnit::positionAfterVerbPrefix() const
{
   int positionAfterSeparable = verbPrefixSeparablePosition() + verbPrefixSeparableLength();
   int positionAfterInseparable = verbPrefixInseparablePosition() + verbPrefixInseparableLength();

   if (positionAfterSeparable > positionAfterInseparable)
   {
      return positionAfterSeparable;
   }
   else
   {
      return positionAfterInseparable;
   }
}
//---------------------------------------------------------------------
void TargetDictionaryUnit::reconcileMarkup()
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
int TargetDictionaryUnit::usageID() const
{
   return primarySsu().usageID();
}
//-------------------------------------------------------------------
int TargetDictionaryUnit::transferID()
{
   Transfer* t = transfer();
   return (t) ? t->id() : 0;
}
//-------------------------------------------------------------------
int TargetDictionaryUnit::combiningFormCode()
{
   if (v_useCombiningForm)
   {
      Transfer* t = transfer();
      if (t)
      {
         return t->combiningFormCode();
      }
   }
   return 0;
}
//-------------------------------------------------------------------
Transfer* TargetDictionaryUnit::transfer()
{
   if (v_transfers.empty())
   {
      return 0;
   }
   if (!v_areTransfersSorted)
   {
      sort(v_transfers.begin(), v_transfers.end());
      v_areTransfersSorted = true;
   }

   Transfer& theTransfer = v_transfers.at(0);

   switch (transferSwitch())
   {
   case FirstAlternateTransfer:
      if (v_transfers.size() > 1)
         theTransfer = v_transfers.at(1);
      else
         v_useCombiningForm = true;
      break;
   case SecondAlternateMainTransfer:
      if (v_transfers.size() > 2)
         theTransfer = v_transfers.at(2);
      else
         v_useCombiningForm = true;
      break;
   case SecondAlternateFirstTransfer:
      if (v_transfers.size() > 2)
         theTransfer = v_transfers.at(2);
      else
      {
         if (v_transfers.size() > 1)
         {
            theTransfer = v_transfers.at(1);
            v_useCombiningForm = true;
         }
         else
            v_useCombiningForm = true;
      }
      break;
   }
   return &theTransfer;
}
//---------------------------------------------------------------------
const LgsString& TargetDictionaryUnit::word() const
{
   return primarySsu().word();
}
//-------------------------------------------------------------------
int TargetDictionaryUnit::wordClassCode() const
{
   int w = primarySsu().wordClassCode();

   if (w == LLanguage::AUXILIARY)
   {
      w = (short)LLanguage::VERB;
   }

   if (LLanguage::GermanID == language().id())
   {
      switch (primarySsu().patNumber())
      {
      case 158:
      case 159:
      case 160:
         w = 4;
         break;
      }
   }
   return w;
}
//-------------------------------------------------------------------
int TargetDictionaryUnit::declension() const
{
   int w = TargetSentenceUnit::declension();

   if (LLanguage::GermanID == language().id())
   {
      switch (primarySsu().patNumber())
      {
      case 158:
      case 159:
      case 160:
         if ((w != 6) && (w != 9))
            w = 3;
         break;
      }
   }
   return w;
}
//-------------------------------------------------------------------
int TargetDictionaryUnit::degree() const
{
   int w = TargetSentenceUnit::degree();

   if (LLanguage::GermanID == language().id())
   {
      switch (primarySsu().patNumber())
      {
      case 158:
      case 159:
      case 160:
         w = 1;
         break;
      }
   }
   return w;
}
//-------------------------------------------------------------------
void TargetDictionaryUnit::checkSconValues()
{
   setWordClassCode(primarySsu().wordClassCode());
   TargetSentenceUnit::checkSconValues();
}
