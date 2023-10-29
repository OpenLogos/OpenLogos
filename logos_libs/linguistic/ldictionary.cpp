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
// --------------------------------------------------------------------------
// File - LDictionary.cpp (implementation)
// --------------------------------------------------------------------------

#include <logos_include/logoscommon.h>
#include <logos_libs/linguistic/ldictionary.h>
#include <logos_libs/linguistic/linguisticfactory.h>
#include <logos_libs/linguistic/sourcesentenceunit.h>
#include <logos_libs/linguistic/ldictionaryentry.h>
#include <logos_libs/linguistic/entrybuilder.h>
#include <logos_libs/linguistic/targetsentenceunit.h>
#include <logos_libs/linguistic/targetunitbuilder.h>
#include <logos_libs/linguistic/llanguage.h>
#include <logos_libs/linguistic/functionconstantsentenceunit.h>
#include <logos_libs/linguistic/punctuationconstantsentenceunit.h>
#include <logos_libs/linguistic/constantsentenceunit.h>
#include <logos_libs/linguistic/translatedsentenceunit.h>


// --------------------------------------------------------------------------
// Constructor
// --------------------------------------------------------------------------
LDictionary::LDictionary(LLanguage* aSourceLanguage, 
						 LLanguage* aTargetLanguage,
                         EntryBuilder* aBuilder, 
						 LinguisticFactory* aFactory)
            :p_sourceLanguage(aSourceLanguage),
             p_targetLanguage(aTargetLanguage),
             p_builder(aBuilder),
             p_linguisticFactory(aFactory),
             p_targetBuilder(0)
{
}


// --------------------------------------------------------------------------
// Destructor
// --------------------------------------------------------------------------
LDictionary::~LDictionary()
{
}


// --------------------------------------------------------------------------
SourceSentenceUnit* LDictionary::createProtectedUnit(const LWordIterator& word)
{
   SourceSentenceUnit* pSentenceUnit = new SourceSentenceUnit(*this);

   LDictionaryEntry* pEntry = builder().buildProtectedEntry(*word);

   pSentenceUnit->surfaceExpressionFromString(*word);
   pSentenceUnit->setProtectedWord(true);

   pSentenceUnit->dictionaryEntry(pEntry);
   pSentenceUnit->getMarkupFromWords();

   return pSentenceUnit;
}


// --------------------------------------------------------------------------
// Calls the builder. The builder really constructs the Dictionary Entry. This method allows the 
// Linguistic object (i.e., Documents, Sentences and Words) to be independent of the building process.
// --------------------------------------------------------------------------
SourceSentenceUnit* LDictionary::createSentenceUnit(const LWordIterator& beginSource,
                                                    const LWordIterator& endSource,
                                                    bool BOS, 
													LWordVector& sourceWords, bool bAllCapitalWords)
{
   SourceSentenceUnit* pSentenceUnit = new SourceSentenceUnit(*this);

   LDictionaryEntry* pEntry = 0;
   bool isEOSPunctuation = false;
   bool isComboEOSPunctuation = false;
   LWord comboEOSString;

   // If this is the next to last word and the last word is not excluded from translation.
   if (((beginSource + 2) == endSource) && !((*(beginSource + 1)).isExcludedFromTranslation()))
   {
      if ((*beginSource == "?") || (*beginSource == "!") || (*beginSource == "."))
      {
         if ((*(beginSource + 1) == ")") || (*(beginSource + 1) == "]") ||
             (*(beginSource+1) == "}"))
         {
            isComboEOSPunctuation = true;
            comboEOSString = *beginSource;
            comboEOSString += *(beginSource + 1);
         }
      }
   }

   // If the last sentence unit or is combination end of sentence punctuation or if next
   // to last sentence unit and the last one is not translatable.
   if (((beginSource + 1) == endSource) || isComboEOSPunctuation ||
       (((beginSource + 2) == endSource) && (*(beginSource + 1)).isExcludedFromTranslation()))
   {
      if (isComboEOSPunctuation)
         pEntry = builder().buildPunctuationEosEntry(comboEOSString);
      else
         pEntry = builder().buildPunctuationEosEntry(*beginSource);
      if (pEntry)
      {
         isEOSPunctuation = true;
      }
   }

   // If it isn't the end of the sentence then we still need to create a sentence unit.
   if (!isEOSPunctuation)
   {
      if (beginSource->isProtected())
      {
         pEntry = builder().buildProtectedEntry (*beginSource);
         pSentenceUnit->surfaceExpressionFromString(*beginSource);
         pSentenceUnit->setProtected(true);
      }
      else if (beginSource->isProtectedWord())
      {
         LgsString protectedString;
         pEntry = builder().buildProtectedWordEntry(beginSource, protectedString);
         pSentenceUnit->surfaceExpressionFromString(protectedString);
         pSentenceUnit->setProtectedWord(true);
      }
      else
      {
         pEntry = builder().buildEntry(beginSource, endSource, BOS, &sourceWords, bAllCapitalWords);
      }
   }
   else
   {
      if (isComboEOSPunctuation)
         pEntry->setOverflow3b(7);
      else
         pEntry->setOverflow3b(1);
      if (!(*beginSource).isExcludedFromTranslation() && !isComboEOSPunctuation)
      {
         pSentenceUnit->setEndOfSentence(true);

         // Some adjustment of the ssu's must be made for EOS units which are the "!" or "?".
         // Eliminate those SSU's that are not punctuation (EOS).
         if ((pEntry->dictionaryToken() == "!") || (pEntry->dictionaryToken() == "?"))
         {
            SsuList& ssuLst = pEntry->semantoSyntacticUnits();
            for (SsuList::iterator i = ssuLst.end() - 1; i >= ssuLst.begin(); i--)
            {
               if (i->wordClassCode() != LLanguage::PUNCTUATION)
                  ssuLst.erase(i);
            }
         }
         else if (pEntry->dictionaryToken() == ".")
         {
            pEntry->semantoSyntacticUnits().begin()->setSuperSetID(10);
            pEntry->semantoSyntacticUnits().begin()->setSetID(10);
            pEntry->semantoSyntacticUnits().begin()->setSubSetID(10);
         }
      }
   }

   pSentenceUnit->dictionaryEntry(pEntry);

   return pSentenceUnit;
}

//-------------------------------------------------------------------
SourceSentenceUnit* LDictionary::createBosUnit()
{
   // Calls the builder. The builder really constructs the
   // Dictionary Entry. This method allows the Linguistic object
   // (i.e., Documents, Sentences and Words) to be independent of
   // the building process.

   LDictionaryEntry* pEntry = builder().bosEntry();

   SourceSentenceUnit* pUnit = new SourceSentenceUnit(*this, pEntry);
   pUnit->position(1);
   pUnit->setSentenceAddress (SentenceUnit::BosIdentifier);
   pUnit->surfaceExpressionFromString("bos");
   pUnit->setTrailingSpaces(1);

   return pUnit;
}

//-------------------------------------------------------------------
SourceSentenceUnit* LDictionary::createTempBosUnit(const LgsString& s)
{
   // Calls the builder. The builder really constructs the
   // Dictionary Entry. This method allows the Linguistic object
   // (i.e., Documents, Sentences and Words) to be independent of
   // the building process.

   LDictionaryEntry* pEntry = builder().createTempBosEntry(s);

   SourceSentenceUnit* pUnit = new SourceSentenceUnit(*this, pEntry);
   pUnit->position (1);
   pUnit->setSentenceAddress(SentenceUnit::BosIdentifier);
   pUnit->surfaceExpressionFromString("bos");
   pUnit->setTrailingSpaces(1);

   return pUnit;
}

//-------------------------------------------------------------------
SourceSentenceUnit* LDictionary::createEmptyEosUnit()
{
   // Calls the builder. The builder really constructs the
   // Dictionary Entry. This method allows the Linguistic object
   // (i.e., Documents, Sentences and Words) to be independent of
   // the building process.

   LDictionaryEntry* pEntry = builder().eosEntry();

   SourceSentenceUnit* pUnit = new SourceSentenceUnit(*this, pEntry);
   pUnit->setEndOfSentence(true);

   return pUnit;
}

//-------------------------------------------------------------------
SourceSentenceUnit* LDictionary::createTempEosUnit(const LgsString& s)
{
   // Calls the builder. The builder really constructs the
   // Dictionary Entry. This method allows the Linguistic object
   // (i.e., Documents, Sentences and Words) to be independent of
   // the building process.

   LDictionaryEntry* pEntry = builder().createTempEosEntry(s);

   SourceSentenceUnit* pUnit = new SourceSentenceUnit(*this, pEntry);
   pUnit->setEndOfSentence(true);

   return pUnit;
}

//-------------------------------------------------------------------
bool LDictionary::isSentencePositionCorrect(int position)
{
   return targetBuilder().isSentencePositionCorrect(position);
}

//-------------------------------------------------------------------
void LDictionary::createTargetSentenceUnits(const SourceUnitVector& sourceUnits,
                                            TargetUnitVector& units)
{
   targetBuilder().buildUnits(sourceUnits, units, *this);

   for (TargetUnitIterator i = units.begin(); i != units.end(); ++i)
   {
      (*i)->setDictionary (*this);
   }
}

//-------------------------------------------------------------------
void LDictionary::createNounPhraseUnits(NounPhraseUnitVector* units)
{
   targetBuilder().buildNounPhraseUnits(units, *this);
}

//-------------------------------------------------------------------
void LDictionary::createProtectedTargetUnits(const SourceUnitVector& source,
                                             TargetUnitVector& units)
{
   targetBuilder().buildProtectedUnits (source, units, *this);

   for (TargetUnitIterator i = units.begin(); i != units.end(); ++i)
   {
      (*i)->setDictionary (*this);
   }
}

//---------------------------------------------------------------------
void LDictionary::createTranslatedEntry (TranslatedSentenceUnit& unit)
{
   if (!targetBuilder().buildTranslatedEntry(unit))
   {
	   LWord word;
      unit.dictionaryEntry(builder().unfoundEntry(EntryBuilder::Unfound,word));
   }
}

//---------------------------------------------------------------------
void LDictionary::createConstantEntry(ConstantSentenceUnit& unit)
{
   if (!targetBuilder().buildConstantEntry(unit))
   {
	   LWord word;
      unit.dictionaryEntry(builder().unfoundEntry(EntryBuilder::Unfound,word));
   }
}

//---------------------------------------------------------------------
void LDictionary::generateStem(FunctionConstantSentenceUnit* unit) const
{
   LgsString stem = unit->word();

   p_linguisticFactory->generateStem(&stem, *unit, false);
   unit->surfaceExpressionFromString(stem);
}

//---------------------------------------------------------------------
void LDictionary::generateStem(TargetDictionaryUnit* unit) const
{
   //-----------------------------------------------------------------
   // The generation of stems forks into 4 branches here. First the
   // single words work differently than the phrases. The single
   // words that have a combining form do not have to search the
   // database for a stem -- they just use the combining form. The
   // phrases have to generate a stem for each individual word. The
   // headword for the phrase just has the stem generated from the
   // information in the unit. The non-head word must be resolved
   // in terms of how they are interpreted as individual words via
   // the word_in_phrase table.
   //-----------------------------------------------------------------

   CompositeWord newSurfaceExpression;
   int combiningFormCode = unit->combiningFormCode();
   LgsString stem;
   
   if (combiningFormCode > 0)
   {
      stem = unit->surfaceExpressionAsString();
      unit->surfaceExpressionFromString((const_cast<LLanguage&> 
         (targetLanguage())).applyCombiningFormCode(combiningFormCode, stem));
   }
   else if (unit->wordCount() < 2)
   {
      stem = unit->surfaceExpressionAsString();
// Further study is needed before this fix can be implemented. Specifically, this change breaks
// possessive nouns and acronyms that are plurals.
//      bool allUpperCase = false;
//      if (StringUtil::isAllUpperCase(stem))
//      {
//         allUpperCase = true;
//      }
      p_linguisticFactory->generateStem(&stem, *unit, false);
//      if (allUpperCase)
//      {
//   	   if (unit->language().id() == LLanguage::GermanID)
//         {
//            StringUtil::toUpperGerman(stem);
//         }
//         else
//         {
//            StringUtil::toUpper(stem);
//         }
//      }
      unit->surfaceExpressionFromString(stem);
   }
   else
   {
      LWordVector& words = unit->surfaceWords();
      int location = 0;
      
      if (unit->language().id() == LLanguage::GermanID && unit->wordClassCode() == 2)
      {
         for (LWordIterator i = words.begin(); i != words.end(); i++)
         {
            if (i != words.begin()) stem += " ";
            stem += *i;
         }
         
         p_linguisticFactory->generateStem(&stem, *unit, false);
         unit->surfaceExpressionFromString(stem);
      }
      else
      {
         for (LWordIterator i = words.begin(); i != words.end(); i++)
         {
            if (++location == unit->headWord())
            {
               p_linguisticFactory->generateStem(&(*i), *unit, true);
            }
            else
            {
               p_linguisticFactory->generateStemPhrase(&(*i), location, *unit);
            }
            
            newSurfaceExpression += *i;
         }

         unit->setSurfaceExpression(newSurfaceExpression);
      }
   }
}

//---------------------------------------------------------------------
const Context& LDictionary::context() const
{
   return sourceLanguage().context();
}

//---------------------------------------------------------------------
void LDictionary::setSourceLanguage(LLanguage* p)
{
   if (p_sourceLanguage)
   {
      delete p_sourceLanguage;
   }
   p_sourceLanguage = p;
}

//---------------------------------------------------------------------
void LDictionary::setTargetLanguage(LLanguage* p)
{
   if (p_targetLanguage)
   {
      delete p_targetLanguage;
   }
   p_targetLanguage = p;
}

