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
// File - targetunitconcretebuilder.cpp
//
// Class - TargetUnitConcreteBuilder
//
//-------------------------------------------------------------------

#include <logos_include/logoscommon.h>
#include <logos_libs/gerdem/targetunitconcretebuilder.h>
#include <logos_libs/linguistic/translatedsentenceunit.h>
#include <logos_libs/linguistic/nounphraseunit.h>
#include <logos_libs/linguistic/lsentence.h>
#include <logos_libs/linguistic/sourcesentenceunit.h>
#include <logos_libs/linguistic/constantsentenceunit.h>
#include <logos_libs/linguistic/insertedsentenceunit.h>
#include <logos_libs/linguistic/functionconstantsentenceunit.h>
#include <logos_libs/linguistic/punctuationconstantsentenceunit.h>
#include <logos_libs/linguistic/punctuationsentenceunit.h>
#include <logos_libs/linguistic/bossentenceunit.h>
#include <logos_libs/linguistic/unfoundsentenceunit.h>
#include <logos_libs/linguistic/linguisticfactory.h>
#include <logos_libs/linguistic/untranslatedsentenceunit.h>
#include <logos_libs/gerdem/scontable.h>

#ifdef TRANSLAPP
#include <logos_libs/multithreadlib/comminterface.h>

extern LgsMessage* tranMsg;
#endif


//-------------------------------------------------------------------
TargetUnitConcreteBuilder::TargetUnitConcreteBuilder()
                          :p_factory(0),
                           currentSentencePosition_(ReadyToReadNextSentence)
{
   // For comments read the non-default constructor
}
//-------------------------------------------------------------------
TargetUnitConcreteBuilder::TargetUnitConcreteBuilder(LinguisticFactory* pFactory)
                          :p_factory(pFactory),
                           currentSentencePosition_(ReadyToReadNextSentence)
{
   // The stream is the source of the scon data from Tran-4.
   // The factory contains the interface to the persistent
   // dictionary.
}
//-------------------------------------------------------------------
TargetUnitConcreteBuilder::~TargetUnitConcreteBuilder()
{
}
//-------------------------------------------------------------------
bool TargetUnitConcreteBuilder::buildTranslatedEntry(TranslatedSentenceUnit& unit)
{
   // At this point the scon information has already been acquired
   // the scon information and needs to build a sentence unit's
   // dictionary entry. Because this is a unit that is completely
   // translated from the source side, selecting which transfer is
   // part of the job.

   LDictionaryEntry *pEntry = 0;
   p_factory->getAlternateTransfers (&unit);
   pEntry = p_factory->createTargetEntry(&unit);
   unit.dictionaryEntry(pEntry);

   return pEntry ? true : false;
}
//-------------------------------------------------------------------
bool TargetUnitConcreteBuilder::buildConstantEntry(ConstantSentenceUnit& unit)
{
   // At this point the scon information has already been acquired
   // the scon information and needs to build a sentence unit's
   // dictionary entry. Because this is a constant sentence unit,
   // alternate transfer do NOT have to be selected from and it is
   // just a matter of persisting in the dictionary information.

   LDictionaryEntry *pEntry = 0;

   if (unit.isHigh())
   {
      p_factory->getHighAlternateTransfers (&unit);
   }
   else
   {
      p_factory->getLowAlternateTransfers (&unit);
   }
   pEntry = p_factory->createTargetEntry (&unit);
   unit.dictionaryEntry(pEntry);

   return pEntry ? true : false;
}
//-------------------------------------------------------------------
bool TargetUnitConcreteBuilder::isSentencePositionCorrect(int position)
{
#ifdef TRANSLAPP
   // This is an important part of the protocol between tran4 and Generate.
	char * dataIn = tranMsg->dataPtr();
   if (currentSentencePosition_ == ReadyToReadNextSentence)
   {
      memcpy((char *)&currentSentencePosition_, dataIn, sizeof(currentSentencePosition_));
   }
   if (currentSentencePosition_ == position)
   {
      currentSentencePosition_ = ReadyToReadNextSentence;
      return true;
   }
#endif
   return false;
}
//-------------------------------------------------------------------
void TargetUnitConcreteBuilder::buildNounPhraseUnits(NounPhraseUnitVector* units,
                                                     const LDictionary& dictionary)
{
   short numOfElements = 0;

   for (int i = 0; i < numOfElements; i++)
   {
   }
}
//-------------------------------------------------------------------
void TargetUnitConcreteBuilder::buildUnits(const SourceUnitVector& sourceUnits,
                                           TargetUnitVector& units, const LDictionary& dictionary)
{
#ifdef TRANSLAPP
   bool wasBounded = false;
   bool placeBracketAtEnd = false;
   int adj = 0;
   TargetSentenceUnit* bracketBeginUnit = 0;
   TargetSentenceUnit* bracketEndUnit = 0;
   int maxSentenceAddress = 0;

   // Check to see if the sentence was bracket.
   if (sourceUnits.size() >= 2)
   {
      if ((*(*(sourceUnits.begin() + 1))->surfaceWords().begin()).isExcludedFromTranslation())
      {
         wasBounded = true;
         adj = 1;
         bracketBeginUnit = buildBracketUnit(*(sourceUnits.begin() + 1), dictionary);
         if ((*(*(sourceUnits.end() - 2))->surfaceWords().begin()).isExcludedFromTranslation())
         {
            bracketEndUnit = buildBracketUnit(*(sourceUnits.end() - 2), dictionary);
            maxSentenceAddress = (*(sourceUnits.end() - 2))->sentenceAddress();
         }
         else
         {
            bracketEndUnit = buildBracketUnit(*(sourceUnits.end() - 1), dictionary);
            maxSentenceAddress = (*(sourceUnits.end() - 1))->sentenceAddress();
            placeBracketAtEnd = true;
         }
      }
   }
	
   char * dataIn = tranMsg->dataPtr();

	// move pointer to start of Target Mapper contents
	dataIn += sizeof(int);			// skip sentence id info
	dataIn += sizeof(short int);		// skip offset number (used for term search)

	// retrieve target info from message
   short numOfElements = 0;
   memcpy((char *)&numOfElements, dataIn, sizeof(numOfElements));
   dataIn += sizeof(numOfElements);

   bool dummyBOSInserted = false;
   short nextOpadr = 0;

   for (int i = 0; i < numOfElements + adj; i++)
   {
      short opadr = nextOpadr;
      short sconPointer = 0;
      short primarySsu = 0;
      char compCode[4];
      compCode[3] = '\0';
      SconTable sconTable;

      // If the BOS was dropped and a dummy BOS was inserted, then there is a value for the
      // opadr read in as the first sentence unit. Otherwise read the next one from the stream.
      if (!opadr)
      {
         memcpy((char *) &opadr, dataIn, sizeof(opadr));
         dataIn += sizeof(opadr);
      }
      else
      {
         nextOpadr = 0;
      }

      if (!opadr)
      {
         throw (LgsString ("Illegal Output Address!"));
      }

      // If this is the first target unit to be read in and is not a BOS (BOS was dropped in 
      // TRAN 4) then create a dummy BOS unit. Otherwise read information from stream.
      if ((i == 0) && (opadr != SentenceUnit::BosIdentifier))
      {
         nextOpadr = opadr;
         opadr = SentenceUnit::BosIdentifier;
         sconPointer = 1;
         primarySsu = 0;
         sconTable.setAsDummyBOS();
         adj++;
      }
      else
      {
         memcpy((char *) &sconPointer, dataIn, sizeof(sconPointer));
         dataIn += sizeof(sconPointer);
         memcpy((char *) &primarySsu, dataIn, sizeof(primarySsu));
         dataIn += sizeof(primarySsu);
         memcpy(compCode, dataIn, (3 * sizeof(char)));
         dataIn += (3 * sizeof(char));

         if (10 < primarySsu)
         {
            primarySsu -= 10;
         }
         primarySsu = (primarySsu) ? primarySsu - 1 : primarySsu;
         streamInSconTable(dataIn, sconTable);
      }

      if (FunctionConstantSentenceUnit::NullAddress != opadr)
      {
         if ((i != 1) || (opadr != PunctuationConstantSentenceUnit::GmComma))
         {
            TargetSentenceUnit* p = createUnit(opadr, sconTable);
            p->setDictionary(const_cast<LDictionary&>(dictionary));
            p->setOpadr(opadr);
            p->setSconTable(sconTable);
            p->setSourcePrimarySsuPosition(primarySsu);
            p->setSentenceAddress(i + 1);
            p->setCompanyCode(compCode);
			   p->sconPointer(sconPointer);

            // If the sentence was bounded by brackets, then target sentence units must be
            // inserted for the brackets that were not sent to translation.
            if (wasBounded)
            {
               // If it's the first unit
               if (i == 0)
               {
                  units.push_back(p);
                  i++;
                  bracketBeginUnit->setSentenceAddress(i + 1);
                  units.push_back(bracketBeginUnit);
               }
               else
               {
                  if (i == numOfElements)
                  {
                     if (placeBracketAtEnd)
                     {
                        if (p->sourceUnitPosition() > 0)
                        {
                           p->setSourceUnitPosition(p->sourceUnitPosition() + 1);
                           if ((p->opadr() > 0) && (p->opadr() <= maxSentenceAddress))
                           {
                              p->setOpadr(p->opadr() + 1);
                           }
                        }
                        units.push_back(p);
                        i++;
                        bracketEndUnit->setSentenceAddress(i + 1);
                        units.push_back(bracketEndUnit);
                     }
                     else
                     {
                        bracketEndUnit->setSentenceAddress(i + 1);
                        units.push_back(bracketEndUnit);
                        i++;
                        p->setSentenceAddress(i + 1);
                        if (p->sourceUnitPosition() > 0)
                        {
                           p->setSourceUnitPosition(p->sourceUnitPosition() + 2);
                           if ((p->opadr() > 0) && (p->opadr() <= maxSentenceAddress))
                           {
                              p->setOpadr(p->opadr() + 2);
                           }
                        }
                        units.push_back(p);
                     }
                  }
                  else
                  {
                     if (p->sourceUnitPosition() > 0)
                     {
                        p->setSourceUnitPosition(p->sourceUnitPosition() + 1);
                        if ((p->opadr() > 0) && (p->opadr() <= maxSentenceAddress))
                        {
                           p->setOpadr(p->opadr() + 1);
                        }
                     }
                     units.push_back(p);
                  }
               }
            }
            else
            {
               units.push_back(p);
            }
         }
      }
   }
#endif
}
//-------------------------------------------------------------------
void TargetUnitConcreteBuilder::buildProtectedUnits(const SourceUnitVector& sourceUnits,
                                                    TargetUnitVector& units,
                                                    const LDictionary& dictionary)
{
   for (SourceUnitVector::const_iterator i = sourceUnits.begin(); i != sourceUnits.end(); ++i)
   {
      TargetSentenceUnit* p = 0;
      LLanguage::WordClassCode wordClassCode;

      if (SentenceUnit::BosIdentifier == (*i)->sentenceAddress())
      {
         p = createBeginningOfSentenceUnit(SentenceUnit::BosIdentifier);
         wordClassCode = LLanguage::PUNCTUATION;
      }
      else if (LLanguage::PUNCTUATION != (*i)->wordClassCode())
      {
         p = createUnfoundWordUnit((*i)->sentenceAddress());
         wordClassCode = LLanguage::NOUN;
      }
      if (p)
      {
         p->setDictionary(const_cast<LDictionary&>(dictionary));
         p->setOpadr((*i)->sentenceAddress());
         p->zeroSconTable();
         p->setWordClassCode(wordClassCode);
         p->setSentenceAddress((*i)->sentenceAddress());
         units.push_back(p);
      }
   }
}
//-------------------------------------------------------------------
TargetSentenceUnit* TargetUnitConcreteBuilder::buildBracketUnit(const SourceSentenceUnit* sourceUnit, const LDictionary& dictionary)
{
   TargetSentenceUnit* p = createUnfoundWordUnit(sourceUnit->sentenceAddress());
   if (p)
   {
      p->setDictionary(const_cast<LDictionary&>(dictionary));
      p->setOpadr(sourceUnit->sentenceAddress());
      p->setSourcePrimarySsuPosition(0);
      p->zeroSconTable();
      p->setDeclension(1);
      p->setSourceUnitPosition(sourceUnit->sentenceAddress());
      p->setWordClassCode(LLanguage::PUNCTUATION);
   }
   return p;
}
//-------------------------------------------------------------------
TargetSentenceUnit* TargetUnitConcreteBuilder::createUnit(int opadr, const SconTable& scon) const
{
   TargetSentenceUnit* pUnit;

   if (FunctionConstantSentenceUnit::isFunctionalConstantAddress(opadr))
   {
      pUnit = new FunctionConstantSentenceUnit(opadr);
   }
   else if (PunctuationConstantSentenceUnit::isPunctuationConstantAddress(opadr))
   {
      pUnit = new PunctuationConstantSentenceUnit(opadr);
   }
   else if (opadr == UntranslatedSentenceUnit::FunctionID)
   {
      pUnit = new (UntranslatedSentenceUnit);
   }
   else if (opadr < SentenceUnit::ProperNameIdentifier)
   {
      pUnit = createHighConstantUnit(opadr, scon);
   }
   /*
   else if (opadr == SentenceUnit::ProperNameIdentifier)
   {
      pUnit = createProperNameSentenceUnit(opadr);
   }
   */
   else if (opadr < SentenceUnit::LowestUnfoundWordIdentifier)
   {
      pUnit = createHighConstantUnit(opadr, scon);
   }
   else if (opadr <= SentenceUnit::HighestUnfoundWordIdentifier)
   {
      pUnit = createUnfoundWordUnit(opadr);
   }
   else if (opadr < SentenceUnit::BosIdentifier)
   {
      pUnit = createPunctuationUnit(opadr);
   }
   else if (opadr == SentenceUnit::BosIdentifier)
   {
      pUnit = createBeginningOfSentenceUnit(opadr);
   }
   else if (opadr > SentenceUnit::HighestTransferIdentifier)
   {
      pUnit = createLowConstantUnit(opadr, scon);
   }
   else
   {
      pUnit = createTranslatedUnit(opadr);
   }
   return pUnit;
}
//-------------------------------------------------------------------
TargetSentenceUnit* TargetUnitConcreteBuilder::createHighConstantUnit(int opadr,
                                                                      const SconTable& scon) const
{
   ConstantSentenceUnit* pUnit = 0;

   if ((LSentence::minimumSourceAddress <= scon.sourceSentenceUnitPosition()) &&
       (LSentence::maximumSourceAddress >= scon.sourceSentenceUnitPosition()))
   {
      pUnit = new ConstantSentenceUnit();
   }
   else
   {
      pUnit = new InsertedSentenceUnit();
   }
   pUnit->setHigh(true);
   return pUnit;
}
//-------------------------------------------------------------------
TargetSentenceUnit* TargetUnitConcreteBuilder::createLowConstantUnit(int opadr,
                                                                     const SconTable& scon) const
{
   ConstantSentenceUnit* pUnit = 0;

   if ((LSentence::minimumSourceAddress <= scon.sourceSentenceUnitPosition()) &&
       (LSentence::maximumSourceAddress >= scon.sourceSentenceUnitPosition()))
   {
      pUnit = new ConstantSentenceUnit();
   }
   else
   {
      pUnit = new InsertedSentenceUnit();
   }
   return pUnit;
}
//-------------------------------------------------------------------
/*
TargetSentenceUnit* TargetUnitConcreteBuilder::createProperNameSentenceUnit(int opadr) const
{
   ProperNameSentenceUnit* pUnit = new ProperNameSentenceUnit();
   pUnit->setSourceUnitPosition(opadr);
   return pUnit;
}
*/
//-------------------------------------------------------------------
TargetSentenceUnit* TargetUnitConcreteBuilder::createUnfoundWordUnit(int opadr) const
{
   UnfoundSentenceUnit* pUnit = new UnfoundSentenceUnit();
   pUnit->setSourceUnitPosition(opadr);
   return pUnit;
}
//-------------------------------------------------------------------
TargetSentenceUnit* TargetUnitConcreteBuilder::createPunctuationUnit(int opadr) const
{
   PunctuationSentenceUnit* pUnit = new PunctuationSentenceUnit();
   pUnit->setSourceUnitPosition(opadr);
   return pUnit;
}
//-------------------------------------------------------------------
TargetSentenceUnit* TargetUnitConcreteBuilder::createBeginningOfSentenceUnit(int opadr) const
{
   BosSentenceUnit* pUnit = new BosSentenceUnit();
   return pUnit;
}
//-------------------------------------------------------------------
TargetSentenceUnit* TargetUnitConcreteBuilder::createTranslatedUnit(int opadr) const
{
   TranslatedSentenceUnit* pUnit = new TranslatedSentenceUnit();
   pUnit->setSourceUnitPosition(opadr);
   return pUnit;
}
