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
// File - punctuationprototypebuilder.cpp
//
// Class - PunctuationPrototypeBuilder
//
//-------------------------------------------------------------------

#include <logos_include/logoscommon.h>
#include <logos_libs/gerdem/punctuationprototypebuilder.h>
#include <logos_libs/gerdem/patternprototypemap.h>
#include <logos_libs/linguistic/ldictionaryentry.h>

//-------------------------------------------------------------------
PunctuationPrototypeBuilder::PunctuationPrototypeBuilder()
{
}
//-------------------------------------------------------------------
PunctuationPrototypeBuilder::~PunctuationPrototypeBuilder()
{
}
//-------------------------------------------------------------------
void PunctuationPrototypeBuilder::Build(PatternPrototypeMap& aMap) const
{
   aMap.InsertEntry(".", createPeriodEntry());
   aMap.InsertEntry("?", createQuestionEntry());
   aMap.InsertEntry("!", createExclamationEntry());
//   aMap.InsertEntry(",", createCommaEntry());
   aMap.InsertEntry(":", createColonEntry());
   aMap.InsertEntry("?)", createQuestionParenEntry());
   aMap.InsertEntry("?]", createQuestionSquareEntry());
   aMap.InsertEntry("?}", createQuestionCurlyEntry());
   aMap.InsertEntry("!)", createExclamationParenEntry());
   aMap.InsertEntry("!]", createExclamationSquareEntry());
   aMap.InsertEntry("!}", createExclamationCurlyEntry());
   aMap.InsertEntry(".)", createPeriodParenEntry());
   aMap.InsertEntry(".]", createPeriodSquareEntry());
   aMap.InsertEntry(".}", createPeriodCurlyEntry());
}
//-----------------------------------------------------------------------
LDictionaryEntry* PunctuationPrototypeBuilder::createPeriodEntry() const
{
   LDictionaryEntry* pEntry = createDefaultEntry();

   LSemantoSyntacticUnit ssu;
   setDefaultWordPhrase(&ssu);
   setDefaultMorphology(&ssu);

   ssu.setWord(".");
   ssu.setTargetWord(".");

   ssu.setFormCode(1);
   ssu.setSuperSetID(10);
   ssu.setSetID(10);
   ssu.setSubSetID(10);

   pEntry->addSsu(ssu);

   return pEntry;
}
//-----------------------------------------------------------------------
LDictionaryEntry* PunctuationPrototypeBuilder::createColonEntry() const
{
   LDictionaryEntry* pEntry = createDefaultEntry();

   LSemantoSyntacticUnit ssu;
   setDefaultWordPhrase(&ssu);
   setDefaultMorphology(&ssu);

   ssu.setWord(":");
   ssu.setTargetWord(":");

   ssu.setFormCode(1);
   ssu.setSuperSetID(10);
   ssu.setSetID(10);
   ssu.setSubSetID(887);

   pEntry->addSsu(ssu);

   return pEntry;
}
//-----------------------------------------------------------------------
//LDictionaryEntry* PunctuationPrototypeBuilder::createCommaEntry() const
//{
//   LDictionaryEntry* pEntry = createDefaultEntry();
//
//   LSemantoSyntacticUnit ssu;
//   setDefaultWordPhrase(&ssu);
//   setDefaultMorphology(&ssu);
//
//   ssu.setWord(",");
//   ssu.setTargetWord(",");
//
//   ssu.setFormCode(2);
//   ssu.setSuperSetID(2);
//   ssu.setSetID(2);
//   ssu.setSubSetID(888);
//
//   pEntry->addSsu(ssu);
//
//   return pEntry;
//}
//-----------------------------------------------------------------------
LDictionaryEntry* PunctuationPrototypeBuilder::createQuestionEntry() const
{
   LDictionaryEntry* pEntry = createDefaultEntry();

   LSemantoSyntacticUnit ssu;
   setDefaultWordPhrase(&ssu);
   setDefaultMorphology(&ssu);

   ssu.setWord("?");
   ssu.setTargetWord("?");

   ssu.setFormCode(9 );
   ssu.setSuperSetID(10);
   ssu.setSetID(10);
   ssu.setSubSetID(10);

   pEntry->addSsu(ssu);

   return pEntry;
}
//-----------------------------------------------------------------------
LDictionaryEntry* PunctuationPrototypeBuilder::createExclamationEntry() const
{
   LDictionaryEntry* pEntry = createDefaultEntry();

   LSemantoSyntacticUnit ssu;
   setDefaultWordPhrase(&ssu);
   setDefaultMorphology(&ssu);

   ssu.setWord("!");
   ssu.setTargetWord("!");

   ssu.setFormCode(8 );
   ssu.setSuperSetID(10);
   ssu.setSetID(10);
   ssu.setSubSetID(10);

   pEntry->addSsu(ssu);

   return pEntry;
}
//-----------------------------------------------------------------------
void PunctuationPrototypeBuilder::setLeftMeaning(LSemantoSyntacticUnit* p) const
{
   p->setFormCode(01);
   p->setSuperSetID(6);
   p->setSetID(26);
   p->setSubSetID(866);
}
//-----------------------------------------------------------------------
void PunctuationPrototypeBuilder::setRightMeaning(LSemantoSyntacticUnit* p) const
{
   p->setFormCode(1);
   p->setSuperSetID(7);
   p->setSetID(27);
   p->setSubSetID(877);
}
//-----------------------------------------------------------------------
inline LDictionaryEntry* PunctuationPrototypeBuilder::createCombinedEntry(const char * wordVal) const
{
   LDictionaryEntry* pEntry = createDefaultEntry();

   LSemantoSyntacticUnit ssu;
   setDefaultWordPhrase(&ssu);
   setDefaultMorphology(&ssu);
   setRightMeaning(&ssu);
   ssu.setOverflow3b(7);

   ssu.setWord(wordVal);
   ssu.setTargetWord(wordVal);
   pEntry->wordsUsedInMatch(2);

   pEntry->addSsu(ssu);

   return pEntry;
}
//-----------------------------------------------------------------------
LDictionaryEntry* PunctuationPrototypeBuilder::createPeriodParenEntry(void) const
{
   return createCombinedEntry(".)");
}
//-----------------------------------------------------------------------
LDictionaryEntry* PunctuationPrototypeBuilder::createPeriodSquareEntry(void) const
{
   return createCombinedEntry(".]");
}
//-----------------------------------------------------------------------
LDictionaryEntry* PunctuationPrototypeBuilder::createPeriodCurlyEntry(void) const
{
   return createCombinedEntry(".}");
}
//-----------------------------------------------------------------------
LDictionaryEntry* PunctuationPrototypeBuilder::createQuestionParenEntry(void) const
{
   return createCombinedEntry("?)");
}
//-----------------------------------------------------------------------
LDictionaryEntry* PunctuationPrototypeBuilder::createQuestionSquareEntry(void) const
{
   return createCombinedEntry("?]");
}
//-----------------------------------------------------------------------
LDictionaryEntry* PunctuationPrototypeBuilder::createQuestionCurlyEntry(void) const
{
   return createCombinedEntry("?}");
}
//-----------------------------------------------------------------------
LDictionaryEntry* PunctuationPrototypeBuilder::createExclamationParenEntry(void) const
{
   return createCombinedEntry("!)");
}
//-----------------------------------------------------------------------
LDictionaryEntry* PunctuationPrototypeBuilder::createExclamationSquareEntry(void) const
{
   return createCombinedEntry("!]");
}
//-----------------------------------------------------------------------
LDictionaryEntry* PunctuationPrototypeBuilder::createExclamationCurlyEntry(void) const
{
   return createCombinedEntry("!}");
}
//-----------------------------------------------------------------------
LDictionaryEntry* PunctuationPrototypeBuilder::createDefaultEntry() const
{
   LDictionaryEntry* pEntry = new LDictionaryEntry;

   pEntry->setCached(true);

   return pEntry;
}
//-----------------------------------------------------------------------
void PunctuationPrototypeBuilder::setDefaultWordPhrase(LSemantoSyntacticUnit* p) const
{
   p->setCompanyCode("LOG");
   p->setWordCount(0);
   p->setWordID(-10);
}
//-----------------------------------------------------------------------
void PunctuationPrototypeBuilder::setDefaultMorphology(LSemantoSyntacticUnit* p) const
{
   p->setPatNumber(0);
   p->setSourceStemNumber(0);
   p->setWordClassCode(LLanguage::PUNCTUATION);
}
