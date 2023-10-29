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
#ifndef __LDictionary_h__
#define __LDictionary_h__

//-------------------------------------------------------------------
// File - LDictionary.h
//
// Class - LDictionary (interface)
//
// Description - The dictionary was primarily intended to be the
//      container of the source and target languages. As much as
//      possible, the languages are the strategies for all the
//      algorithms and data that are language dependent.
//
//      However, the largest part of this class is used to produce
//      interpretive information for words that are passed it. In
//      the DictionaryEntry and SSU classes, the information that
//      interprets words is found. The dictionary object is a
//      database independent access to dictionary information.
//
//-------------------------------------------------------------------

#include <logos_libs/linguistic/lword.h>
#include <logos_libs/linguistic/entrybuilder.h>

class ConstantSentenceUnit;
class LDictionaryEntry;
class LinguisticFactory;
class LLanguage;
class SourceSentenceUnit;
class SourceUnitVector;
class TargetDictionaryUnit;
class TargetUnitBuilder;
class TargetUnitVector;
class TranslatedSentenceUnit;
class FunctionConstantSentenceUnit;
class PunctuationConstantSentenceUnit;
class NounPhraseUnitVector;
class Context;

class LDictionary
{
public:
   //---------------------------------------------------------------
   // There is no "default constructor". To be useful, a dictionary
   // needs a source and target language set and a builder and a factory.
   //---------------------------------------------------------------
   LDictionary (LLanguage* aSourceLanguage, LLanguage* aTargetLanguage,
                EntryBuilder* aBuilder, LinguisticFactory* aFactory);
   virtual ~LDictionary();

   //---------------------------------------------------------------
   // sourceLanguage() - The language object that describes the
   //      source of the translation.
   // targetLanguage() - The language object that describes the
   //      target of the translation.
   // context() - A context is an object that combines subject
   //      matter information, company information and their
   //      algorithms.
   // setSourceLanguage() -
   // setTargetLanguage() -
   // setTargetBuilder() -
   //---------------------------------------------------------------
   const LLanguage& sourceLanguage() const;
   LLanguage& sourceLanguage();
   const LLanguage& targetLanguage() const;
   const Context& context() const;

   void setSourceLanguage(LLanguage*);
   void setTargetLanguage(LLanguage*);
   void setTargetBuilder(TargetUnitBuilder*);

   //---------------------------------------------------------------
   // createSentenceUnit() - Creates source sentence unit
   // createBosUnit() - Creates source sentence unit for BOS unit.
   // createProtectedUnit() - Creates source sentence unit
   // createEmptyEosUnit() - Creates source sentence unit
   // createTempEosUnit() - Creates source sentence unit
   // createTempBosUnit() - Creates source sentence unit
   //---------------------------------------------------------------
   SourceSentenceUnit* createSentenceUnit(const LWordIterator& from, const LWordIterator& to,
                                          bool BOS, LWordVector & sourceWords,
                                          bool bAllCapitalWords);
   SourceSentenceUnit* createBosUnit();
   SourceSentenceUnit* createProtectedUnit(const LWordIterator& word);
   SourceSentenceUnit* createEmptyEosUnit();
   SourceSentenceUnit* createTempEosUnit(const LgsString&);
   SourceSentenceUnit* createTempBosUnit(const LgsString&);

   //---------------------------------------------------------------
   // createProtectedTargetUnits() - Called by the sentence to make
   //      protected units for an all-protected sentence.
   // createTargetSentenceUnits() -
   // createTranslatedEntry() -
   // createConstantEntry() -
   // createNounPhraseUnits() -
   //---------------------------------------------------------------
   void createProtectedTargetUnits(const SourceUnitVector&, TargetUnitVector&);
   void createTargetSentenceUnits(const SourceUnitVector&, TargetUnitVector&);
   void createTranslatedEntry(TranslatedSentenceUnit&);
   void createConstantEntry(ConstantSentenceUnit&);
   void createNounPhraseUnits(NounPhraseUnitVector*);

   //---------------------------------------------------------------
   // generateStem() - Unfortunately, the stem generation process
   //     is heavily intertwined with the relational database. The
   //     only way to isolate the linguistic library from the
   //     database dependent stuff was to treat stem generation as
   //     a function call. There is a instance of this method for
   //     both dictionary units and functional constants.
   //---------------------------------------------------------------
   void generateStem(FunctionConstantSentenceUnit*) const;
   void generateStem(TargetDictionaryUnit*) const;

   //---------------------------------------------------------------
   // isSentencePositionCorrect() - This method determines if the
   //      sentence coming in from Tran-4 is in sync with the
   //      sentence coming from Lookup. The sentences can get out of
   //      sync because, at any step along the translation path,
   //      a sentence can fail and not be passed on to the next
   //      step. If the sentences are out of sync, it can be assumed
   //      that Lookup (the earlier module) has sentences that
   //      must be passed through untranslated until it has caught
   //      up with the Tran-4 (later) module.
   //---------------------------------------------------------------
   bool isSentencePositionCorrect(int);
	void initializeWordGroupManager(LWordVector * srcWords);
   void updateWordPosition(int wordCount, LWordIterator startSeq);

protected:
   //---------------------------------------------------------------
   // builder() - accessor for the source unit builder.
   // linguisticFactory() - accessor for the factory.
   // targetBuilder() - accessor for the target unit builder.
   //---------------------------------------------------------------
   EntryBuilder& builder();
   LinguisticFactory& linguisticFactory();
   TargetUnitBuilder& targetBuilder();

private:
    //---------------------------------------------------------------
    // Member variables are only the languages and pointers to the
    // creational patterns.
    //---------------------------------------------------------------
    LLanguage* p_sourceLanguage;
    LLanguage* p_targetLanguage;
    EntryBuilder* p_builder;
    LinguisticFactory* p_linguisticFactory;
    TargetUnitBuilder* p_targetBuilder;
};

//---------------------------------------------------------------------
inline const LLanguage& LDictionary::sourceLanguage() const
{
   assert(p_sourceLanguage);
   return *p_sourceLanguage;
}
//---------------------------------------------------------------------
inline LLanguage& LDictionary::sourceLanguage()
{
   assert(p_sourceLanguage);
   return *p_sourceLanguage;
}
//---------------------------------------------------------------------
inline const LLanguage& LDictionary::targetLanguage() const
{
   assert(p_targetLanguage);
   return *p_targetLanguage;
}
//---------------------------------------------------------------------
inline EntryBuilder& LDictionary::builder()
{
   assert(p_builder);
   return *p_builder;
}
//---------------------------------------------------------------------
inline LinguisticFactory& LDictionary::linguisticFactory()
{
   assert(p_linguisticFactory);
   return *p_linguisticFactory;
}
//---------------------------------------------------------------------
inline TargetUnitBuilder& LDictionary::targetBuilder()
{
   assert(p_targetBuilder);
   return *p_targetBuilder;
}
//---------------------------------------------------------------------
inline void LDictionary::setTargetBuilder(TargetUnitBuilder* p)
{
   p_targetBuilder = p;
}
//---------------------------------------------------------------------
inline void LDictionary::initializeWordGroupManager(LWordVector * srcWords)
{
	p_builder->initializeWordGroupManager(srcWords);
}
//---------------------------------------------------------------------
inline void LDictionary::updateWordPosition(int wordCount, LWordIterator startSeq)
{
   p_builder->updateWordPosition(wordCount, startSeq);
}
#endif // __LDictionary_h__

