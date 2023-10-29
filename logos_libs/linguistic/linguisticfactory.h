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
#ifndef __linguisticfactory_h__
#define __linguisticfactory_h__

//---------------------------------------------------------------------
// File - linguisticfactory.h
//
// Class - LinguisticFactory
//
// Description - creates objects and vectors of objects from persistent
//     storage.
//
// Patterns
//
//---------------------------------------------------------------------

#include <logos_libs/entity/dderivedform.h>
#include <logos_libs/linguistic/linflection.h>
#include <logos_libs/entity/dwordphrase.h>
#include <logos_libs/entity/dcompany.h>
#include <logos_libs/linguistic/ldictionarytoken.h>
#include <logos_libs/ruleengine/engine.h>
#include <logos_libs/elision/variable.h>
#include <logos_libs/linguistic/lword.h>
#include <logos_libs/linguistic/lsubjectmatter.h>

class LTextualComponent;
class LDictionary;
class LDictionaryEntry;
class LSemantoSyntacticUnit;
class LLanguage;
class TargetDictionaryUnit;
class TargetSentenceUnit;
class TranslatedSentenceUnit;
class ConstantSentenceUnit;

class LinguisticFactory
{
public:
   virtual ~LinguisticFactory();
   virtual void Open(const LDictionary*);

   //-----------------------------------------------------------------
   // Each of the following methods create a vector of objects from
   // some persistent source (the assumption is from a database).
   // NOTE: the preface "create" in front of each product indicates
   // that an object has been created on the heap and the customer of
   // the factory is responsible for managing its memory.
   //
   // CreateAtomic() - returns all of the DAtomic objects that
   //      persist in the database.
   // CreateCompanies() - returns all of the DCompany objects that
   //      persist in the database.
   // CreateInflections() - returns all of the DInflection objects
   //      that persist in the database.
   //-----------------------------------------------------------------
//   virtual DDerivedFormVector* CreateDerivedForms(int languageCode) = 0;
   virtual DDerivedFormMap* CreateDerivedFormsMap(int languageCode) = 0;
   virtual LInflectionVector* createInflections(int languageCode) = 0;

   virtual DWordPhraseVector* CreatePhraseMatches(const LgsString&, short firstSpaceCount, const LgsString&) = 0;
   virtual DWordPhraseVector* CreatePhraseCapMatches(const LgsString&, short firstSpaceCount, const LgsString&) = 0;
   virtual DWordPhraseVector* CreateWordMatches(const LgsString& ) = 0;
   virtual DWordPhraseVector* CreateWordCapMatches(const LgsString& ) = 0;

   virtual RE_Engine<EL_Variable>* createElisionEngine(int languageCode, const LgsString& elisionFileName) = 0;

   //-----------------------------------------------------------------
   // completeEntryComponents() - This method completes a dictionary
   //      entry by getting the Morphology and Meaning data for an
   //      already matched word. The Word-Phrase data for the matched
   //      word is found in the DictionaryToken. This information
   //      can be joined to the Morphology/Meaning complex.
   // completePhraseComponents() - This does the same as the method,
   //     completeEntryComponents(), except that it does it for
   //     phrase matches (more data is required here).
   //-----------------------------------------------------------------
   virtual bool validDictionaryToken(LDictionaryToken&, bool) = 0;
   virtual void completeEntryComponents(LDictionaryEntry*, LDictionaryTokenVector&) = 0;
   virtual void completePhraseComponents(LDictionaryEntry*, LDictionaryTokenVector&) = 0;

   virtual void CompleteSubjectMatter(const LgsString &, LSubjectMatterVector*) = 0;
   virtual void CompleteCompany(DCompany*) = 0;
   virtual void CompleteCompanyVector(DCompanyVector* pCompanyVector) = 0;
   virtual void CompleteSubjectMatterVector(LSubjectMatterVector*) = 0;

   virtual void getAlternateTransfers(TranslatedSentenceUnit*) = 0;
   virtual void getHighAlternateTransfers(ConstantSentenceUnit*) = 0;
   virtual void getLowAlternateTransfers(ConstantSentenceUnit*) = 0;

   virtual LDictionaryEntry* createTargetEntry(TargetDictionaryUnit*) = 0;

   virtual void generateStem(LgsString* stem, const TargetSentenceUnit&, bool) = 0;
   virtual void generateStemPhrase(LgsString* stem, int location, const TargetDictionaryUnit&) = 0;

   virtual bool IsSsuTransferred(const LSemantoSyntacticUnit&) = 0;

   const LDictionary& Dictionary() const;

protected:
   //-----------------------------------------------------------------
   // Since this is an abstract class, making the default constructor
   // protected is illustrative for the users of this class.
   //-----------------------------------------------------------------
   LinguisticFactory();

private:
   const LDictionary* p_dictionary;
};

//-------------------------------------------------------------------
inline const LDictionary& LinguisticFactory::Dictionary() const
{
   return *p_dictionary;
}

#endif // __linguisticfactory_h__

