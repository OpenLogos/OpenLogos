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
// File - GFactory.h (interface)
// Description - creates objects and vectors of objects from persistent storage.
// --------------------------------------------------------------------------

#ifndef __GFactory_h__
#define __GFactory_h__

#include <logos_libs/linguistic/linguisticfactory.h>
#include <logos_libs/linguistic/ldictionary.h>
#include <logos_libs/linguistic/ldictionarytoken.h>
#include <logos_libs/gerdem/dictionaryentrybuilder.h>
#include <logos_libs/sql/sqlconnection.h>
#include <logos_libs/gerdem/gphrasequery.h>
//#include <logos_libs/gerdem/PhraseIdQuery.h>
#include <logos_libs/gerdem/gwordquery.h>
#include <logos_libs/gerdem/gwordcapquery.h>
#include <logos_libs/gerdem/gssuquery.h>
#include <logos_libs/gerdem/targetquery.h>
#include <logos_libs/gerdem/irregularstemquery.h>
#include <logos_libs/gerdem/targetwordquery.h>
#include <logos_libs/gerdem/transfercountquery.h>
#include <logos_libs/gerdem/transferquery.h>
#include <logos_libs/gerdem/constantpointerquery.h>
#include <logos_libs/entity/dcompany.h>
#include <logos_libs/gerdem/stembuilder.h>
#include <logos_libs/gerdem/wordinphrasequery.h>
#include <logos_libs/linguistic/targetsentenceunit.h>
#include <logos_libs/linguistic/lsubjectmatter.h>
#include <logos_libs/sql/sqlconnection.h>

class GFactory: public LinguisticFactory
{
public:
   GFactory(SqlConnection*);
   virtual ~GFactory();
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
//   virtual DDerivedFormVector* CreateDerivedForms(int);
   virtual DDerivedFormMap* CreateDerivedFormsMap(int);
   virtual LInflectionVector* createInflections(int);
   virtual DWordPhraseVector* CreatePhraseMatches(const LgsString&, short firstSpaceCount,
												  const LgsString&);
   virtual DWordPhraseVector* CreatePhraseCapMatches(const LgsString&, short firstSpaceCount,
													 const LgsString&);
   virtual DWordPhraseVector* CreateWordMatches(const LgsString&);
   virtual DWordPhraseVector* CreateWordCapMatches(const LgsString&);
   virtual RE_Engine<EL_Variable>* createElisionEngine(int, const LgsString&);
   virtual bool validDictionaryToken(LDictionaryToken&, bool);
   virtual void completeEntryComponents(LDictionaryEntry*, LDictionaryTokenVector&);
   virtual void completePhraseComponents(LDictionaryEntry*, LDictionaryTokenVector&);
   virtual void CompleteSubjectMatter(const LgsString&, LSubjectMatterVector*);
   virtual void CompleteCompany(DCompany*);
   virtual void CompleteCompanyVector(DCompanyVector*);
   virtual void CompleteSubjectMatterVector(LSubjectMatterVector*);
   virtual void getAlternateTransfers(TranslatedSentenceUnit*);
   virtual void getHighAlternateTransfers(ConstantSentenceUnit*);
   virtual void getLowAlternateTransfers(ConstantSentenceUnit*);
   virtual LDictionaryEntry* createTargetEntry(TargetDictionaryUnit*);
   virtual void generateStem(LgsString*, const TargetSentenceUnit&, bool);
   virtual void generateStemPhrase(LgsString*, int, const TargetDictionaryUnit&);
   virtual bool IsSsuTransferred(const LSemantoSyntacticUnit&);
   WordInPhraseQuery& getWordInPhraseQuery();

private:
   SqlConnection* p_connection;
   GPhraseQuery v_phraseQuery;
//   PhraseIdQuery v_phraseIdQuery;
   GWordQuery v_wordQuery;
   GWordCapQuery v_wordCapQuery;
   GSsuQuery v_ssuQuery;
   TransferCountQuery v_transferCountQuery;
   WordInPhraseQuery v_wordInPhraseQuery;
   ConstantPointerQuery v_constantQuery;
   ConstantPointerQuery v_constantQueryNoCC;
   TransferQuery v_transferQuery;
   TargetQuery v_targetQuery;
   IrregularStemQuery v_irregularStemQuery;
   TargetWordQuery v_targetWordQuery;
   StemBuilder v_stemBuilder;
};

// --------------------------------------------------------------------------
inline WordInPhraseQuery& GFactory::getWordInPhraseQuery()
{
   return v_wordInPhraseQuery;
}

#endif // __GFactory_h__

