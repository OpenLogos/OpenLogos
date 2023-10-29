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
#ifndef __targetunitconcretebuilder_h__
#define __targetunitconcretebuilder_h__

//-------------------------------------------------------------------
// File - targetunitconcretebuilder.h
//
// Class - TargetUnitConcreteBuilder (Abstract)
//
// Description - This is a part of an application of the "Builder"
//      pattern.
//
// Patterns
//      Builder
//
//-------------------------------------------------------------------

#include <logos_libs/linguistic/targetunitbuilder.h>
#include <logos_libs/gerdem/constantpointerquery.h>
#include <logos_libs/linguistic/targetsentenceunit.h>
#include <logos_libs/linguistic/targetdictionaryunit.h>
#include <logos_libs/linguistic/linguisticfactory.h>
#include <logos_libs/gerdem/scontable.h>

class TargetUnitConcreteBuilder: public TargetUnitBuilder
{
public:
   //---------------------------------------------------------------
   // The destructor will perhaps do nothing for this class but it needs to be declared
   // virtual so that the delete of a pointer to this abstract class will cause the
   // subclass's destructor to be executed.
   //---------------------------------------------------------------
   TargetUnitConcreteBuilder();
   TargetUnitConcreteBuilder(LinguisticFactory*);
   virtual ~TargetUnitConcreteBuilder();

   //--------------------------------------------------------------
   // BuilderEntry - This is the primary member of class. It returns a new DictionaryEntry
   // that is based upon as many source words as possible. The beginSource iterator indicates
   // the first source word that can be matched. The endSource iterator indicates the
   // non-inclusive end of the source words. The matchEnd iterator is updated by the function
   // and represents the non-inclusive end of the source words used in building the
   // LDictionaryEntry object. Non-inclusive vector endings correspond to STL style.
   //-----------------------------------------------------------------
   virtual void buildUnits(const SourceUnitVector&,TargetUnitVector&, const LDictionary&);
   virtual void buildNounPhraseUnits(NounPhraseUnitVector*, const LDictionary&);
   virtual void buildProtectedUnits(const SourceUnitVector& sourceUnits, TargetUnitVector& units,
                                     const LDictionary& dictionary);
   virtual bool buildTranslatedEntry(TranslatedSentenceUnit&);
   virtual bool buildConstantEntry(ConstantSentenceUnit&);
   virtual bool isSentencePositionCorrect(int);
   virtual TargetSentenceUnit* buildBracketUnit(const SourceSentenceUnit* sourceUnit,
                                                const LDictionary& dictionary);

protected:
   TargetSentenceUnit* createUnit(int opadr, const SconTable& scon) const;
   TargetSentenceUnit* createHighConstantUnit(int opadr, const SconTable& scon) const;
   TargetSentenceUnit* createLowConstantUnit(int opadr, const SconTable& scon) const;
   TargetSentenceUnit* createUnfoundWordUnit(int opadr) const;
   TargetSentenceUnit* createPunctuationUnit(int opadr) const;
   TargetSentenceUnit* createBeginningOfSentenceUnit(int opadr) const;
   TargetSentenceUnit* createTranslatedUnit(int opadr) const;
   //TargetSentenceUnit* createProperNameSentenceUnit(int opadr) const;

private:
   LinguisticFactory* p_factory;
   int currentSentencePosition_;

   enum
   {
      ReadyToReadNextSentence = -1
   };
};

//-------------------------------------------------------------------

#endif // __targetunitconcretebuilder_h__

